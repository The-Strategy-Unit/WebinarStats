#' -----------------------------------------------------------------------------
#' FUNCTIONS USED IN SCRIPT
#' 
#' Here are functions used throughout the script
#' -----------------------------------------------------------------------------

# libraries --------------------------------------------------------------------
library(tidyverse)
library(clock)

#' Get Joined Data
#' 
#' This function returns a list of attendees who joined or left an MS Teams
#' meeting within a specified window of time.
#' 
#' @description Returns a list of attendees within the given start and end times
#' @param data Dataframe of an MS Teams meeting attendance report 
#' @param eventdate Date the event took place
#' @param starttime Time the event started
#' @param endtime Time the event ended
#'
#' @return Dataframe of joiner / leaver events within the specified window
#' @export
#' @examples
get_joined_data <- function(data, eventdate, starttime, endtime){
  
  # parse timestamp in data to date and datetime and 
  data <- data |>
    dplyr::mutate(
      datestamp = mdy(str_sub(utc_event_timestamp,start=1,end=10)), # get just the date part and parse
      datetimestamp = mdy_hms(utc_event_timestamp), # fully convert timestamp
      correctedtime = dplyr::case_when (
        datetimestamp < starttime ~ starttime, 
        TRUE ~ datetimestamp
      )
    )
  
  # get a table of attendees joining the event within the time window
  data_joined <- data |>
    dplyr::filter(
      action == "Joined",
      role == "Attendee",
      datestamp > eventdate-1,
      datetimestamp < endtime
    )
  
  # get a table of attendees leaving the event within the time window
  data_left <- data |>
    dplyr::filter(action == "Left") |>
    dplyr::filter(role == "Attendee") |>
    dplyr::filter(datetimestamp > starttime)|>
    dplyr::mutate(correctedtime = dplyr::case_when ( 
      datetimestamp > endtime ~ endtime, 
      TRUE ~ datetimestamp)
    )
  
  # for attendees who joined and left multiple times:
  # a) get the earliest time they joined within the window
  min_data_joined <-data_joined |>
    group_by(full_name) |>
    filter(correctedtime == min(correctedtime)) 
  
  # b) get the latest time they left within the window
  max_data_left <-data_left |>
    group_by(full_name) |>
    filter(correctedtime == max(correctedtime))
  
  # merge joiners and leavers data
  merge_data_join <- select(min_data_joined,participant_id,full_name,correctedtime) |>
    rename(joinedtime = correctedtime) 
  
  merge_data_left <- select(max_data_left,full_name,correctedtime) |>
    rename(lefttime = correctedtime)
  
  merged_data <- merge(x = merge_data_join, y = merge_data_left, by = "full_name") 
  
  # Convert invalid attendee names to NA then exlude from data
  merged_data$full_name[merged_data$full_name==" "] <- NA
  merged_data <-na.omit(merged_data)
  
  # get a distinct list of data and work out how long they attended
  merged_data <- distinct(merged_data, full_name, .keep_all = TRUE) |>
    mutate(how_long = as.numeric(difftime(lefttime, joinedtime, units = "mins")))
  
  #  merged_data <- select(merged_data, full_name,participant_id,joined_time,left_time,how_long)
  
  # return the result
  return(merged_data)
  
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmodaldate <- function(timestamp) {
  # figure out the modal date
  date <-getmode(format(mdy(str_sub(timestamp,start=1,end=10)),"%d"))
  month <-getmode(format(mdy(str_sub(timestamp,start=1,end=10)),"%m"))
  year <-getmode(format(mdy(str_sub(timestamp,start=1,end=10)),"%Y"))
  modaldate <-ymd(paste0(year,"-",month,"-",date))
  return(modaldate)
}

getmedianhour <- function(timestamp) {
  medianhour <- round(median(as.numeric((str_sub(timestamp,start=1,end=2)))))
  return(medianhour)
}

getmedianmin <- function(timestamp) {
  medianmin <- round(median(as.numeric((str_sub(timestamp,start=4,end=5)))))
  return(medianmin)
}


create_chart <- function(merged_data,filename,modaldate) {
  
  chart_data<- merged_data |>
    mutate(arrival_times=floor_date(joinedtime,unit='minute'),
           depart_times=floor_date(lefttime,unit='minute'))
  
  # Joined
  arrivals <- chart_data|> 
    select(timestamp=arrival_times)|>
    mutate(counter=1)
  
  # Left
  departures <- chart_data |>
    select(timestamp=depart_times) |>
    mutate(counter=-1)
  
  #Volumes per minute
  census_volumes <- arrivals |>
    bind_rows(departures) |>
    arrange(timestamp,counter) |>   #arrange by time
    mutate(volume=cumsum(counter)) #cumsum of counters to get the exact volumes at that point.
  
  # create a sequence of times from the start to end of available data.
  
  start <- min(census_volumes$timestamp)
  end   <- max(census_volumes$timestamp)
  full_time_window <- tibble(timestamp=seq(start,end,by='mins'))
  
  #right join to get the missing time intervals.
  
  census_volumes <- census_volumes |> 
    right_join(full_time_window,by='timestamp') |>
    arrange(timestamp) |>
    fill(volume,.direction='down') #take last observation carried forward.
  
  
  chart <-census_volumes |>
    ggplot(mapping=aes(x=timestamp,y=volume))+
    geom_line(colour="orange",size=1)+
    labs(title="Event Attendance by Minute",
         subtitle=modaldate,
         caption=paste0('Data Source: ',filename))+
    theme(
      plot.title = element_text(color = "black", size = 12, face = "bold",hjust=0.5),
      plot.subtitle = element_text(color = "black",hjust=0.5),
      plot.caption = element_text(color = "black", face = "italic",hjust=1)
    )+
    xlab('Time')+
    ylab('Attendees')
  
  return(chart) 
  
}

create_how_long_chart <- function(merged_data,filename,modaldate){
  chart<- 
    ggplot(merged_data,aes(how_long))+
    geom_bar(colour="orange",fill="orange")+
    #geom_histogram()+
    labs(title="Length of Event Attendance (Minutes)",
         subtitle=modaldate,
         caption=paste0('Data Source: ',filename))+
    theme(
      plot.title = element_text(color = "black", size = 12, face = "bold",hjust=0.5),
      plot.subtitle = element_text(color = "black",hjust=0.5),
      plot.caption = element_text(color = "black", face = "italic",hjust=1)
    )+
    xlab('Minutes Attended')+
    ylab('Number of Attendees')
  return(chart)
}


# Code taken from an internet search for use with import of Zoom file: 
# https://stackoverflow.com/questions/39110755/skip-specific-rows-using-read-csv-in-r
#' read csv table, wrapper of \code{\link{read.csv}}
#' @description read csv table, wrapper of \code{\link{read.csv}}
#' @param tolower whether to convert all column names to lower case
#' @param skip.rows rows to skip (1 based) before read in, eg 1:3
#' @return returns a data frame
#' @export
ez.read = function(file, ..., skip.rows=NULL, tolower=FALSE){
  if (!is.null(skip.rows)) {
    tmp = readLines(file)
    tmp = tmp[-(skip.rows)]
    tmpFile = tempfile()
    on.exit(unlink(tmpFile))
    writeLines(tmp,tmpFile)
    file = tmpFile
  }
  result = read.csv(file, ...)
  if (tolower) names(result) = tolower(names(result))
  return(result)
}


#' Estimate event times
#' 
#' Estimates the start and end times for an event from a given tibble of uploaded data
#'
#' @param df Dataframe of an uploaded MS Teams Attendee Report
#'
#' @return Tibble containing two fields and one row identifying the estimated start and end datetimes for an event.
#' @export
#'
#' @examples df_estimated_event_times = estimate_event_times(df = df)
estimate_event_times <- function(df) {
  
  # try to determine the field containing datetime information and alias it
  if ('utc_event_timestamp' %in% colnames(df)) {
    df <- df |> 
      rename(event_timestamp = utc_event_timestamp)
  } else if ('UTC Event Timestamp' %in% colnames(df)) {
    df <- df |> 
      rename(event_timestamp = `UTC Event Timestamp`)
  }
  
  # parse the datetime and then round it to 15 minute intervals
  df <- df |> 
    mutate(
      # parse datetime from the utc event string - nb, using %I for the hour so takes account of am/pm
      action_datetime = date_time_parse(event_timestamp, format = '%m/%d/%Y %I:%M:%S %p', zone = 'UTC'),
      # round to 15 minute intervals
      action_datetime_rounded = date_round(action_datetime, precision = 'minute', n = 15)
    ) |> 
    # remove duplicate records - i.e. where participant joins the session on multiple devices at the same time
    select(-session_id) |> 
    unique()
  
  # estimate start time from the average (median) rounded start time
  df_start <- df |> 
    filter(action == 'Joined') |> 
    summarise(start = median(action_datetime_rounded))
  
  # estimate end time from the third quartile rounded time for attendees leaving
  df_end <- df |> 
    filter(action == 'Left') |> 
    summarise(end = quantile(action_datetime_rounded, probs = 0.75))
  
  # combine together to a single table with one row and two columns
  df_return <- bind_cols(
    df_start,
    df_end
  )
  
  # return the result
  return(df_return)
}
