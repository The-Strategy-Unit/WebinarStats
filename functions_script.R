#' -----------------------------------------------------------------------------
#' FUNCTIONS USED IN SCRIPT
#' 
#' Here are functions used throughout the script
#' -----------------------------------------------------------------------------

# libraries --------------------------------------------------------------------
library(tidyverse)         # tidy processing
library(clock)             # datetime processing
library(vroom)             # speedy file loads
library(here)              # localised file references
library(janitor)           # tidy field names
library(uaparserjs)        # parse user-agent strings

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
    #summarise(end = median(action_datetime_rounded))
  
  # combine together to a single table with one row and two columns
  df_return <- bind_cols(
    df_start,
    df_end
  )
  
  # return the result
  return(df_return)
}


#' Process an imported attendance report file
#' 
#' Takes a tibble of data from an imported attendance file and prepares it for
#' analysis by cleaning column names, parsing times and user-agent strings and
#' then joining session join/left records to return a single tidy dataset with
#' one session per row.
#' 
#' NB, participants with multiple sessions will have multiple rows but these
#' will be handled in subsequent analysis function calls.
#' 
#' @param df An imported attendance file from MS Teams
#'
#' @return Tibble of data with parsed fields and one row per session
#' @export
#'
#' @examples
process_imported_file <- function(df) {
  
  # clean fields and parse datetimes ----
  df <- df |> 
    # make the column names easier to work with
    clean_names() |> 
    # work out when things happened
    mutate(
      # parse datetime from the utc event string - nb, using %I for the hour so takes account of am/pm
      action_datetime = date_time_parse(utc_event_timestamp, format = '%m/%d/%Y %I:%M:%S %p', zone = 'UTC'),
      # round to 15 minute intervals
      action_datetime_rounded = date_round(action_datetime, precision = 'minute', n = 15),
    ) |> 
    # remove fields no longer required
    select(-utc_event_timestamp) |> 
    # arrange by participant
    arrange(participant_id)
  
  # tidy data ----
  # combine start/end dates for each session to a single row
  df <- left_join(
    x = df |> filter(action == 'Joined') |> 
      rename(joined_datetime = action_datetime, joined_datetime_rounded = action_datetime_rounded),
    y = df |> filter(action == 'Left') |> 
      select(session_id, left_datetime = action_datetime, left_datetime_rounded = action_datetime_rounded),
    by = 'session_id'
  ) |> 
    # remove fields no longer required
    select(-session_id, -action) |> 
    # work out length of session in seconds
    mutate(session_length = left_datetime - joined_datetime) |> 
    # put relevant columns together
    relocate(left_datetime, .after = joined_datetime) |> 
    relocate(session_length, .after = left_datetime)
  
  # parse user-agent details ----
  df_ua <- df |> select(user_agent) |> unique()
  df_ua <- ua_parse(df_ua$user_agent) |> clean_names() |> 
    # pick out data-dense fields that are likely to be useful
    select(
      user_agent,     # key back to the df table
      ua_family,      # the type of 'browser' the session was attended on
      os_family,      # the operating system used to access the session
      device_family,  # what type of device accessed the session
    ) |> 
    # categorise into mobile / other based on os_family
    mutate(
      os_formfactor = case_when(
        os_family %in% c('Android', 'iOS') ~ 'Mobile',
        TRUE ~ 'Computer'
      )
    )
  
  # add the ua details to the df
  df <- left_join(
    x = df,
    y = df_ua,
    by = 'user_agent'
  ) |> 
    # remove fields no longer required
    select(-user_agent)
  
  # return the result
  return(df)
  
}


#' Get attendee data for an event
#' 
#' Takes a processed df as an output from process_imported_file() along with a
#' start and end datetime for an event and returns a list of unique attendees
#' with the duration of their attendance calculated. 
#' 
#' Attendees who had multiple sessions, for example because they joined / left 
#' more than once, will be assumed to have attended from their earliest join 
#' datetime to the their latest left datetime.
#'
#' @param df Tibble of data from a processed attendance file
#' @param event_start_datetime Datetime specifying the start of the event
#' @param event_end_datetime Datetime specifying the end of the event
#'
#' @return Tibble of attendees for the event with one row per attendee
#' @export
#' @seealso process_imported_file()
#'
#' @examples
get_event_attendees <- function(df, event_start_datetime, event_end_datetime) {
  
  # get attendee data ----------------------------------------------------------
  df_attendees <- df |> 
    # keep records where sessions coincided with the event start and end times
    filter(
      role == 'Attendee', # only work with attendees (not team members)
      !is.na(participant_id), # only work with participants with a valid id
      joined_datetime < event_end_datetime, # where they joined before the end of the session
      left_datetime > event_start_datetime # where they left after the start of the session
    ) |> 
    # keep unique records
    unique()
  
  # merge multiple sessions per person -----------------------------------------
  # assume they attended from their earliest session start to their latest session end
  
  # get the earliest session by join time
  df_attendees_earliest_join <- df_attendees |>
    group_by(participant_id) |> 
    slice_min(order_by = joined_datetime) |> 
    slice_head(n = 1) |> # some sessions appear duplicate times
    ungroup() |> 
    select(participant_id, joined_datetime, joined_datetime_rounded)
  
  # get the latest session by left time
  df_attendees_latest_left <- df_attendees |> 
    group_by(participant_id) |> 
    slice_max(order_by = left_datetime) |> 
    slice_tail(n = 1) |> # some sessions appear duplicate times
    ungroup() |> 
    select(participant_id, left_datetime, left_datetime_rounded)
  
  # get a single device per participant (select the device with longest use)
  df_attendees_device <- df_attendees |> 
    group_by(participant_id) |> 
    slice_max(order_by = session_length) |> 
    slice_head(n = 1) |> # some sessions have the same length so in this case select the first one
    ungroup() |> 
    select(participant_id, ua_family, os_family, device_family, os_formfactor)
  
  # combine to a single table
  df_attendees_unique <- df_attendees |> 
    select(participant_id, full_name) |> 
    unique()
  
  df_attendees_unique <- left_join(
    x = df_attendees_unique,
    y = df_attendees_earliest_join,
    by = 'participant_id'
  )
  df_attendees_unique <- left_join(
    x = df_attendees_unique,
    y = df_attendees_latest_left,
    by = 'participant_id'
  )
  df_attendees_unique <- left_join(
    x = df_attendees_unique,
    y = df_attendees_device,
    by = 'participant_id'
  )
  
  # housekeeping
  rm(df_attendees_earliest_join, df_attendees_latest_left, df_attendees_device)
  
  # work out the length time each participant attended the session -------------
  df_attendees <- df_attendees_unique |> 
    mutate(
      attendance_start_datetime = pmax(joined_datetime, event_start_datetime),
      attendance_end_datetime = pmin(left_datetime, event_end_datetime),
      attendance_duration = date_count_between(
        start = attendance_start_datetime,
        end = attendance_end_datetime,
        precision = 'minute'
      )
    )
}

#' Get attendance count by minute
#' 
#' Gets a count of attendees for each minute of the event.
#'
#' @param df_attendees Tibble of attendees with a unique id, attendance start and end times 
#' @param start Datetime of the start of the event
#' @param end Datetime of the end of the event
#'
#' @return Tibble of with the number of attendees for each minute of the event
#' @export
#'
#' @examples
get_attandance_count_per_minute <- function(df_attendees, start, end) {
  
  # get a sequence of datetimes from event start to event end
  df_minutes <- seq(
    #from = make_datetime(2023,03,02,11),
    #to = make_datetime(2023,03,02,12),
    from = start,
    to = end,
    by = '1 min'
  ) |> 
    # convert to tibble and name the column
    as_tibble() |> 
    rename(minute = value)
  
  # cartesian join all minutes to each attendee record
  df_attendees_per_minute <- merge(
    x = df_attendees,
    y = df_minutes
  ) |> 
    # limit to where the minute is within attendance start and end dates
    filter(
      !is.na(attendance_start_datetime),
      !is.na(attendance_end_datetime),
      (attendance_start_datetime <= minute) & (attendance_end_datetime >= minute)
    ) |> 
    # count number of participants per minute
    group_by(minute) |> 
    summarise(attendees = n_distinct(participant_id)) |> 
    ungroup()
  
  # return the result
  return(df_attendees_per_minute)
  
}

get_event_datetime <- function(event_date, event_time) {
  event_datetime <- date_time_parse(
    x = paste(
      strftime(event_date),
      strftime(event_time, '%T')
    ),
    zone = 'UTC'
  )
  return(event_datetime)
}


calculate_attendance_statistic <- function(df_attendees) {
  
  # combine all stats to a tibble with a column each
  df_return = bind_cols(
    
    # average attendance duration in minutes
    mean_duration = paste(
      prettyNum(
        round(
          mean(
            df_attendees$attendance_duration
          )
        ), big.mark = ','
      ), 'mins'
    ),
    
    # number of attendees for the event
    number_of_attendees = paste(
      prettyNum(
        n_distinct(df_attendees$participant_id),
        big.mark = ','
      ), 'people'
    ),
    
    # number of attendees who stayed for less than 15 minutes
    attend_less_than_15 = paste(
      prettyNum(
        n_distinct(
          df_attendees |> 
            filter(attendance_duration < 15) |> 
            select(participant_id) |> 
            as_vector()
        ), big.mark = ','
      ), 'people'
    ),
    
    # number of people who stayed for over 45 minutes
    attend_more_than_45 = paste(
      prettyNum(
        n_distinct(
          df_attendees |> 
            filter(attendance_duration > 45) |> 
            select(participant_id) |> 
            as_vector()
        ), big.mark = ','
      ), 'people'
    ),
    
    # people who joined the event after 15 minutes
    joined_after_15mins = paste(
      prettyNum(
        n_distinct(
          df_attendees |> 
            mutate(start_time_plus15 = add_minutes(min(attendance_start_datetime), 15)) |> 
            filter(attendance_start_datetime > start_time_plus15) |> 
            select(participant_id) |> 
            as_vector()
        ), big.mark = ','
      ), 'people'
    ),
    
    # people who attended on a computer device
    attended_on_computer = paste(
      prettyNum(
        n_distinct(
          df_attendees |> 
            filter(os_formfactor == 'Computer') |> 
            select(participant_id) |> 
            as_vector()
        ), big.mark = ','
      ), 'people'
    ),
    
    # people who attended on a mobile device
    attended_on_mobile = paste(
      prettyNum(
        n_distinct(
          df_attendees |> 
            filter(os_formfactor == 'Mobile') |> 
            select(participant_id) |> 
            as_vector()
        ), big.mark = ','
      ), 'people'
    )
    
  ) # end of bind_col ---

  # return the result
  return(df_return)

}




