get_joined_data <- function(data,eventdate,starttime,endtime){
  data <- data |>
    dplyr::mutate(
      datestamp = mdy(str_sub(utc_event_timestamp,start=1,end=10)),
      datetimestamp = mdy_hms(utc_event_timestamp),
      correctedtime = dplyr::case_when ( datetimestamp < starttime ~ starttime, 
                                                     TRUE ~ datetimestamp)
    )
  
  data_joined <- data |>
    dplyr::filter(
      action == "Joined",
      role == "Attendee",
      datestamp > eventdate-1,
      datetimestamp < endtime
    )
  
  
  data_left <- data |>
    dplyr::filter(action == "Left") |>
    dplyr::filter(role == "Attendee") |>
    dplyr::filter(datetimestamp > starttime)|>
    dplyr::mutate(correctedtime = dplyr::case_when ( datetimestamp > endtime ~ endtime, 
                                                     TRUE ~ datetimestamp))
  

  
  min_data_joined <-data_joined |>
    group_by(full_name) |>
    filter(correctedtime == min(correctedtime)) 
  
  max_data_left <-data_left |>
    group_by(full_name) |>
    filter(correctedtime == max(correctedtime))
  
  
  merge_data_join <- select(min_data_joined,participant_id,full_name,correctedtime) |>
    rename(joinedtime = correctedtime) 
  
  merge_data_left <- select(max_data_left,full_name,correctedtime) |>
    rename(lefttime = correctedtime)
  
  
  merged_data <- merge(x = merge_data_join, y = merge_data_left, by = "full_name") 
  
  merged_data$full_name[merged_data$full_name==" "] <- NA
  
  merged_data <-na.omit(merged_data)
  
  merged_data <- distinct(merged_data, full_name, .keep_all = TRUE) |>
    mutate(how_long = as.numeric(difftime(lefttime, joinedtime, units = "mins")))
  
#  merged_data <- select(merged_data, full_name,participant_id,joined_time,left_time,how_long)
  
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
