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
# 
# getstarttime <- function(t) {
#   
#   starttimeestimate <- t
# }

