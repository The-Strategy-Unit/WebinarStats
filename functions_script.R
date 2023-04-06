get_joined_data <- function(data,eventdate,starttime,endtime){
  data |>
    dplyr::mutate(
      datestamp = mdy(str_sub(utc_event_timestamp,start=1,end=10)),
      datetimestamp = mdy_hms(utc_event_timestamp),
      correctedtime = dplyr::case_when ( datetimestamp < starttime ~ starttime, 
                                                     TRUE ~ datetimestamp)
    ) |>
    dplyr::filter(
      action == "Joined",
      role == "Attendee",
      datestamp > eventdate-1,
      datetimestamp < endtime
    )
  
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