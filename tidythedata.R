

library(lubridate)
library(stringr)
library(tidyverse)

source("functions_script.R")

  data <- readr::read_csv("data/AttendeeReportTest.csv") |>
  janitor::clean_names() |>
  janitor::clean_names() |>
  dplyr::mutate(datestamp = mdy(str_sub(utc_event_timestamp,start=1,end=10))) |>
  dplyr::mutate(datetimestamp = mdy_hms(utc_event_timestamp)) |>
  dplyr::mutate(roundtime = format(round(strptime(mdy_hms(utc_event_timestamp), format="%Y-%m-%d %H:%M"), units="hours"), format="%H:%M"))

joined <- data |>
  dplyr::filter(action == "Joined") |>
  dplyr::filter(role == "Attendee")

#modal or median times?
modaljoinhour <- getmode(str_sub(joined$roundtime,start=1,end=2))
modaljoinmin <- getmode(str_sub(joined$roundtime,start=4,end=5))
medianjoinhour <- round(median(as.numeric((str_sub(joined$roundtime,start=1,end=2)))))
medianjoinmin <- round(median(as.numeric((str_sub(joined$roundtime,start=4,end=5)))))

left <- data |>
  dplyr::filter(action == "Left") |>
  dplyr::filter(role == "Attendee")

#modal or median times?
modallefthour <- getmode(str_sub(left$roundtime,start=1,end=2))
modalleftmin <- getmode(str_sub(left$roundtime,start=4,end=5))
medianlefthour <- round(median(as.numeric((str_sub(left$roundtime,start=1,end=2)))))
medianleftmin <- round(median(as.numeric((str_sub(left$roundtime,start=4,end=5)))))

date <- getmode(format(data$datestamp,"%d"))
month <- getmode(format(data$datestamp,"%m"))
year <- getmode(format(data$datestamp,"%Y"))
modaldate <-ymd(paste0(year,"-",month,"-",date))

#used median time and modal date
eventdate <- modaldate
starttime <- ymd_hms(paste0(eventdate," ",medianjoinhour, ":",medianjoinmin,":00"))
endtime <- ymd_hms(paste0(eventdate," ",medianlefthour, ":",medianleftmin,":00"))

data_joined <- data |>
  dplyr::filter(action == "Joined") |>
  dplyr::filter(role == "Attendee") |>
  dplyr::filter(datestamp > eventdate-1) |>
  dplyr::filter(datetimestamp < endtime) |>
  dplyr::mutate(correctedtime = dplyr::case_when ( datetimestamp < starttime ~ starttime,TRUE ~ datetimestamp))



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

average_attend_time <- mean(merged_data$how_long)
attend_less_than_15 <- nrow(merged_data |> filter(how_long < 15))
number_of_attendees <- nrow(merged_data)
attend_more_than_45 <- nrow(merged_data |> filter(how_long > 45))
joined_after_15mins <- nrow(merged_data |> filter(joinedtime > starttime+900))#900 is the number of seconds in 15 mins

#save the data to a csv
#write.csv(merged_data,"data/merged_data.csv")
