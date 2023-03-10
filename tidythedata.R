

library(lubridate)
library(stringr)
library(tidyverse)


data <- read.csv("data/AttendeeReportTest.csv") |>
  dplyr::mutate(datestamp = mdy(str_sub(`UTC.Event.Timestamp`,start=1,end=10))) |>
  dplyr::mutate(datetimestamp = mdy_hms(`UTC.Event.Timestamp`))



eventdate <- ymd("2023-03-02")
starttime <- ymd_hms("2023-03-02 11:00:00")
endtime <- ymd_hms("2023-03-02 12:00:00")



data_joined <- data |>
  dplyr::filter(Action == "Joined") |>
  dplyr::filter(Role == "Attendee") |>
  dplyr::filter(datestamp > eventdate-1) |>
  dplyr::filter(datetimestamp < endtime) |>
  dplyr::mutate(correctedtime = dplyr::case_when ( datetimestamp < starttime ~ starttime, 
                                                   TRUE ~ datetimestamp))
  

data_left <- data |>
  dplyr::filter(Action == "Left") |>
  dplyr::filter(Role == "Attendee") |>
  dplyr::filter(datetimestamp > starttime)|>
  dplyr::mutate(correctedtime = dplyr::case_when ( datetimestamp > endtime ~ endtime, 
                                        TRUE ~ datetimestamp))

min_data_joined <-data_joined |>
  group_by(Full.Name) |>
  filter(correctedtime == min(correctedtime)) 

max_data_left <-data_left |>
  group_by(Full.Name) |>
  filter(correctedtime == max(correctedtime))


merge_data_join <- select(min_data_joined,Participant.Id,Full.Name,correctedtime) |>
                rename(joinedtime = correctedtime) 

merge_data_left <- select(max_data_left,Full.Name,correctedtime) |>
  rename(lefttime = correctedtime)




merged_data <- merge(x = merge_data_join, y = merge_data_left, by = "Full.Name") 

merged_data$Full.Name[merged_data$Full.Name==" "] <- NA

merged_data <-na.omit(merged_data)

merged_data <- distinct(merged_data, Full.Name, .keep_all = TRUE) |>
  mutate(how_long = as.numeric(difftime(lefttime, joinedtime, units = "mins")))

average_attend_time <- mean(merged_data$how_long)
attend_less_than_15 <- nrow(merged_data |> filter(how_long < 15))
number_of_attendees <- nrow(merged_data)
attend_more_than_45 <- nrow(merged_data |> filter(how_long > 45))
joined_after_15mins <- nrow(merged_data |> filter(joinedtime > starttime+900))#900 is the number of seconds in 15 mins

#save the data to a csv
#write.csv(merged_data,"data/merged_data.csv")



  
