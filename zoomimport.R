

library(lubridate)
library(stringr)
library(tidyverse)

data <- ez.read("secret/zoomfile.csv",skip.rows=1:3, tolower=FALSE) |>
  janitor::clean_names() |>
  filter(guest == 'Yes') |>
  rename(joinedtime=join_time,
         lefttime=leave_time,
         participant_id=user_email,
         full_name=name_original_name) |>

  dplyr::mutate(datestamp = mdy(str_sub(utc_event_timestamp,start=1,end=10))) |>
  dplyr::mutate(datetimestamp = mdy_hms(utc_event_timestamp)) |>
  dplyr::mutate(roundtime = format(round(strptime(mdy_hms(utc_event_timestamp), format="%Y-%m-%d %H:%M"), units="hours"), format="%H:%M"))
