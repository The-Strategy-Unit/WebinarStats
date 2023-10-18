# Code in development for the import of files from Zoom events

library(lubridate)
library(stringr)
library(tidyverse)

data <- ez.read("secret/zoomfile.csv",skip.rows=1:3, tolower=FALSE) |>
  janitor::clean_names() |>
  filter(guest == 'Yes') |>
  rename(joinedtime=join_time,
         lefttime=leave_time,
         participant_id=user_email,
         full_name=name_original_name)

modaljoinhour <- getmode(str_sub(data$joinedtime,start=1,end=2))
modaljoinmin <- getmode(str_sub(data$joinedtime,start=4,end=5))
medianjoinhour <- round(median(as.numeric((str_sub(data$joinedtime,start=1,end=2)))))
medianjoinmin <- round(median(as.numeric((str_sub(data$joinedtime,start=4,end=5)))))

modallefthour <- getmode(str_sub(data$lefttime,start=1,end=2))
modalleftmin <- getmode(str_sub(data$lefttime,start=4,end=5))
medianlefthour <- round(median(as.numeric((str_sub(data$lefttime,start=1,end=2)))))
medianleftmin <- round(median(as.numeric((str_sub(data$lefttime,start=4,end=5)))))




  
  zoom_data <- distinct(data, full_name, .keep_all = TRUE) |>
  mutate(how_long = as.numeric(difftime(lefttime, joinedtime, units = "mins")))

  dplyr::mutate(datestamp = mdy(str_sub(utc_event_timestamp,start=1,end=10))) |>
  dplyr::mutate(datetimestamp = mdy_hms(utc_event_timestamp)) |>
  dplyr::mutate(roundtime = format(round(strptime(mdy_hms(utc_event_timestamp), format="%Y-%m-%d %H:%M"), units="hours"), format="%H:%M"))
