# WebinarStats

There was a short presentation on this project at NHS-R/NHS.pycom Conference in October 2023:

https://www.youtube.com/watch?v=8jcPwD8x8WE

The presentation is at 4:46:26 and is called, "Getting started with collaboration on GitHub: Crunching webinar attendance data with R and Shiny"


This repository contains code to take the files created after MS Teams Live Events and Webinars or Zoom events, clean the data and generate some stats

Initially this is working for the AttendeeReport.csv created after a Teams Live Event. Zoom file functiionality is under development.

The idea of this is to take the auto-generated csv reports from the events and generate some data about the attendance at the event.

The file tidythedata.R has the filename, its location and the date and start and finish times hard-coded into the script and generates some basic stats with option to output a "tidy" csv file of the results.

Also within this project is the development of a small shiny app where the user can select the file and type, enter the relevant event details and then present a few stats and charts, maybe some functionality to export a useable file of data etc, short pdf report of the charts and basic stats etc.

The data here is a test file.  If you use the Attendee engagement report from your live event in Teams then it will be named:  AttendeeReport.csv 
You can obtain it via the calendar in Teams, click on the event and then look here at Live Event resources and then download it.

![image](https://user-images.githubusercontent.com/103451105/224339648-fba18b0f-fe33-4252-b59d-3914699d845d.png)


Put it into the data folder and the script will pick it up from there
