# WebinarStats
Code to take the AttendeeReport.csv created after a Teams Live Event, clean the data and generate some stats

The idea of this is to take the auto-generated csv reports from Teams Live events and generate some data about the attendance at the event.
Initially the filename, its location and the date and start and finish times are hard-coded into the script. Eventualy I would like this to maybe be a small shiny app where I can select the file and enter the relevant event details and then present a few stats

The data here is a test file.  If you use the Attendee engagement report from your live event in Teams then it will be named:  AttendeeReport.csv 
You can obtain it via the calendar in Teams, click on the event and then look here at Live Event resources and then download it.

![image](https://user-images.githubusercontent.com/103451105/224339648-fba18b0f-fe33-4252-b59d-3914699d845d.png)


Put it into the data folder and the script will pick it up from there
