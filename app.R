library(shiny)
library(tidyverse)
library(readr)
library(lubridate)
library(stringr)
library(shinyTime)


source("functions_script.R")

# Main Panel
ui_mainpanel <- mainPanel(
    tableOutput("file"),
    #textOutput("eventdate"),
    #textOutput("starttime"),
    #textOutput("endtime"),
    textOutput("averagetime"),
    textOutput("attend_less_than_15"),
    textOutput("number_of_attendees"),
    textOutput("attend_more_than_45"),
    textOutput("joined_after_15mins"),
    plotOutput("plot"),
 width=8)


ui <- fluidPage(
  
  theme = bslib::bs_theme(bootswatch = "united"),

  titlePanel(windowTitle = "Microsoft Teams Live Event Statistics",
             
             fixedRow(
               column(width = 1, tags$img(src="tsu_logo_yellow_screen_transparent.png", height = '90'),
                      style = "background-color:#FFFFFF;"),
               column(width = 11,
                      h1(p("Microsoft Teams Live Event Statistics"),
                         style = "font-weight:bold; font-family:Segoe UI; font-size:xx-large"),
                      style = "background-color:#FFFFFF;")
             )
  ),
  sidebarLayout(
    
    sidebarPanel(
      fileInput("file", "Select the csv file containing the Attendance Data for your Teams Live Event:", accept = ".csv"),
#     numericInput("n", "Rows", value=10, min = 1, step = 1),
      dateInput("live_event_date", "What was the date of your Teams Live Event?"),
      timeInput("start", "Enter event start time (15 minute steps)", value = strptime("09:00:00", "%T"), minute.steps = 15),
      timeInput("end", "Enter event end time (15 minute steps)", value = strptime("10:00:00", "%T"), minute.steps = 15),
#      numericInput("start2", "Start time of event", 11, min = 0),
#      numericInput("end2", "End time of event", 12, min = 0),
      actionButton("click","Generate stats", icon("bar-chart-o"), style="color: #FFFFFF; background-color: #D20019; border-color: #8C0032")
    ),
    ui_mainpanel,
    position = c("left", "right"),
    fluid = TRUE
  )
)
  
server <- function(input,output,session) {

  
      data <- reactive({
        file <- req(input$file)
        ext <- tools::file_ext(input$file$name)
        d <- switch(ext,
               csv = readr::read_csv(input$file$datapath) |>
                 janitor::clean_names(),
               validate("invalid csv")
        ) 
            
        modaldate <-getmodaldate(d$utc_event_timestamp)
        
        # figure out the modal start time
        #modaljoinhour <- getmode(str_sub(format(round(strptime(mdy_hms(d$utc_event_timestamp), format="%Y-%m-%d %H:%M"), units="hours"), format="%H:%M"),start=1,end=2))
        #modaljoinmin <- getmode(str_sub(format(round(strptime(mdy_hms(d$utc_event_timestamp), format="%Y-%m-%d %H:%M"), units="hours"), format="%H:%M"),start=4,end=5))
       # modalstarttime <- ymd_hms(paste0(modaldate," ",strftime(paste0(modaljoinhour,":",modaljoinmin,":00"), "%T")))
        
       
        shiny::updateDateInput(session, "live_event_date", value = modaldate)
        #shiny::updateTextInput(session, "start", value = modalstarttime)
        
        return (d)
      }) |>
        bindEvent(input$file)
  
     # get_data <- eventReactive(input$click,{
     #   req(input$file)
     #   # ,input$live_event_date,input$start<25,input$end<25
     #   ext <- tools::file_ext(input$file$name)
     #   switch(ext,
     #          csv = readr::read_csv(input$file$datapath) |>
     #            janitor::clean_names(),
     #          validate("invalid csv")
     #          )
     # 
     # }) 

     get_joined <-reactive({
       data <- data()
       
       validate(
         need(nrow(data) > 0,"no rows of data uploaded"),
         need("utc_event_timestamp" %in% colnames(data), "not correct csv")
       )
       
       eventdate <- ymd(input$live_event_date)
       
       #cat(eventdate, "\n")
       starttime <-ymd_hms(paste0(ymd(input$live_event_date)," ",strftime(input$start, "%T")))
       endtime <-ymd_hms(paste0(ymd(input$live_event_date)," ",strftime(input$end, "%T")))
       #starttime <- ymd_hms("2023-03-02 11:00:00")
       #endtime <- ymd_hms("2023-03-02 12:00:00")
       get_joined_data(data(),eventdate,starttime,endtime)
       })
    
     get_chart <-reactive({
       create_chart(get_joined(),input$file$name,input$live_event_date)
     })
  
    output$averagetime <- renderText({
      paste0("Average time attending the event: ",
             round(mean(get_joined()$how_long)),
             " minutes")
      })
    
    output$attend_less_than_15 <- renderText({
      paste0("Attended for less than 15 minutes: ",
             nrow(get_joined() |>
            filter(how_long < 15)),
            " people")
      })
    
    output$number_of_attendees <- renderText({
      paste0("Number of attendees: ",
             nrow(get_joined()),
             " people")
      })
    
    
    output$attend_more_than_45 <- renderText({
      paste0("Attended for more than 45 minutes: ",
             nrow(get_joined() |> 
             filter(how_long > 45)),
             " people")
      })
    

    output$joined_after_15mins <- renderText({
      paste0("Joined after 15 minutes: ",
             nrow(get_joined() |>
             filter(joinedtime > (ymd_hms(paste0(ymd(input$live_event_date)," ",strftime(input$start, "%T")))+900))),#900 is the number of seconds in 15 mins
             " people")
      })
    
    #glue package
    
    #output$head <- renderTable({head(get_joined(),input$n)})
    output$upload <- renderTable(input$upload)
    output$eventdate <- renderText(input$live_event_date)
    #output$starttime <- renderText(input$start)
    #output$endtime <- renderText(input$end)
    output$starttime <- renderText(strftime(input$start, "%T"))
    output$endtime <- renderText(strftime(input$end, "%T"))
    output$plot <- renderPlot({get_chart()})
    
}

shinyApp(ui,server)