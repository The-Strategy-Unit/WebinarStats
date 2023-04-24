#' -----------------------------------------------------------------------------
#' WEBINAR STATS APP
#' 
#' Here is the Shiny code to control the ui and logic for the WebinarStats app
#' -----------------------------------------------------------------------------

# libraries --------------------------------------------------------------------
library(shiny)
library(tidyverse)
library(readr)
library(lubridate)
library(stringr)
library(shinyTime)
#library(flexdashboard)
library(clock)
library(shinydashboard)

## user-defined functions ----
source("functions_script.R")

# ui ---------------------------------------------------------------------------


ui_mainpanel <- mainPanel(
  # file
  tableOutput("file"),
  textOutput("averagetime"),
  textOutput("attend_less_than_15"),
  textOutput("number_of_attendees"),
  textOutput("attend_more_than_45"),
  textOutput("joined_after_15mins"),
  plotOutput("plot",width="90%"),
  plotOutput("plot2",width="90%"),
  width=8)

filetypes <- c("MS Teams Live Event", "MS Teams Webinar", "Zoom")

ui <- fluidPage(
  
  ## ui settings ---------------------------------------------------------------
  theme = bslib::bs_theme(bootswatch = "yeti"),
  
  ## title panel ---------------------------------------------------------------
  titlePanel(
    windowTitle = "Microsoft Teams and Zoom Event Statistics",
    fixedRow(
      column(
        width = 1, tags$img(src="tsu_logo_yellow_screen_transparent.png", height = '90'),
        style = "background-color:#FFFFFF;"
      ),
      column(
        width = 11, h1(p("Microsoft Teams and Zoom Event Statistics"),
                       style = "font-weight:bold; font-family:Segoe UI, Helvetica, Arial, sans-serif; font-size:xx-large"),
        style = "background-color:#FFFFFF;"
      )
    )
  ),
  
  ## sidebar -------------------------------------------------------------------
  sidebarLayout(
    
    ### sidebar settings ----
    position = c("left", "right"),
    fluid = TRUE,
    
    ### sidebar panel ----
    sidebarPanel(
      fileInput("file", "Select the csv file containing the attendance data for your event:", accept = ".csv"),
      radioButtons("file_type","Select file type", filetypes),
      dateInput("live_event_date", "What was the date of your event?"),
      timeInput("start", "Enter event start time (15 minute steps)", value = strptime("09:00:00", "%T"), minute.steps = 15),
      timeInput("end", "Enter event end time (15 minute steps)", value = strptime("17:00:00", "%T"), minute.steps = 15),
      
      #      actionButton("click","Generate stats", icon("bar-chart-o"), style="color: #FFFFFF; background-color: #D20019; border-color: #8C0032")
    ),
    
    ## main panel ----
    #ui_mainpanel,
    mainPanel(
      
      ### stats boxes ----
      fluidRow(
        
        valueBoxOutput(outputId = 'averagetime', width = 2),
        valueBoxOutput(outputId = 'attend_less_than_15', width = 2),
        valueBoxOutput(outputId = 'number_of_attendees', width = 2),
        valueBoxOutput(outputId = 'attend_more_than_45', width = 2),
        valueBoxOutput(outputId = 'joined_after_15mins', width = 2),
        
      ),
      
      fluidRow(
        plotOutput("plot",width="90%"),
        plotOutput("plot2",width="90%"),
      )
    )
    
  )
)

# server -----------------------------------------------------------------------
server <- function(input,output,session) {
  
  ## file uploads --------------------------------------------------------------
  data <- reactive({
    
    # ensure we have a file uploaded
    file <- req(input$file)
    
    # determine how to load / process based on filetype
    ext <- tools::file_ext(input$file$name)
    d <- switch(
      ext,
      csv = readr::read_csv(input$file$datapath) |>
        janitor::clean_names(),
      validate("invalid csv")
    ) 
    
    # update date and time parameters on the form
    d_events <- estimate_event_times(df = d)
    #modaldate <-getmodaldate(d$utc_event_timestamp)
    #shiny::updateDateInput(session, "live_event_date", value = modaldate)
    updateDateInput(session, 'live_event_date', value = as_date(d_events$start))
    updateTimeInput(session, 'start', value = d_events$start)
    updateTimeInput(session, 'end', value = d_events$end)
    
    return (d)
  }) |>
    bindEvent(input$file)
  
  
  get_joined <-reactive({
    data <- data()
    
    validate(
      need(nrow(data) > 0,"no rows of data uploaded"),
      need("utc_event_timestamp" %in% colnames(data), "not correct csv")
    )
    
    eventdate <- ymd(input$live_event_date)
    
    #medianhour <-getmedianhour(input$start)
    #medianmin <-getmedianmin(input$start)
    
    #cat(medianhour)
    #cat(medianmin)
    
    starttime <-ymd_hms(paste0(ymd(input$live_event_date)," ",strftime(input$start, "%T")))
    endtime <-ymd_hms(paste0(ymd(input$live_event_date)," ",strftime(input$end, "%T")))
    get_joined_data(data(),eventdate,starttime,endtime)
    
    # shinyTime::updateTimeInput(session, "start", value = hms(paste0(medianhour, ":",medianmin,":00")))
  })
  
  ## render charts -------------------------------------------------------------
  get_chart <-reactive({
    create_chart(get_joined(),input$file$name,input$live_event_date)
  })
  
  get_chart2 <-reactive({
    create_how_long_chart(get_joined(),input$file$name,input$live_event_date)
  })
  
  ## render valuebox -----------------------------------------------------------
  # average time attending the event
  # output$averagetime <- renderText({
  #   paste0("Average time attending the event: ",
  #          round(mean(get_joined()$how_long)),
  #          " minutes")
  #   prettyNum(round(mean(get_joined()$how_long)), big.mark = ',')
  #   })
  output$averagetime <- renderValueBox({
    valueBox(
      value = paste(prettyNum(round(mean(get_joined()$how_long)), big.mark = ','), 'mins'),
      subtitle = 'Average time attending the event',
      icon = icon('clock'),
      width = 2,
      color = 'purple'
    )
  })
  
  # output$attend_less_than_15 <- renderText({
  #   paste0("Attended for less than 15 minutes: ",
  #          nrow(get_joined() |>
  #                 filter(how_long < 15)),
  #          " people")
  # })
  output$attend_less_than_15 <- renderValueBox({
    valueBox(
      value = paste(prettyNum(nrow(get_joined() |> filter(how_long < 15)), big.mark = ','), 'people'),
      subtitle = 'Attended for less than 15 minutes',
      icon = icon('stopwatch'),
      width = 2,
      color = 'red'
    )
  })
  
  # output$number_of_attendees <- renderText({
  #   paste0("Number of attendees: ",
  #          nrow(get_joined()),
  #          " people")
  # })
  output$number_of_attendees <- renderValueBox({
    valueBox(
      value = paste(prettyNum(nrow(get_joined()), big.mark = ','), 'people'),
      subtitle = 'Attended the event',
      icon = icon('people-group'),
      width = 2,
      color = 'green'
    )
  })
  
  # output$attend_more_than_45 <- renderText({
  #   paste0("Attended for more than 45 minutes: ",
  #          nrow(get_joined() |> 
  #                 filter(how_long > 45)),
  #          " people")
  # })
  output$attend_more_than_45 <- renderValueBox({
    valueBox(
      value = paste(prettyNum(nrow(get_joined() |> filter(how_long > 45)), big.mark = ','), 'people'),
      subtitle = 'Attended for more than 45 minutes',
      icon = icon('people-line'),
      width = 2,
      color = 'green'
    )
  })
  
  # output$joined_after_15mins <- renderText({
  #   paste0("Joined after 15 minutes: ",
  #          nrow(get_joined() |>
  #                 filter(joinedtime > (ymd_hms(paste0(ymd(input$live_event_date)," ",strftime(input$start, "%T")))+900))),#900 is the number of seconds in 15 mins
  #          " people")
  # })
  output$joined_after_15mins <- renderValueBox({
    valueBox(
      value = paste(nrow(get_joined() |> filter(joinedtime > (ymd_hms(paste0(ymd(input$live_event_date)," ",strftime(input$start, "%T")))+900))), 'people'),
      subtitle = 'Joined after 15 minutes',
      icon = icon('right-to-bracket'),
      width = 2,
      color = 'green'
    )
  })
  
  # output$box1 <- renderValueBox({paste0("Joined after 15 minutes: ",
  #    nrow(get_joined() |>
  #              filter(joinedtime > (ymd_hms(paste0(ymd(input$live_event_date)," ",strftime(input$start, "%T")))+900))),#900 is the number of seconds in 15 mins
  #      " people")})
  
  
  # output$box1 <- renderValueBox({
  #   
  #   valueBox("Over Daily Value", HTML(paste0(nrow(get_joined() |> 
  #                                                   filter(how_long > 45)),
  #                                            sep="<br>")), icon = icon("exclamation-triangle"), color = "red")
  #   
  # })
  
  ## build value box
  # output$box1 <- valueBox({
  #   valueBox(
  #     (nrow(get_joined() |>
  #       filter(joinedtime > (ymd_hms(paste0(ymd(input$live_event_date)," ",strftime(input$start, "%T")))+900)))), 
  #    icon = icon('export', lib = 'glyphicon'), 
  #     color = "primary" )
  # })
  
  # output$box1 <- valueBox(247, caption = "Connections", icon="fa-random")
  
  
  output$upload <- renderTable(input$upload)
  # output$eventdate <- renderText(input$live_event_date)
  # output$starttime <- renderText(strftime(input$start, "%T"))
  # output$endtime <- renderText(strftime(input$end, "%T"))
  output$plot <- renderPlot({get_chart()})
  output$plot2 <- renderPlot({get_chart2()})
  
}

shinyApp(ui, server)