#' -----------------------------------------------------------------------------
#' WEBINAR STATS (V2)
#' 
#' A Shiny app to explore statistics following an online webinar
#' -----------------------------------------------------------------------------

# libraries --------------------------------------------------------------------
library(shiny)                    # shiny core
library(shinydashboard)           # layout and display functions
library(shinyTime)                # allows for a time input widget
library(here)                     # localised file references
library(plotly)                   # interactive charts
library(ggsurvfit)                # time-to-event plots
library(gtsummary)                # summarise survival stats
library(gt)                       # render gt outputs in shiny

#devtools::install_github("The-Strategy-Unit/StrategyUnitTheme")
library(StrategyUnitTheme)        # corporate colours

## user-defined functions ----
source(here('functions_script.R'))

# ui ---------------------------------------------------------------------------
ui <- dashboardPage(
  skin = 'yellow',
  
  ## header --------------------------------------------------------------------
  dashboardHeader(
    title = 'Webinar Stats'
    # title = tags$a(
    #   href='http://mycompanyishere.com',
    #   tags$img(src='tsu_logo_yellow_screen_transparent.png'),
    #   width = 10
    # )
  ),
  
  
  # dashboardPage(
  #   dashboardHeader(title = tags$a(href='http://mycompanyishere.com',
  #                                  tags$img(src='logo.png')))
    
  
  
  ## sidebar -------------------------------------------------------------------
  dashboardSidebar(
    sidebarMenu(
      
      ### import ---
      menuItem(
        text = 'Import',
        tabName = 'file_import',
        icon = icon('file-import')
      ),
      
      ### stats ---
      menuItem(
        text = 'Statistics',
        tabName = 'statistics',
        icon = icon('table')
      ),
      
      ### survival ---
      menuItem(
        text = 'Survival analysis',
        tabName = 'survival',
        icon = icon('stopwatch-20')
      )
    )
  ),
  
  ## body -----------------------------------------------------------------------
  dashboardBody(
    
    tags$img(
      src = 'tsu_logo_yellow_screen_transparent.png',
      align = 'right',
      height = 70
    ),

    tabItems(
      ### import ---------------------------------------------------------------
      tabItem(
        tabName = 'file_import',
        
        ### title and instructions
        fluidRow(
          column(
            width = 12,
            h1('File import'),
            'Import your attendance file and configure start and end times',
          )
        ),
        
        ### import configuration box ---
        fluidRow(
          
          #### file --
          box(
            title = 'File import',
            width = 6,
            fileInput(
              inputId = 'upload',
              label = 'Select the csv file containing the attendance data for your event:',
              accept = '.csv'
            )
          ),
          
          #### date --
          box(
            title = 'Date',
            width = 2,
            
            dateInput(
              inputId = 'live_event_date',
              label = 'Event date'
            )
          ),
          
          #### time --
          box(
            title = 'Time',
            width = 4,
            
            column(
              width = 6,
              timeInput(
                inputId = 'start',
                label = 'Start time',
                value = strptime('09:00:00', '%T'),
                minute.steps = 15
              )
            ),
            
            column(
              width = 6,
              timeInput(
                inputId = 'end',
                label = 'End time',
                value = strptime('17:00:00', '%T'),
                minute.steps = 15
              )
            )
          )
          
        ),
        
        ### import feedback chart ---
        fluidRow(
          plotlyOutput('file_import_calibration_chart')
        )
      ), # import end -
      
      ### statistics -----------------------------------------------------------
      tabItem(
        tabName = 'statistics',
        
        ### title and instructions
        fluidRow(
          column(
            width = 12,
            h1('Statistics'),
          )
        ),
        
        fluidRow(
          column(width = 12, 'Here are some interesting facts about your event:'),
          valueBoxOutput(outputId = 'number_of_attendees', width = 6),
          valueBoxOutput(outputId = 'averagetime', width = 6),
        ),
        
        fluidRow(
          column(width = 12, 'Timeliness of attendances'),
          valueBoxOutput(outputId = 'attend_less_than_15', width = 4),
          valueBoxOutput(outputId = 'attend_more_than_45', width = 4),
          valueBoxOutput(outputId = 'joined_after_15mins', width = 4),
        ),
        
        fluidRow(
          column(width = 12, 'How people attended'),
          valueBoxOutput(outputId = 'attended_on_computer', width = 6),
          valueBoxOutput(outputId = 'attended_on_mobile', width = 6),
        )

      ), # statistics end -
      
      ### survival -------------------------------------------------------------
      tabItem(
        tabName = 'survival',
        
        ### title and instructions
        fluidRow(
          column(
            width = 12,
            h1('Survival analysis')
          )
        ),
        
        ### tabset of different survival analyses
        fluidRow(
          
          tabsetPanel(
            type = 'tabs',
            
            tabPanel(
              title = 'Overall',
              plotOutput(outputId = 'survival_plot_overall'),
              gt_output(outputId = 'survival_table_overall_time'),
              gt_output(outputId = 'survival_table_overall_probs')
            ),
            
            tabPanel(
              title = 'Device',
              plotOutput(outputId = 'survival_plot_device'),
              gt_output(outputId = 'survival_table_device_time'),
              gt_output(outputId = 'survival_table_device_probs')
            ),
            
            tabPanel(
              title = 'Promptness',
              plotOutput(outputId = 'survival_plot_promptness'),
              gt_output(outputId = 'survival_table_promptness_time'),
              gt_output(outputId = 'survival_table_promptness_probs')
            )
          ),
        ),
        
        fluidRow(
          box(
            title = 'About', 
            width = 12,
            tags$p('This analysis looks at the expected duration until attendees leave the meeting.'),
            tags$p('"Events" are people leaving the meeting and attendees are "Censored" where their leaving time is unknown.'),
            tags$a(href='https://towardsdatascience.com/what-is-survival-analysis-examples-by-hand-and-in-r-3f0870c3203f', 'Learn more about survival analysis', target = '_blank')
          )
        )
        
        
      ) # survival end -
      
      
    )
  )
)


# server -----------------------------------------------------------------------
server <- function(input, output) {
  

  
  
  # import ---------------------------------------------------------------------
  # read the imported file
  df <- reactive({
    
    # require a file to be uploaded before doing this
    req(input$upload)
    
    # work out how to read the file
    ext <- tools::file_ext(input$upload$name)
    switch(
      ext,
      csv = vroom(input$upload$datapath, delim = ',', col_types = cols(col_character())),
      tsv = vroom(input$upload$datapath, delim = '\t', col_types = cols(col_character())),
      validate('Invalid file; please upload a .csv or .tsv file')
    ) |> 
      # process the file
      process_imported_file()
  })
  
  ## estimate start / end dates -------------------------------------------------
  observeEvent(input$upload, {
    
    # require a file to be uploaded and processed
    req(df)
    
    # update the ui with estimated start and end datetimes
    updateDateInput(
      inputId = 'live_event_date',
      value = median(df()$joined_datetime_rounded, na.rm = T)
    )
    updateTimeInput(
      session = getDefaultReactiveDomain(),
      inputId = 'start',
      value = median(df()$joined_datetime_rounded, na.rm = T)
    )
    updateTimeInput(
      session = getDefaultReactiveDomain(),
      inputId = 'end',
      value = quantile(df()$left_datetime_rounded, probs = 0.75, na.rm = T)
    )
  })
  
  ## file import calibration chart ---------------------------------------------
  # get attendee data
  df_attendees <- reactive({
    req(df(), input$live_event_date, input$start, input$end)
    
    df() |> 
      get_event_attendees(
        event_start_datetime = get_event_datetime(input$live_event_date, input$start),
        event_end_datetime = get_event_datetime(input$live_event_date, input$end)
      )
  })
  
  # get count of attendees per minute
  df_attendees_per_minute <- reactive({
    req(df_attendees(), input$live_event_date, input$start, input$end)
    
    df_attendees() |> 
      get_attandance_count_per_minute(
        start = get_event_datetime(input$live_event_date, input$start),
        end = get_event_datetime(input$live_event_date, input$end)
      )

  })
  
  # render the chart of attendees per minute
  output$file_import_calibration_chart <- renderPlotly({
    req(df_attendees_per_minute())

    plot_ly(
      data = df_attendees_per_minute(),
      x = ~minute,
      y = ~attendees
    ) |>
      add_bars(marker = list(color = 'rgb(222,163,59)')) |> 
      layout(
        title = 'Number of attendees each minute',
        bargap = 0.05,
        xaxis = list(title = 'Time'),
        yaxis = list(title = 'Number of attendees'),
        font = list(family = 'Segoe UI, Helvetica, Arial, sans-serif')
      )
  })
  
  
  # statistics -----------------------------------------------------------------
  # get a tibble with calculated statistics
  df_statistics <- reactive({
    req(df_attendees())
    calculate_attendance_statistic(df_attendees = df_attendees())
  })
  
  # render stats boxes
  output$averagetime <- renderValueBox({
    valueBox(
      value = df_statistics()$mean_duration,
      subtitle = 'Average time attending the event',
      icon = icon('clock'),
      color = 'yellow'
    )
  })
    
  output$number_of_attendees <- renderValueBox({
    valueBox(
      value = df_statistics()$number_of_attendees,
      subtitle = 'Attended the event',
      icon = icon('people-group'),
      color = 'yellow'
    )
  })
    
  output$attend_less_than_15 <- renderValueBox({
    valueBox(
      value = df_statistics()$attend_less_than_15,
      subtitle = 'Attended for less than 15 minutes',
      icon = icon('stopwatch'),
      color = 'green'
    )
  })
    
  output$attend_more_than_45 <- renderValueBox({
    valueBox(
      value = df_statistics()$attend_more_than_45,
      subtitle = 'Attended for more than 45 minutes',
      icon = icon('people-line'),
      color = 'green'
    )
  })
    
  output$joined_after_15mins <- renderValueBox({
    valueBox(
      value = df_statistics()$joined_after_15mins,
      subtitle = 'Joined after 15 minutes',
      icon = icon('right-to-bracket'),
      color = 'green'
    )
  })
    
  output$attended_on_computer <- renderValueBox({
    valueBox(
      value = df_statistics()$attended_on_computer,
      subtitle = 'Attended using a computer or laptop',
      icon = icon('display'),
      color = 'purple'
    )
  })
    
  output$attended_on_mobile <- renderValueBox({
    valueBox(
      value = df_statistics()$attended_on_mobile,
      subtitle = 'Attended on a mobile device',
      icon = icon('mobile-screen-button'),
      color = 'purple'
    )
  })
  
  # survival -------------------------------------------------------------------
  # modify the df to work with survival analysis
  df_survival <- reactive({
    req(df_attendees())
    
    df_attendees() |> 
      mutate(
        # defines the end point of the meeting
        event_end = quantile(left_datetime, probs = 0.75), 
        start_time_plus15 = add_minutes(min(attendance_start_datetime), 15),
        start_time_minus5 = add_minutes(min(attendance_start_datetime), -5),
        # censor attendees for whom we don't know their left time
        status = case_when(
          is.na(left_datetime) ~ 0,
          TRUE ~ 1
        ),
        # convert output variables to factors
        os_formfactor = factor(os_formfactor),
        flag_promptness = case_when(
          joined_datetime > start_time_plus15 ~ 'Joined 15+ mins late',
          joined_datetime < start_time_minus5 ~ 'Joined 5+ mins early',
          TRUE ~ 'Joined on time'
        ),
        flag_promptness = factor(flag_promptness),
      )
    
  })
  
  ## overall --
  output$survival_plot_overall <- renderPlot({
    req(df_survival())
    
    # simple survival curve
    surv_overall <- survfit2(
      Surv(attendance_duration, status) ~ 1, data = df_survival()
    )
    
    surv_overall |> 
      ggsurvfit(color = su_theme_cols('orange')) +
      add_confidence_interval(fill = su_theme_cols('orange')) +
      add_risktable() +
      scale_ggsurvfit() + 
      labs(
        title = 'Survival analysis for all attendees',
        x = 'Minutes'
      )
  })
  
  output$survival_table_overall_time <- render_gt({
    req(df_survival())
    
    # simple survival curve
    surv_overall <- survfit2(
      Surv(attendance_duration, status) ~ 1, data = df_survival()
    )
    
    # what is the maximum attendance duration (rounded to nearest 15 mins)
    temp_mins_max <- ceiling(max(df_survival()$attendance_duration, na.rm = T)/15)*15
    
    surv_overall |> 
      tbl_survfit(
        times = c((temp_mins_max/4), (temp_mins_max/2), ((temp_mins_max/4)*3)),
        label_header = '{time} minutes'
      ) |> 
      as_gt()
  })
  
  output$survival_table_overall_probs <- render_gt({
    req(df_survival())
    
    # simple survival curve
    surv_overall <- survfit2(
      Surv(attendance_duration, status) ~ 1, data = df_survival()
    )
    
    surv_overall |> 
      tbl_survfit(
        probs = c(0.5),
        label_header = 'Median survival time in mins (95% CI)'
      ) |> 
      as_gt()
  })
  
  ## device --
  output$survival_plot_device <- renderPlot({
    req(df_survival())
    
    # simple survival curve
    surv_overall <- survfit2(
      Surv(attendance_duration, status) ~ os_formfactor, data = df_survival()
    )
    
    surv_overall |> 
      ggsurvfit() +
      add_confidence_interval() +
      add_risktable() +
      scale_ggsurvfit() + 
      labs(
        title = 'Survival analysis for all attendees, split by device',
        x = 'Minutes'
      )
  })

  output$survival_table_device_time <- render_gt({
    req(df_survival())
    
    # simple survival curve
    surv_overall <- survfit2(
      Surv(attendance_duration, status) ~ os_formfactor, data = df_survival()
    )
    
    # what is the maximum attendance duration (rounded to nearest 15 mins)
    temp_mins_max <- ceiling(max(df_survival()$attendance_duration, na.rm = T)/15)*15
    
    surv_overall |> 
      tbl_survfit(
        times = c((temp_mins_max/4), (temp_mins_max/2), ((temp_mins_max/4)*3)),
        label = list(os_formfactor ~ 'Device type'),
        label_header = '{time} minutes',
        
      ) |> 
      as_gt()
  })
  
  output$survival_table_device_probs <- render_gt({
    req(df_survival())
    
    # simple survival curve
    surv_overall <- survfit2(
      Surv(attendance_duration, status) ~ os_formfactor, data = df_survival()
    )
    
    surv_overall |> 
      tbl_survfit(
        probs = c(0.5),
        label = list(os_formfactor ~ 'Device type'),
        label_header = 'Median survival time in mins (95% CI)'
      ) |> 
      as_gt()
  })
  
  ## promptness
#   'survival_plot_promptness'),
# gt_output(outputId = 'survival_table_promptness_time'),
# gt_output(outputId = 'survival_table_promptness_probs')
  ## device --
  output$survival_plot_promptness <- renderPlot({
    req(df_survival())
    
    # simple survival curve
    surv_overall <- survfit2(
      Surv(attendance_duration, status) ~ flag_promptness, data = df_survival()
    )
    
    surv_overall |> 
      ggsurvfit() +
      add_confidence_interval() +
      add_risktable() +
      scale_ggsurvfit() + 
      labs(
        title = 'Survival analysis for all attendees, split by promptness',
        x = 'Minutes'
      )
  })
  
  output$survival_table_promptness_time <- render_gt({
    req(df_survival())
    
    # simple survival curve
    surv_overall <- survfit2(
      Surv(attendance_duration, status) ~ flag_promptness, data = df_survival()
    )
    
    # what is the maximum attendance duration (rounded to nearest 15 mins)
    temp_mins_max <- ceiling(max(df_survival()$attendance_duration, na.rm = T)/15)*15
    
    surv_overall |> 
      tbl_survfit(
        times = c((temp_mins_max/4), (temp_mins_max/2), ((temp_mins_max/4)*3)),
        label = list(flag_promptness ~ 'Promptness'),
        label_header = '{time} minutes',
        
      ) |> 
      as_gt()
  })
  
  output$survival_table_promptness_probs <- render_gt({
    req(df_survival())
    
    # simple survival curve
    surv_overall <- survfit2(
      Surv(attendance_duration, status) ~ flag_promptness, data = df_survival()
    )
    
    surv_overall |> 
      tbl_survfit(
        probs = c(0.5),
        label = list(flag_promptness ~ 'Promptness'),
        label_header = 'Median survival time in mins (95% CI)'
      ) |> 
      as_gt()
  })
}

# run --------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
