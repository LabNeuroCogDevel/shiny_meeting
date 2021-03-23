#!/usr/bin/env Rscript

# lab meeting shiny app
# 20191028WF - init
# 20191029JF - add dashboard stuff

# install.packages('shinydashboard')
# install.packages('RColorBrewer')

library(LNCDR)
if (! "pacman" %in% installed.packages()) install.packages("pacman")
library(pacman)
p_load(shiny)
p_load(lubridate)
p_load(dplyr)
p_load(ggplot2)
p_load(cowplot)
p_load(scales)
p_load(shinydashboard)
p_load(RColorBrewer)
p_load(stringr)
p_load(glue)
source('funcs.R')
source('plots.R')

### not used. just hardcode the two we want
studies <- db_query("select study from study")$study
vtypes <- unique(db_query("select * from visit_summary")$vtype)


# note file
get_week_notes <- function(note_date=today()) {
   # find file like
   # /Volumes/L/bea_res/Administrative/Lab Mtg Agenda and Minutes/labmeeting_20210323.md
   note_dir <- bea_res("Administrative/Lab Mtg Agenda and Minutes")
   ymd_date <- format(note_date, "%Y%m%d")

   mtg_file <- file.path(note_dir, glue("labmeeting_{ymd_date}.md"))
   if (! file.exists(mtg_file)) return(glue("missing file {mtg_file}"))
   includeMarkdown(mtg_file)
}
### create dashboard interface
## sidebar menu
sidebar <- dashboardSidebar(
  sidebarMenu(id = 'sidebarmenu',
    menuItem('Calendar', tabName = 'calendar', icon = icon('calendar')),
    menuItem('Study Progress', tabName = 'study_progress', icon = icon('signal')),
    menuItem('Notes', tabName = 'notes', icon = icon('clipboard')),

    dateRangeInput("daterange", label = "Date range:",
                   start = today() - days(7),
                   end   = today()),
    sliderInput('study_yr', label = 'Study year:', min = 1, max = 3, value = c(1, 3)),
    selectInput('study', label = 'Study:', choices = c('All' = '%', '7T' = 'BrainMechR01', 'PET' = 'PET'), selected = 'All'),
    selectInput('status', label = 'Status:', choices = c('Completed' = 'complete', 'Scheduled' = 'scheduled'), selected = 'Completed')
  ),
  collapsed = TRUE
)

## dashboard body (one tab per sidebar menu option)
body <- dashboardBody(
  tabItems(
    # calendar tab
    tabItem(tabName = 'calendar',
            
            # widgets                     
            fluidRow(
              valueBoxOutput('subject_widget', width = 3), # participants enrolled since 'lastweek'
              valueBoxOutput('behav_widget', width = 3), # behaviorals completed since 'lastweek'
              valueBoxOutput('scan_widget', width = 3),  # scans completed since 'lastweek'
              valueBoxOutput('eeg_widget', width = 3) # eegs completed since 'lastweek'
            ),

            # plots
            fluidRow(box(title = 'Calendar', width = 12, plotOutput('cal')))
            
            ),
    
    # study progress tab
    tabItem(tabName = 'study_progress',
            
            # plots
            fluidRow(box(title = 'Recruitment Progress', width = 12, plotOutput('hist'))),
            fluidRow(box(title = 'Recruitment Sources', width = 12, plotOutput('source')))
    ),
    
    # notes tab
    tabItem(tabName = 'notes',
            fluidRow(get_week_notes())
    )
    
  )
)


### ui
ui <- dashboardPage(
  dashboardHeader(title = 'LNCD Lab Meeting'),
  sidebar,
  body
)

# eg
#input <- list(study="BrainMechR01", daterange=c(lastweek, today() + days(7)))

### server
server <- function(input, output) {
 
## code for plots 
  output$cal <- renderPlot(calendar_plot(input))
  output$hist <- renderPlot(recruitment_hist(input))
  output$source <- renderPlot(source_hist(input))
 
## code for widgets
# get enrollment totals and print in val box
  subject_totals <- reactive(get_total(input))
  
  output$subject_widget <- renderValueBox({
    valueBox(
      paste0(subject_totals()),
      'Subjects "enrolled"',
      icon = icon('user', lib = 'font-awesome'),
      color = 'red'
    )
  })

# get behavioral totals and print in val box
  behav_totals <- reactive({
    
    if (input$study == '%') {
      study_filter <- list('BrainMechR01', 'PET')
    } else {
      study_filter <- input$study
    }
    
    behav_n <- get_data(input) %>%
      filter(vtype == 'Behavioral') %>%
      filter(study %in% study_filter) %>%
      summarize(n = n())
    behav_n[[1]]
    
  })

  output$behav_widget <- renderValueBox({
    valueBox(
      paste0(behav_totals()),
      'Behaviorals completed',
      icon = icon('cogs', lib = 'font-awesome'),
      color = 'blue'
    )
  }) 
 
# get scan totals and print in val box 
  scan_totals <- reactive({
    
    if (input$study == '%') {
      study_filter <- list('BrainMechR01', 'PET')
    } else {
      study_filter <- input$study
    }
    
    scan_n <- get_data(input) %>%
      filter(vtype == 'Scan') %>%
      filter(study %in% study_filter) %>%
      summarize(n = n())
    scan_n[[1]]
    
  })
  
  output$scan_widget <- renderValueBox({
    valueBox(
      paste0(scan_totals()),
      'Scans completed',
      icon = icon('magnet', lib = 'font-awesome'),
      color = 'purple'
    )
  }) 

# get EEG totals and print in val box 
  eeg_totals <- reactive({
    
    if (input$study == '%') {
      study_filter <- list('BrainMechR01', 'PET')
    } else {
      study_filter <- input$study
    }
    
    eeg_n <- get_data(input) %>%
      filter(vtype == 'eeg') %>%
      filter(study %in% study_filter) %>%
      summarize(n = n())
    eeg_n[[1]]
    
  })
  
  output$eeg_widget <- renderValueBox({
    valueBox(
      paste0(eeg_totals()),
      'EEGs completed',
      icon = icon('cloud', lib = 'font-awesome'),
      color = 'teal'
    )
  })
}

### run the application
## if you get an error because there are too many connections open, execute following line in console first:
# lapply(dbListConnections(drv = dbDriver("PostgreSQL")), function(x) {dbDisconnect(conn = x)})
shinyApp(ui = ui, server = server)

