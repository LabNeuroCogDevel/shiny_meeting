#!/usr/bin/env Rscript

# lab meeting shiny app
# 20191028WF - init
# 20191029JF - add dashboard stuff
# 20210323WF - resurrect



if (! "LNCDR" %in% installed.packages()) 
   remotes::install_github('LabNeuroCogDevel/LNCDR')
library(LNCDR)
# we are sure to have pacman if coming from './run'
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
p_load(DT)
source('funcs.R')
source('plots.R')

# how to show the google calendar
gcal_iframe <- function() {
   gcalurl <- 'https://calendar.google.com/calendar/u/0/embed?src=lunalncd@gmail.com&ctz=America/New_York&mode=WEEK'
   HTML(glue('<iframe src="{gcalurl}" style="border: 0" width="800" height="600" frameborder="0" scrolling="no"></iframe>'))
}

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
    menuItem('Visits', tabName = 'visits', icon = icon('calendar')),
    menuItem('Study Progress', tabName = 'study_progress', icon = icon('signal')),
    menuItem('Notes', tabName = 'notes', icon = icon('clipboard')),
    menuItem('GCal', tabName = 'gcal', icon = icon('calendar')),

    # see funcs::example_input
    dateRangeInput("daterange", label = "Date range:",
                   start = today() - days(7),
                   end   = today()),
    sliderInput('study_yr', label = 'Study year:', value = c(1, 3),
                min = 1, max = 3,),
    selectInput('study', label = 'Study:', selected = 'All',
                choices = c('All' = '%', 'Habit' = 'Habit%', '7T' = 'BrainMechR01', 'PET' = 'PET')),
    selectInput('status', label = 'Status:', selected = 'Completed',
                choices = c('Completed' = 'complete', 'Scheduled' = 'scheduled'))
  ),
  collapsed = TRUE
)

## dashboard body (one tab per sidebar menu option)
body <- dashboardBody(
  tabItems(
    # calendar visits tab
    tabItem(tabName = 'visits',
            # widgets                     
            fluidRow(
              valueBoxOutput('subject_widget', width = 3), # participants enrolled since 'lastweek'
              valueBoxOutput('behav_widget', width = 3), # behaviorals completed since 'lastweek'
              valueBoxOutput('scan_widget', width = 3),  # scans completed since 'lastweek'
              valueBoxOutput('eeg_widget', width = 3) # eegs completed since 'lastweek'
            ),

            # plots
            fluidRow(box(title = 'Calendar', width = 12,
                         plotOutput('visit_cal', click="visit_cal_click"))),
            fluidRow(box(title="Visit Notes", width = 12, tableOutput('visit_info'))),
            fluidRow(box(title="Subj Notes", width = 12, dataTableOutput('subj_notes')))
            ),
    
    # study progress tab
    tabItem(tabName = 'study_progress',
            
            # plots
            fluidRow(box(title = 'Recruitment Progress', width = 12, plotOutput('hist'))),
            fluidRow(box(title = 'Recruitment Sources', width = 12, plotOutput('source')))
    ),
    
    tabItem(tabName = 'notes', fluidRow(get_week_notes())),
    tabItem(tabName="gcal", fluidRow(gcal_iframe()))
))



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
 
  # is not updated unless input changes
  visit_data <- reactive({get_visit_data(input)})
     #bindCache(input$daterange, input$study, input$study_yr)
  # TODO:  do the same for recruitment

  ## plots
  output$visit_cal  <- renderPlot(visit_plot(visit_data()))
  #output$visit_cal  <- renderPlot(calendar_plot_label(input))
  output$visit_info <- renderTable(notes_at_click(visit_data(), input$visit_cal_click))
  output$hist       <- renderPlot(recruitment_hist(input))
  output$source     <- renderPlot(source_hist(input))
 
  ## widgets
  # get enrollment totals and print in val box
  output$subject_widget <- renderValueBox({
    valueBox(
      paste0(get_total(visit_data(), input$study)),
      "Subjects visited",
      icon = icon('user', lib = 'font-awesome'),
      color = 'red'
    )
  })

# get behavioral totals and print in val box
  output$behav_widget <- renderValueBox({
    valueBox(
      total_visit_of_type(visit_data(), "Behavioral", input$study),
      'Behaviorals completed',
      icon = icon('cogs', lib = 'font-awesome'),
      color = 'blue')})
  output$scan_widget <- renderValueBox({
    valueBox(
      total_visit_of_type(visit_data(), "Scan", input$study),
      'Scans completed',
      icon = icon('magnet', lib = 'font-awesome'),
      color = 'purple'
    )})
  output$eeg_widget <- renderValueBox({
    valueBox(
             {total_visit_of_type(visit_data(), "eeg", input$study)},
      'EEGs completed',
      icon = icon('cloud', lib = 'font-awesome'),
      color = 'teal'
    )
  })  

  # interactive plot: click on calendar -> bring up notes
  output$subj_notes <- renderDataTable({get_notes_at(input)})
  # output$visit_info <- renderTable(notes_at_click(visit_data(), input$visit_cal_click))
}

### run the application
## if you get an error because there are too many connections open, execute following line in console first:
# lapply(dbListConnections(drv = dbDriver("PostgreSQL")), function(x) {dbDisconnect(conn = x)})
app <- shinyApp(ui = ui, server = server)
# runApp(app, host="0.0.0.0", port=1555)

