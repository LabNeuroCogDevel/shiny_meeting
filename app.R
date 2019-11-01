#!/usr/bin/env Rscript

# lab meeting shiny app
# 20191028WF - init
# 20191029JF - add dashboard stuff

# install.packages('shinydashboard')
# install.packages('RColorBrewer')

library(LNCDR)
library(shiny)
library(lubridate)
library(dplyr)
library(ggplot2)
library(cowplot)
library(scales)
library(shinydashboard)
library(RColorBrewer)
library(stringr)
con <- lncd_pgconn()

### not used. just hardcode the two we want
studies <- db_query("select study from study", con)$study
vtypes <- unique(db_query("select * from visit_summary", con)$vtype)
lastweek <- today() - days(7)
nextweek <- today() + days(7)

bea_res <- function(...) normalizePath(
  file.path(
    ifelse(.Platform$OS.type == "windows", 'L:','/Volumes/L'),
    'bea_res',
    ...))

get_data <- function(input) {
  qry <- sprintf("
                 select * from visit_summary
                 natural join person
                 natural join enroll
                 where study like '%s'
                 and vtimestamp >= '%s'
                 and vtimestamp <= '%s'
                 and etype like 'LunaID'",
                 input$study, input$daterange[1], input$daterange[2])
  d <- db_query(qry, con) %>%
    mutate(vdate=date(vtimestamp),
           #vtime=hm(format(vtimestamp, "%H:%M")),
           vtime=as.POSIXct(format(vtimestamp, "%H:%M"), format="%H:%M"),
           lbl=sprintf("%s - %.0f%s(%s) - %.1f", str_to_title(vtype), age, sex, id, vscore))
}

get_enrollment <- function(input) {
  enroll_qry <- sprintf("
                 select * from visit_summary
                 natural join person
                 natural join enroll
                 where study like '%s'
                 and edate >= '%s'
                 and edate <= '%s'
                 and etype like 'LunaID'",
                 input$study, input$daterange[1], input$daterange[2])
  enroll_d <- db_query(enroll_qry, con) %>%
    mutate(enrolldate=date(edate))
}

# note file
mtg_date <- lapply(strsplit(as.character(today()), '-'), FUN = function(x){paste(x[1], x[2], x[3], sep = '')})
mtg_file <- paste(paste('labmeeting', mtg_date, sep = '_'), 'md', sep = '.')
bea_res_mtg_file <- file.path(bea_res('Administrative/Lab Mtg Agenda and Minutes'), paste(paste('labmeeting', mtg_date, sep = '_'), 'md', sep = '.'))

### create dashboard interface
## sidebar menu
sidebar <- dashboardSidebar(
  sidebarMenu(id = 'sidebarmenu',
    menuItem('Calendar', tabName = 'calendar', icon = icon('calendar')),
    menuItem('Study Progress', tabName = 'study_progress', icon = icon('signal')),
    menuItem('Notes', tabName = 'notes', icon = icon('clipboard')),

    dateRangeInput('daterange', label = 'Date range:', start = lastweek, end = today()),    
    selectInput('study', label = 'Study:', choices = c('All' = '%', '7T' = 'BrainMechR01', 'PET' = 'PET'), selected = 'All'),
    selectInput('study_yr', label = 'Study year:', choices = c('All' = '%', '1' = '1', '2' = '2', '3' = '3'), selected = 'All'),
    selectInput('status', label = 'Status:', choices = c('Completed' = 'finished', 'Scheduled' = 'scheduled'), selected = 'Completed')
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
            fluidRow(includeMarkdown(bea_res_mtg_file))
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
#input <- list(study="BrainMechR01", daterange=c(lastweek, nextweek))

### server
server <- function(input, output) {
 
## code for plots 
  output$cal <- renderPlot({
    d <- get_data(input)
    
    ggplot(d) +
      aes(x=vdate, y=vtime, color=paste(study, vtype), label=lbl, group=id) +
      geom_line(aes(color=NULL)) +
      geom_label(show.legend = TRUE) +
      theme_cowplot() +
      theme(axis.text.x=element_text(angle=45, hjust=1),
           legend.position="bottom") +
      #theme_minimal() +
      scale_x_date(expand = expand_scale(add=1), labels=date_format('%a %b %d')) +
      labs(x = 'Visit date', y = 'Visit time', color = 'Visit type') + 
      scale_colour_brewer(palette = 'Set1')
    #scale_y_datetime(labels=date_format("%I:%M %p"))
    
  })
  
  output$hist <- renderPlot({
    d <- get_data(input) %>% filter(!duplicated(id))
    #d <- get_enrollment(input) %>% filter(!duplicated(id))
    
    ggplot(d) +
      aes(x=age, fill=sex) +
      geom_histogram(position='dodge', binwidth = 1) +
      theme_cowplot() + facet_wrap(~study) +
      scale_fill_brewer(palette = 'Set1') +
      labs(x = 'Age', y = 'Count', fill = 'Sex') + 
      xlim(9, 35)
  })
  
  output$source <- renderPlot({
    d <- get_data(input)
    
    ggplot(d) +
      aes(x=source) +
      geom_histogram(stat='count') +
      theme_cowplot() +
      facet_wrap(~study) +
      scale_fill_brewer(palette = 'Set1') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = 'Source', y = 'Count')
  })
 
## code for widgets
# get enrollment totals and print in val box
  subject_totals <- reactive({
    
    if (input$study == '%') {
      study_filter <- list('BrainMechR01', 'PET')
    } else {
      study_filter <- input$study
    }
    
    sub_n <- get_data(input) %>% 
    #sub_n <- get_enrollment(input) %>%
      filter(!duplicated(id)) %>%
      filter(study %in% study_filter) %>%
      summarize(n = n())
    sub_n[[1]]
    
  })
  
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

