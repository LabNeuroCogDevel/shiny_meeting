#!/usr/bin/env Rscript

# lab meeting shiny app
# 20191028WF - init


library(LNCDR)
library(shiny)
library(lubridate)
library(dplyr)
library(ggplot2)
library(cowplot)
library(scales)

source('funcs.R') # get_data, get_notes
source('plots.R') # calendar_plot, source_hist, recruitment_hist


# not used. just hardcode the two we want
studies <- db_query("select study from study")$study
lastweek <- today() - days(7)
nextweek <- today() + days(7)
# eg
input <- list(study="BrainMechR01", daterange=c(lastweek, nextweek), note="%")

# outline
ui<-fluidPage(
 titlePanel("Lab Meeting"),
 sidebarLayout(
  sidebarPanel(
      selectInput("study", "Study", choices=c("BrainMechR01", "PET", "%"), selected="%"),
      dateRangeInput("daterange", "Dates", start=lastweek, end=nextweek),
      textInput("Notes", "Notes", value="%" )),
 mainPanel(
           plotOutput("cal"),
           plotOutput("hist"),
           plotOutput("source")
           )))


server <- function(input, output) {
   output$cal <- renderPlot({calendar_plot(input) })
   output$hist <- renderPlot({recruitment_hist(input)})
   output$source <- renderPlot({source_hist(input) })
}

# see 'run' file
shinyApp(ui, server=server)
#runApp(Sys.getcwd())
#as.Date(input$plot_click$x, origin = "1970-01-01")
