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
con <- lncd_pgconn()

# not used. just hardcode the two we want
studies <- db_query("select study from study", con)$study
lastweek <- today() - days(7)
nextweek <- today() + days(7)

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
            lbl=sprintf("%.0f%s(%s) - %.1f", age, sex, id, vscore))
}

# outline
ui<-fluidPage(
 titlePanel("recruitment"),
 sidebarLayout(
  sidebarPanel(
      selectInput("study", label="study", choices=c("BrainMechR01", "PET", "%"), selected="%"),
      dateRangeInput("daterange", label="date", start=lastweek, end=nextweek),
      selectInput("metric", label="metric", choices=c("scheduled", "finished"), selected="finished")),
 mainPanel(
           plotOutput("cal"),
           plotOutput("hist"),
           plotOutput("source")
           )))
# eg
input <- list(study="BrainMechR01", daterange=c(lastweek, nextweek))


server <- function(input, output) {
   addClass(selector = "body", class = "sidebar-collapse")
   output$cal <- renderPlot({
      d <- get_data(input)
      ggplot(d) +
         aes(x=vdate, y=vtime, color=paste(vtype, study), label=lbl, group=id) +
         geom_line(aes(color=NULL)) +
         geom_label() +
         theme_cowplot() +
         theme(axis.text.x=element_text(angle=45, hjust=1),
               legend.position="bottom") +
         #theme_minimal() +
         scale_x_date(expand = expand_scale(add=1), labels=date_format('%a %b %d'))
         #scale_y_datetime(labels=date_format("%I:%M %p"))

   })
   output$hist <- renderPlot({
      d <- get_data(input) %>% filter(!duplicated(id))

      ggplot(d) +
         aes(x=age, fill=sex) +
         geom_histogram(position='dodge') +
         theme_cowplot() + facet_wrap(~study)
   })

   output$source <- renderPlot({
      ggplot(d) +
         aes(x=`source`) +
         geom_histogram(stat='count') +
         theme_cowplot() +
         facet_wrap(~study)
   })
}
print("LOADED")

#shinyApp(ui, server=server)
# runApp(Sys.getcwd())
#as.Date(input$plot_click$x, origin = "1970-01-01")
