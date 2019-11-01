#!/usr/bin/env Rscript

# windows L: is /Volumes/L elsewhere
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
                 and visitno >= %s
                 and visitno <= %s
                 and etype like 'LunaID'",
                 input$study, input$daterange[1], input$daterange[2], input$study_yr[1], input$study_yr[2])
  d <- db_query(qry) %>%
    mutate(vdate=date(vtimestamp),
           #vtime=hm(format(vtimestamp, "%H:%M")),
           vtime=as.POSIXct(format(vtimestamp, "%H:%M"), format="%H:%M"),
           lbl=sprintf("x%s %s - %.0f%s(%s) - %s - %.1f", visitno, str_to_title(vtype), age, sex, id, ra, vscore))
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
  enroll_d <- db_query(enroll_qry) %>%
    mutate(enrolldate=date(edate))
}

get_total <- function(input) {
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
    
}

get_notes <- function(input){
 
qry <- sprintf("
    with visit_notes as (
      select pid, vid, string_agg(note, ';;;') as vnotes from note group by vid
    ),
    subj_only as (
      select pid, string_agg(note, ';;;') as snotes from note group by pid where vid is null
    ),
    notes as (
      select pid, vid 
    )
    
    left join visit_notes vn on vs.vid=vn.vd")
}

