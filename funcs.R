#!/usr/bin/env Rscript

# windows L: is /Volumes/L elsewhere
bea_res <- function(...) normalizePath(
            file.path(
             ifelse(.Platform$OS.type == "windows", 'L:','/Volumes/L'),
             'bea_res',
             ...))

get_data <- function(input) {
   
  qry <- sprintf("
    select * from visit_summary vs
    natural join enroll e
    natural join person p
    where study like '%s'
    and vtimestamp >= '%s'
    and vtimestamp <= '%s'
    and etype like 'LunaID'",
    input$study, input$daterange[1], input$daterange[2])
  d <- db_query(qry) %>%
     mutate(vdate=date(vtimestamp),
            #vtime=hm(format(vtimestamp, "%H:%M")),
            vtime=as.POSIXct(format(vtimestamp, "%H:%M"), format="%H:%M"),
            lbl=sprintf("%.0f%s(%s) - %.1f", age, sex, id, vscore))
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

