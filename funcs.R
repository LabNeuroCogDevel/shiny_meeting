#!/usr/bin/env Rscript

# windows L: is /Volumes/L elsewhere
bea_res <- function(...) normalizePath(
            file.path(
             ifelse(.Platform$OS.type == "windows", 'L:','/Volumes/L/bea_res'),
             # 'bea_res', # 20191112 - bea_res no longer part of path, 20191216: put bea_res back
             ...))

#
example_input <- function(first_day=lubridate::today()-lubridate::days(7),
                          last_day=lubridate::today(),
                          min_yr=1, max_yr=3,
                          study="%",
                          status="complete") {
    input <- list(
      daterange = c(first_day, last_day),
      study_yr  = c(min_yr, max_yr),
      study= study,
      status = status)
}

# get data if there are no results
# column names are hard coded b/c empty query results does not have them
get_visit_data_empty <- function() {
  db_cols <- c("pid", "vid", "vtimestamp", "vtype", "vscore", "visitno",
               "googleuri", "age", "study", "cohort", "action", "ra",
               "dur_hr", "notes", "dvisit", "dperson", "fname", "lname",
               "dob", "sex", "hand", "nick", "adddate", "source", "eid",
               "etype", "id", "edate")
  d <- as.data.frame(t(rep(NA, length(db_cols))))
  names(d) <- db_cols
  return(d[0,])
}

# @param date_column = 'edate' or 'adddate' for enrollment
get_visit_data <- function(input, date_column="vtimestamp") {
  # need these. empty query returns empty dataframe
  df_empty <- get_visit_data_empty()
  db_cols_str <- paste(collapse=",", names(df_empty))

  qry <- glue("select {db_cols_str} from visit_summary
               natural join person
               natural join enroll
               where study like '{input$study}'
               and {date_column} >= '{input$daterange[1]}'
               and {date_column} <= '{input$daterange[2]}'
               and etype like 'LunaID'")

  # if we are looking for visits we can restrict by visit number
  visit_no <- glue(" and (
                 (visitno >= {input$study_yr[1]} and
                  visitno <= {input$study_yr[2]})
                 or visitno is NULL)")
  if (date_column %in% c('vtimestamp')) qry <-paste0(qry, visit_no)

  res <- db_query(qry)
  if (nrow(res) <= 0L) {
     cat("query: \n")
     print(gsub(" \\+|\\n"," ", qry))
     cat("no results!\n")
     return(df_empty)
   }

  d <- res %>%
    mutate(vdate=date(vtimestamp),
           #vtime=hm(format(vtimestamp, "%H:%M")),
           enrolldate=date(edate),
           vtime=as.POSIXct(format(vtimestamp, "%H:%M"), format="%H:%M"),
           lbl=sprintf("x%s %s - %.0f%s(%s) - %s - %.1f", visitno, str_to_title(vtype), age, sex, id, ra, vscore))  %>%
      group_by(vdate) %>%
      # specific to plotting
      mutate(visit_in_day=rank(paste(vtype,vdate), ties.method="first"),
             color=paste(gsub('BrainMechR01','7T',study), vtype),
             lbl_newline=gsub("-", "\n", lbl),
             vtype_visit_in_day=as.factor(paste(vtype, visit_in_day))
      )

   return(d)
}

get_enrollment <- function(input) get_visit_data(input, 'edate')

# used for widget counts
visit_subset <- function(visit_data, only_vtype, study="%") {
   only_type <- visit_data %>%
      filter(grepl(only_vtype, vtype, ignore.case=T))
  if (study != "%") only_type <- only_type %>% filter(study %in% study)
  return(only_type)
}
total_visit_of_type <- function(...) visit_subset(...) %>% nrow

get_total <- function(visit_data, study) {
   visits <- visit_subset(visit_data, only_vtype=".*", study)
   length(unique(visits$id))
}

get_notes <- function(pid){
 
qry <- sprintf("
    with visit_notes as (
      select pid, vid, string_agg(note, ';;;') as vnotes from note group by vid
    ),
    subj_only as (
      select pid, string_agg(note, ';;;') as snotes from note group by pid where vid is null
    ),
    select * from subj_only
    left join visit_notes vn on vs.vid=vn.vd")
}

get_notes_at <- function(input) {
   click_pos <- input$cal_click
   d <- get_visit_data(input)
   #with(d, LNCDR::qassertm(pid='i',vdate='s',vtime='s'))
   d_pnt <- nearPoints(d, click_pos, xvar="vdate", yvar="vtime")
   #if(is.null(d_pnt$pid)) { warning("no pid in clicked", d_pnt); return()}
   qry <- sprintf("select * from note
                  natural join enroll
                  where pid in (%s) and
                  etype like 'LunaID'
                  order by ndate", paste(collapse=',', d_pnt$pid))

   print(qry)
   db_query(qry)

}
notes_at_click <- function(visit_data, click){
  # exday <- ymd('2021-03-17')
  # visit_data <- get_visit_data(example_input(first_day=exday-days(1), last_day=exday+days(1)))
  # click <- list(y=1,x=as.numeric(exday))

  if (is.null(click$x)) return(data.frame(error="no click"))
  if (nrow(visit_data)<=0L) return(data.frame(error="no visit data"))
  lvls <- levels(visit_data$vtype_visit_in_day)
  y <- lvls[round(click$y)]
  x <- as.Date(round(click$x), origin="1970-01-01")
  clicked <- visit_data %>% filter(vtype_visit_in_day == y, vdate==x)
  if (nrow(clicked)<=0L) return(data.frame(error=glue("bad click {x}, {y}")))
  qry <- glue("select note.ndate, note.note, visit.vtimestamp
              from note
              natural left join visit
              where pid = {clicked$pid[1]}")
  db_query(qry)
}
