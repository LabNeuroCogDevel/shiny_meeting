# source('funcs.R') # for get_data and get_notes
#' @examples
#' calendar_plot(example_input(first_day=today()-days(100)) 
calendar_plot <- function(input) {
   d <- get_data(input) %>%
      group_by(vdate) %>%
      mutate(visit_in_day=rank(paste(vtype,vdate), ties.method="first"),
             color=paste(gsub('BrainMechR01','7T',study), vtype),
             lbl_newline=gsub("-", "\n", lbl)
      )

   # date_breaks <- "1 day"
   date_breaks <- as.Date(unique(d$vdate))
   if (length(date_breaks) > 10) {
      date_breaks <- waiver()
   }

   ggplot(d) +
      aes(x=vdate, y=paste(vtype, visit_in_day),
          fill=color, label=lbl_newline, group=id) +
      geom_tile(width=.8) +
      geom_line() +
      geom_label() +
      theme_cowplot() +
      theme(axis.text.x=element_text(angle=45, hjust=1),
            legend.position="bottom") +
      #theme_minimal() +
      scale_x_date(expand = expand_scale(add=1),
                   labels=date_format('%a %b %d'),
                   breaks=date_breaks) +
      labs(x = 'Visit date', y = '', color = 'Visit type') +
      scale_colour_brewer(palette = "Set1")
      #scale_y_datetime(labels=date_format("%I:%M %p"))
}
calendar_plot_label <- function(input) {
   d <- get_data(input) %>% mutate(color=paste(gsub('BrainMechR01','7T',study), vtype))
   ggplot(d) +
      aes(x=vdate, y=vtime, color=color, label=lbl, group=id) +
      geom_line(aes(color=NULL)) +
      geom_label() +
      theme_cowplot() +
      theme(axis.text.x=element_text(angle=45, hjust=1),
            legend.position="bottom") +
      #theme_minimal() +
      scale_x_date(expand = expand_scale(add=1), labels=date_format('%a %b %d')) +
      labs(x = 'Visit date', y = 'Visit time', color = 'Visit type') + 
      scale_colour_brewer(palette = 'Set1')
      #scale_y_datetime(labels=date_format("%I:%M %p"))
}
recruitment_hist <- function(input) {
    d <- get_data(input) %>% filter(!duplicated(id))
    #d <- get_enrollment(input) %>% filter(!duplicated(id))
    
    ggplot(d) +
      aes(x=age, fill=sex) +
      geom_histogram(position='dodge', binwidth = 1) +
      theme_cowplot() + facet_wrap(~study) +
      scale_fill_brewer(palette = 'Set1') +
      labs(x = 'Age', y = 'Count', fill = 'Sex') + 
      xlim(9, 35)
}

source_hist <- function(input){
    d <- get_data(input)
    ggplot(d) +
      aes(x=source) +
      geom_histogram(stat='count') +
      theme_cowplot() +
      facet_wrap(~study) +
      scale_fill_brewer(palette = 'Set1') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = 'Source', y = 'Count')
}
