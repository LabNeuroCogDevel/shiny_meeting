# source('funcs.R') # for get_visit_data and get_notes
visit_plot <- function(visit_data) {
   # date_breaks <- "1 day"
   date_breaks <- as.Date(unique(visit_data$vdate))
   if (length(date_breaks) > 10) {
      date_breaks <- waiver()
   }

   ggplot(visit_data) +
      aes(x=vdate, y=vtype_visit_in_day,
          fill=color, label=lbl_newline, group=id) +
      geom_tile(width=0.5) +
      geom_line() +
      geom_label() +
      theme_cowplot() +
      theme(axis.text.x=element_text(angle=45, hjust=1),
            legend.position="bottom") +
      #theme_minimal() +
      scale_x_date(expand = expansion(add=1),
                   labels=date_format('%a %b %d'),
                   breaks=date_breaks) +
      labs(x = 'Visit date', y = '', color = 'Visit type') +
      scale_colour_brewer(palette = "Set1")
      #scale_y_datetime(labels=date_format("%I:%M %p"))
}

#' @examples
#' calendar_plot_label(example_input(first_day=today()-days(100))
calendar_plot_label <- function(input) {
   d <- get_visit_data(input) %>% mutate(color=paste(gsub('BrainMechR01','7T',study), vtype))

   ggplot(d) +
      aes(x=vdate, y=time_rank, fill=color, label=lbl, group=id) +
      geom_tile() +
      geom_line(aes(color=NULL)) +
      geom_label() +
      theme_cowplot() +
      theme(axis.text.x=element_text(angle=45, hjust=1),
            legend.position="bottom") +
      scale_x_date(expand = expansion(add=1), labels=date_format('%a %b %d')) +
      labs(x = 'Visit date', y = 'Visit time', color = 'Visit type') + 
      scale_colour_brewer(palette = 'Set1')
      #scale_y_datetime(labels=date_format("%I:%M %p"))
}

recruitment_hist <- function(input) {
    #d <- get_visit_data(input) %>% filter(!duplicated(id))
    d <- get_enrollment(input) %>% filter(!duplicated(id))

    rplot <- ggplot(d) +
      aes(x=age, fill=sex) +
      geom_histogram(position='dodge', binwidth = 1) +
      theme_cowplot() +
      labs(x = 'Age', y = 'Count', fill = 'Sex') + 
      xlim(9, 35)

   if (nrow(d) > 0L)
      rplot <- rplot + facet_wrap(~study) +
         scale_fill_brewer(palette = 'Set1')

   return(rplot)
}

source_hist <- function(input){
   #d <- get_visit_data(input)
   d <- get_enrollment(input)
   shist <-  ggplot(d) +
      aes(x=source) +
      geom_histogram(stat='count') +
      theme_cowplot() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = 'Source', y = 'Count')

   if (nrow(d) > 0L)
      shist <- shist +
      facet_wrap(~study) +
      scale_fill_brewer(palette = 'Set1')

   return(shist)
}
