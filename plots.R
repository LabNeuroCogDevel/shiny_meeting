# source('funcs.R') # for get_data and get_notes
calendar_plot <- function(input) {
   d <- get_data(input) %>% mutate(color=paste(gsub('BrainMechR01','7T',study), vtype))
   ggplot(d) +
      aes(x=vdate, y=vtime, color=color, label=lbl, group=id) +
      geom_line(aes(color=NULL)) +
      geom_label() +
      theme_cowplot() +
      theme(axis.text.x=element_text(angle=45, hjust=1),
            legend.position="bottom") +
      #theme_minimal() +
      scale_x_date(expand = expand_scale(add=1), labels=date_format('%a %b %d'))
      #scale_y_datetime(labels=date_format("%I:%M %p"))
}
recruitment_hist <- function(input) {
   d <- get_data(input) %>% filter(!duplicated(id))
  
   ggplot(d) +
      aes(x=age, fill=sex) +
      geom_histogram(position='dodge') +
      theme_cowplot() + facet_wrap(~study)
}

source_hist <- function(input){
   d <- get_data(input) %>% filter(!duplicated(id))
   ggplot(d) +
      aes(x=`source`, fill=study) +
      geom_histogram(stat='count', position='dodge') +
      scale_fill_brewer('Accent') +
      theme_cowplot()
}
