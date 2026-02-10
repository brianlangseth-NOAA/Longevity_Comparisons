#Function to plot the age and length data by species.
#Saves figure to location 'output' as provided by user
#User supplied category variable (cat) used for plotting

plot_lengthAge <- function(spec, data, cat, output = NULL){
  
  #Enter in species and dataset
  #Output length-age plot by survey and area
  
  temp <- data %>% 
    dplyr::filter(grepl(spec, common_name))
  
  max_temp <- temp %>% 
    group_by(.data[[cat]]) %>%
    summarize("max" = max(age, na.rm = TRUE))
  
  ggplot(temp, aes(x = age, y = length_cm, colour = .data[[cat]])) + 
    geom_point(alpha = 0.3, size = 1) +
    geom_segment(data = max_temp, mapping = aes(x = max, xend = max, 
                                                y = 0, yend = 5,
                                                colour = .data[[cat]]), lwd=1) +
    geom_point(data = max_temp, 
               mapping = aes(x = max, y = -1, colour = .data[[cat]]), size = 1) + 
    guides(colour = guide_legend(override.aes = list(linetype = 0))) +
    labs(title = spec)
    
  if(!is.null(output)) {
    ggsave(output, width = 6, height = 4, units = "in")
  }
  
}