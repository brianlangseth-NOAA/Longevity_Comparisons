#Function to plot the age and length data by species.
#Saves figure to location 'output' as provided by user

plot_lengthAge <- function(spec, data, output = NULL){
  
  #Enter in species and dataset
  #Output length-age plot by survey and area
  
  temp <- data %>% 
    dplyr::filter(grepl(spec, common_name))
  
  max_temp <- temp %>% 
    group_by(survey_id) %>%
    summarize("max" = max(age, na.rm = TRUE))
  
  ggplot(temp, aes(x = age, y = length_cm, colour = survey_id)) + 
    geom_point(alpha = 0.3, size = 1) +
    geom_segment(data = max_temp, mapping = aes(x = max, xend = max, 
                                                y = 0, yend = 5,
                                                colour = survey_id), lwd=1) +
    guides(colour = guide_legend(override.aes = list(linetype = 0))) +
    labs(title = spec)
    
  if(!is.null(output)) {
    ggsave(output, width = 6, height = 4, units = "in")
  }
  
}