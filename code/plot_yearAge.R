#Function to plot the age data by species and year.
#Saves figure to location 'output' as provided by user

library(gtable)
library(grid)

plot_yearAge <- function(spec, data, cat, output = NULL){
  
  #Enter in species and dataset
  #Output length-age plot by survey and area
  
  temp <- data %>% 
    dplyr::filter(grepl(spec, common_name))
  
  max_temp <- temp %>% 
    group_by(.data[[cat]]) %>%
    summarize("max" = max(age, na.rm = TRUE),
              "yearmax" = year[which.max(age)])
  
  #Match x axis between the two plots
  common_limits_year <- c(min(temp$year)-1, max(temp$year)+1)
  
  p1 <- ggplot(temp, aes(x = year, y = age, fill = .data[[cat]])) + 
    geom_bar(position="stack", stat="identity") +
    scale_x_continuous(limits = common_limits_year)  +
    labs(title = spec,
         x = "",
         y = "Number of Ages")
  
  p2 <- ggplot(temp, aes(x = year, y = age, colour = .data[[cat]])) + 
    geom_point(alpha = 0.3, size = 1)  +
    geom_point(data = max_temp, 
               mapping = aes(x = yearmax, y = max, colour = .data[[cat]]), size = 3) +
    scale_x_continuous(limits = common_limits_year) +
    labs(x = "Year",
         y = "Age (years)")
  
  #Match the margins between the two plots
  g1 <- ggplotGrob(p1)
  g2 <- ggplotGrob(p2)
  g3 <- rbind(g1, g2, size = "first")
  g3$widths <- grid::unit.pmax(g1$widths, g2$widths)
  grid.newpage()
  grid.draw(g3)
  
  if(!is.null(output)) {
    png(output, width = 6, height = 4, units = "in", res = 300)
    grid.newpage()
    grid.draw(g3)
    dev.off()
  }
  
}

