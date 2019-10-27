#' Visualize Airport Delays
#' 
#' This function plots the Mean Delay Time of Flights in USA Airports.
#' The function does not take any parameters.
#' 
#' @return The function returns a graph.
#' 
#' @import dplyr
#' @import ggplot2
#' @import nycflights13
#' @import maps
#' @name visualize_airport_delays
#' @export

visualize_airport_delays = function(){
  
  delay_mean = flights %>%
    na.omit() %>%
    group_by(dest) %>%
    summarise(mean_delay = mean(dep_delay)) %>%
    rename(airport = dest)
  
  airport_location = airports %>%
    na.omit() %>%
    select(faa, lon, lat) %>%
    group_by(faa) %>%
    rename(airport = faa)
  
  airport_data = as.data.frame(inner_join(airport_location, delay_mean, by = "airport"))
  
  usa = map_data("usa")
  
  plot_map = ggplot() + 
    geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = "#adabaa") +
    labs(x = "", y = "", title = "Mean Delay Time of Flights in USA Airports") +
    geom_point(data = airport_data, aes(x=lon, y = lat, color = mean_delay), size = 2, alpha = 1) +
    scale_color_gradientn("Mean Delay\nTime (m)", colors = c("#69d9f2", "#565187")) +
    coord_equal(ratio = 1.5)
  
  return(plot_map)
}

