

ribbon_plot <- function(weatherdata, type = "temp", current_year = lubridate::year(Sys.time())){
  
  permitted_types <- c("temp", "rainfall")
  
  if(!(type %in% permitted_types)){
    cli::cli_abort(c(x = "{type} is not valid a valid type",
                     i = "Valid types are {.val {permitted_types}}"))
  }
  
  if(current_year > year(Sys.time())){
    cli::cli_alert(c(i = "current_year must be this year or a previous year. Setting year to {.val {year(Sys.time())}}"))
    current_year <- year(Sys.time())
  }
  
  #select the stations with enough data
  weatherdata %>% 
    group_by(station) %>% 
    summarise(min_year = min(year)) %>% 
    filter(min_year < 1940 & station != "Southampton" & station != "Ringway") -> facet_stations
  
  
  if(type == "temp"){
    plotdata <- weatherdata %>% mutate(temp = (temp_max+temp_min)/2) %>% 
      group_by(station,month) %>% 
      mutate(rib_avg = mean(temp, na.rm = TRUE),
             rib_sd = sd(temp, na.rm=TRUE)) %>% 
      ungroup() %>% 
      rename(line_data = temp)
    chartpal <- temp_pal
  } else if(type == "rainfall"){
    plotdata <- weatherdata %>% group_by(station,month) %>% 
      mutate(rib_avg = mean(rain_mm, na.rm = TRUE),
             rib_sd = sd(rain_mm, na.rm = TRUE)) %>% 
      ungroup() %>% 
      rename(line_data = rain_mm)
    chartpal <- rain_pal
  }
  

  plotdata %>% 
    filter(station %in% facet_stations$station) %>% 
    mutate(month = lubridate::month(month)) %>% 
    ggplot(aes(x=month, y=line_data, group = station))+
    geom_ribbon(aes(ymin = rib_avg - rib_sd, ymax = rib_avg + rib_sd), alpha = .5, fill = chartpal["heavy_ribbon"])+
    geom_ribbon(aes(ymin = rib_avg - 2*rib_sd, ymax = rib_avg + 2*rib_sd), alpha = .3, fill = chartpal["light_ribbon"])+
    geom_line(data = . %>% filter(year == current_year), aes(x=month, y=line_data), colour = chartpal["main_line"], size = 1.5) +
    scale_x_continuous(breaks = c(3,6,9,12), labels = \(x) lubridate::month(x,label = TRUE))+
    facet_wrap(~station, nrow = 3)
    
    
}

ribbon_plot(weatherdata, type = "rainfall")









