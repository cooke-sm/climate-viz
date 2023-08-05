library(tidyverse)
library(showtext)


font_add_google(name = "Rubik", family = "rubik") # if you need to install the font
showtext_auto()

rain_pal <- c('#BCD2E8',light_ribbon = '#91BAD6', heavy_ribbon = '#73A5C6', '#528AAE','#2E5984', main_line = '#1E3F66')
temp_pal <- c(main_line = '#B31313',heavy_ribbon = '#FF9000',light_ribbon = '#FDDA16','#FFEE82')
temp_pal <- c(main_line = "#C12600", "#DA2B27", heavy_ribbon =  "#EA5A3E", light_ribbon = "#F68656", "#F0BA70")



weatherdata <- read_csv("~/climate-viz/data/weatherdata.csv")

plot <- ribbon_plot(weatherdata %>% filter(!station %in% c('Eskdalemuir', 'Lerwick', 'Wick')), type = "rainfall")


theme_climate <- function(){
  theme_minimal()+
  theme(text = element_text(family = 'Rubik', colour = rain_pal["main_line"]),
        line = element_line(colour = rain_pal["main_line"]),
        strip.text = element_text(hjust = 0, colour = rain_pal["main_line"]),
        axis.line.x = element_line(),
        )
}


plot+theme_climate()+labs(title = "Title")
plot2 <- ribbon_plot(weatherdata %>% filter(!station %in% c('Eskdalemuir', 'Lerwick', 'Wick')), type = "temp")

plot2+theme_climate()
