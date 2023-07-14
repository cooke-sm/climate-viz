library(tidyverse)


colour <- c("#BCD2E8",'#91BAD6', '#73A5C6', '#528AAE','#2E5984', '#1E3F66')
heat <- c('#B31313','#FF9000','#FDDA16','#FFEE82')

theme_set(theme_minimal())
theme_replace(text = element_text(family = "Rubik", size = 36, colour = colour[5]),
              strip.text = element_text(hjust = 0))

weatherdata %>% 
  group_by(station) %>% 
  summarise(min_year = min(year)) %>% 
  filter(min_year < 1940 & station != "Southampton" & station != "Ringway") -> facet_stations


#temperature
weatherdata %>% 
  filter(station %in% facet_stations$station) %>% 
  mutate(temp = (temp_max+temp_min)/2) %>% 
  group_by(station,month) %>% 
  mutate(temp_avg = mean(temp, na.rm = TRUE),
         temp_sd = sd(temp, na.rm=TRUE)) %>% 
  ungroup() %>% 
  ggplot(aes(x=month, y=temp_avg))+
  geom_ribbon(aes(ymin = temp_avg - temp_sd, ymax = temp_avg + temp_sd), alpha = .3, fill = heat[2])+
  geom_ribbon(aes(ymin = temp_avg - 2*temp_sd, ymax = temp_avg + 2*temp_sd), alpha = .1, fill = heat[2])+
  geom_line(data = . %>% filter(year == '2022'), aes(x=month, y=temp), colour = heat[1], size = .5, alpha = .5) +
  facet_wrap(~station, nrow = 3)+
  labs(title = "How does 2022 compare to previous years? (Temperature)",
       subtitle = "Red line is 2022. Ribbon shows 95% of the average range",
       caption = "@CookeComms | Met Office Weather Station Data")+
  ylab("Monthly average temp (deg C)")+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) ->plot1

#rainfall
weatherdata %>% 
  filter(station %in% facet_stations$station) %>% 
  group_by(station,month) %>% 
  mutate(rain_avg = mean(rain_mm, na.rm = TRUE),
         rain_sd = sd(rain_mm, na.rm = TRUE)) %>% 
  ungroup() %>% 
  ggplot(aes(x=month, y=rain_mm))+
  geom_ribbon(aes(ymin = rain_avg - rain_sd, ymax = rain_avg + rain_sd), alpha = .3, fill = colour[4])+
  geom_ribbon(aes(ymin = rain_avg - 2*rain_sd, ymax = rain_avg + 2*rain_sd), alpha = .1, fill = colour[4])+
  geom_line(data = . %>% filter(year == '2022'), aes(x=month, y=rain_mm), colour = colour[6], size = .6, alpha = .5) +
  facet_wrap(~station, nrow = 3)+
  labs(title = "How does 2022 compare to previous years? (Rainfall)",
       subtitle = "Blue line is 2022. Ribbon shows 95% of the average range",
       caption = "@CookeComms | Met Office Weather Station Data")+
  ylab("Monthly average rainfall (mm)")+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())->plot2


ggsave("temp_facets.png", plot1, device = "png", height = 1500, width = 2600, units = 'px', dpi = 320, bg = "white")  
ggsave("rain_facets.png", plot2, device = "png", height = 1500, width = 2600, units = 'px', dpi = 320, bg = "white")  
