theme_set(theme_minimal())

scatterplot <- weatherdata %>% 
  mutate(avg_temp = (temp_max+temp_min)/2) %>% 
  filter(month == 7) %>% 
  group_by(station) %>% 
  mutate(rain_z = scale(rain_mm),
         temp_z = scale(avg_temp)) %>% 
  ggplot(aes(x=rain_z, y=temp_z))+
  geom_hline(yintercept = 0, colour = rain_pal[3], linewidth = 1)+
  geom_vline(xintercept = 0, colour = rain_pal[3], linewidth = 1)+
  geom_point(data = . %>% filter(year != 2023), alpha = .2, colour = rain_pal[4])+
  geom_point(data = . %>% filter(year == 2023), colour = temp_pal[4], size = 2)+
  annotate(geom = "text", x=4, y=4, label = "Hotter and wetter", hjust = 1, colour = rain_pal[5])+
  annotate(geom = "text", x=-4, y=4, label = "Hotter and drier", hjust = 0, colour = rain_pal[5])+
  annotate(geom = "text", x=-4, y=-4, label = "Colder and drier", hjust = 0, colour = rain_pal[5])+
  annotate(geom = "text", x=4, y=-4, label = "Colder and wetter", hjust = 1, colour = rain_pal[5])+
  annotate(geom = "text", x=3, y=2.5, label = "Sheffield | 162mm rain", hjust = 1, colour = rain_pal[5])+
  geom_curve(data = . %>% filter(year == 2023 & station == "Sheffield"), 
             aes(x = 3.1, y= 2.4, xend = rain_z,yend = temp_z), 
             curvature = -.6, 
             colour = rain_pal[5])+
  coord_cartesian(xlim = c(-4,4),ylim = c(-4,4))+
  theme_climate()+
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank())


ggsave("scatter_plot.jpg", plot = scatterplot, device = 'jpg', dpi = 300)


weatherdata %>% 
  mutate(avg_temp = (temp_max+temp_min)/2) %>% 
  filter(month == 7) %>% 
  group_by(station) %>% 
  mutate(rain_z = scale(rain_mm),
         temp_z = scale(avg_temp)) %>% 
  ggplot(aes(x=rain_z, y=temp_z))+
  geom_point(alpha = .2)+
  geom_path(data = . %>% filter(year == 2023| year == 1970), aes(group = station),arrow = arrow(), colour = 'red')+
  coord_cartesian(xlim = c(-4,4),ylim = c(-4,4))
  
  
         
         