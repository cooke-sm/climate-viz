


rain_pal <- c('#BCD2E8',light_ribbon = '#91BAD6', heavy_ribbon = '#73A5C6', '#528AAE','#2E5984', main_line = '#1E3F66')
temp_pal <- c(main_line = '#B31313',heavy_ribbon = '#FF9000',light_ribbon = '#FDDA16','#FFEE82')



weatherdata <- read_csv("~/climate-viz/data/weatherdata.csv")

plot <- ribbon_plot(weatherdata, type = "temp")



plot + theme_minimal()


