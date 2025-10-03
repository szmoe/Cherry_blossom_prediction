# install.packages('devtools')
# devtools::install_github('https://github.com/larscaspersen/addition_chillR')

library(LarsChill)
library(ncdf4)
library(CFtime)
library(tidyverse)
library(chillR)
library(dplyr)

# Download forecasts for phenoflex
forecast_Bonn <- download_seasonal_forecast(year = 2025,
                                            month = 1:5, 
                                            area = c(50.78, 7, 50.62, 7.2),
                                            leadtime_hour = seq(0, 24*7, by = 6), 
                                            start_download = TRUE)


# By the way, you can also download the file later if you don't want to be stuck 
# in your R session. If you set start_download = FALSE and later re-run the 
# download function. Here is an example:
# request <- LarsChill::download_seasonal_forecast(year = c('2025'),
#                                                  month = 1:5, 
#                                                  area = c(50.78, 7, 50.62, 7.2),
#                                                  leadtime_hour = seq(0, 24*7, by = 6), 
#                                                  start_download = FALSE)
# 
# download <- LarsChill::download_seasonal_forecast(year = c('2025'),
#                                                   month = 1:5, 
#                                                   area = c(50.78, 7, 50.62, 7.2),
#                                                   leadtime_hour = seq(0, 24*7, by = 6), 
#                                                   request_env = request)

fname <- 'season-forecast_dwd21_2m_temperature_2025_1-5_1_0-168_50.78-7-50.62-7.2.nc'

# Check the lat and lon of downloaded grid
nc <- nc_open(fname)

# Extract lat and lon to resolve grid error
ncvar_get(nc, "latitude")
ncvar_get(nc, "longitude")

# extract file
Bonn_forecast_2025_Jan_May <- extract_seasonal_forecast(file = fname, 
                                                        target_lat = 50.62,
                                                        target_lon = 7)

write.csv(Bonn_forecast_2025_Jan_May, 'Bonn_forecast_2025_Jan_May.csv', row.names = FALSE)

# Load the csv 
Bonn_forecast_2025_Jan_May <- read_csv("Bonn_forecast_2025_Jan_May.csv")

# Extract the first model of the forecast data
data_forecast <- Bonn_forecast_2025_Jan_May[Bonn_forecast_2025_Jan_May$model == 1, ]

# We only want the forecast from Jan to May 2025
data_forecast_2025 <- data_forecast[data_forecast$Year == 2025, ]
  

## there is a function in chillR, that can help us with the task to bring the 
## sub-daily weather forecast (every six hours a datapoint) to an hourly format. 
## chillR::interpolate_gaps_hourly() can help us there (alternatively to aggregating 
## the data into daily Tmin and Tmax and then generating hourly temperautres based on 
## daily extremes). You need to set the argument minimum_values_for_solving = 4 , 
## instead of 5 (default).

# Error: File is missing one of the following columns: Year, Month, Day, Hour, Temp
# Check column name
colnames(data_forecast_2025)

# Change temp to Temp
data_forecast_2025 <- data_forecast_2025 %>%
  rename(Temp = temp)
# colnames(data_forecast_2025)[colnames(data_forecast_2025) == "temp"] <- "Temp"

colnames(data_forecast_2025)

# bring the sub-daily weather forecast (every six hours a datapoint) to an hourly format
Bonn_forecast_2025_Jan_May_hourly <- interpolate_gaps_hourly(hourtemps = data_forecast_2025,
                                                            latitude = 50.62,
                                                            daily_temps = NULL,
                                                            interpolate_remaining = TRUE,
                                                            return_extremes = FALSE,
                                                            minimum_values_for_solving = 4,
                                                            daily_patch_max_mean_bias = NA,
                                                            daily_patch_max_stdev_bias = NA
                                                              )
# Save csv for later use
write.csv(Bonn_forecast_2025_Jan_May_hourly$weather, 'data/Bonn_forecast_2025_hourly_model1.csv',
          row.names = FALSE)

# Download observed 
long <- 7
lat <- 50.62

weather_dwd <- chillR::handle_dwd(action = 'list_stations', location = c(long, lat), 
                                  time_interval = c(20240801, 20241231))

View(weather_dwd)
nrow(weather_dwd)
data <- chillR::handle_dwd(action = "download_weather",
                           location = weather_dwd[1 : 3, "Station_ID"],
                           time_interval = c(20240801, 20241231),
                           stations_to_choose_from = 50,
                           station_list = weather_dwd,
                           drop_most = TRUE,
                           add.DATE = FALSE,
                           quiet = TRUE,
                           add_station_name = FALSE)

View(data)

data_clean <- chillR::handle_dwd(data)
View(data_clean)

# Extract one station
data_observed <- data_clean[["Königswinter-Heiderhof"]]

# Make it to an hourly format
data_observed_hourly  <- stack_hourly_temps(data_observed, latitude = 50.62)

# Clean obeserved data format
data_observed_formatted <- data_observed_hourly$hourtemps %>%
  mutate(DATE = ymd(paste(Year, Month, Day))) %>%
  select(DATE, YEARMODA, Year, Month, Day, Hour, Tmin, Tmax, Temp)

# Clean forecast format
data_forecast_model1 <- read.csv("data/Bonn_forecast_2025_hourly_model1.csv")

data_forecast_model1_clean <- data_forecast_model1 %>%
  mutate(DATE = ymd(paste(Year, Month, Day))) %>%  
  mutate(YEARMODA = sprintf("%04d%02d%02d", Year, Month, Day)) %>%
  rename(Tmin = Tmin_source, Tmax = Tmax_source) %>%      
  mutate(
    Tmin = NA,                                        
    Tmax = NA,                                             
    Temp = Temp - 273.15                                    
  ) %>%
  select(DATE, YEARMODA, Year, Month, Day, Hour, Tmin, Tmax, Temp)  


# Merge observed and forecast
phenoflex_2025 <- rbind(data_observed_formatted, data_forecast_model1_clean)

# Save for later use 
write.csv(phenoflex_2025, 'data/phenoflex_2025.csv', row.names = FALSE)

# Feed into phenoflex
# Not sure if it is 40 and 190-- try with this first
yc <- 40
zc <- 190

iSeason <- genSeason(phenoflex_2025,
                     years = c(2025))

season_data <- phenoflex_2025[iSeason[[1]],]

res <- PhenoFlex(temp = season_data$Temp,
                 times = c(1: length(season_data$Temp)),
                 zc = zc,
                 stopatzc = TRUE,
                 yc = yc,
                 basic_output = FALSE)

DBreakDay <- res$bloomindex
seasontemps <- phenoflex_2025[iSeason[[1]],]
seasontemps[,"x"] <- res$x
seasontemps[,"y"] <- res$y
seasontemps[,"z"] <- res$z
seasontemps <- add_date(seasontemps)

CR_full <- seasontemps$Date[which(seasontemps$y >= yc)[1]]
Bloom <- seasontemps$Date[which(seasontemps$z >= zc)[1]]

chillplot <- ggplot(data = seasontemps[1:DBreakDay,],
                    aes(x = Date,
                        y = y)) +
  geom_line(col = "blue",
            lwd = 1.5) +
  theme_bw(base_size = 20) +
  geom_hline(yintercept = yc,
             lty = 2,
             col = "blue",
             lwd = 1.2) +
  geom_vline(xintercept = CR_full,
             lty = 3,
             col = "blue",
             lwd = 1.2) +
  ylab("Chill accumulation (y)") +
  labs(title = "Chilling") +
  annotate("text",
           label = "Chill req. (yc)", 
           x = ISOdate(2025,01,01),
           y = yc*1.1,
           col = "blue",
           size = 5)

heatplot <- ggplot(data = seasontemps[1:DBreakDay,],
                   aes(x = Date,
                       y = z)) +
  geom_line(col = "red",
            lwd = 1.5) +
  theme_bw(base_size = 20) +
  scale_y_continuous(position = "right") +
  geom_hline(yintercept = zc,
             lty = 2,
             col = "red",
             lwd = 1.2) +
  geom_vline(xintercept = CR_full,
             lty = 3,
             col = "blue",
             lwd = 1.2) +
  geom_vline(xintercept = Bloom,
             lty = 3,
             col = "red",
             lwd = 1.2) +
  ylab("Heat accumulation (z)") +
  labs(title = "Forcing") +
  annotate("text",
           label = "Heat req. (zc)", 
           x = ISOdate(2025,01,01),
           y = zc*0.95,
           col = "red",
           size = 5)


library(patchwork)
chillplot + heatplot

yc <- 40
zc <- 60
seasons <- 2025

iSeason <- genSeason(phenoflex_2025,
                     years = seasons)
for (sea in 1:length(seasons))
{season_data <- phenoflex_2025[iSeason[[sea]], ]
res <- PhenoFlex(temp = season_data$Temp,
                 times = c(1: length(season_data$Temp)),
                 zc = zc,
                 stopatzc = TRUE,
                 yc = yc,
                 basic_output = FALSE)
if(sea == 1)
  results <- season_data$DATE[res$bloomindex] else
    results <- c(results,
                 season_data$DATE[res$bloomindex])}

predictions <- data.frame(Season = seasons,
                          Prediction = results)
predictions$Prediction <-
  ISOdate(2001,
          substr(predictions$Prediction, 4, 5),
          substr(predictions$Prediction, 1, 2))

ggplot(data = predictions,
       aes(x = Season,
           y = Prediction)) +
  geom_smooth() +
  geom_point() +
  ylab("Predicted bloom date") +
  theme_bw(base_size = 15)

