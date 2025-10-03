## A season forecast

library(LarsChill)
library(ncdf4)
library(CFtime)
library(tidyverse)
library(chillR)
library(dplyr)
library(patchwork)
library(lubridate)

# Download forecast for October 2024
forecast_Bonn <- download_seasonal_forecast(year = 2024,
                                            month = 10, 
                                            area = c(50.78, 7, 50.62, 7.2), 
                                            leadtime_hour = seq(0, 24*30*6, by = 6),
                                            fname = "bonn_forecast_oct_2024.nc",
                                            start_download = TRUE)

# load file
fname <- 'bonn_forecast_oct_2024.nc'

# Extract forecast
Bonn_forecast_oct_2024 <- extract_seasonal_forecast(fname, 50.62, 7)


# Download observed data from Jan 2024 to Sep 2024 to patch
long <- 7
lat <- 50.62

weather_dwd <- chillR::handle_dwd(action = 'list_stations', location = c(long, lat), 
                                  time_interval = c(20240101, 20240930))

data <- chillR::handle_dwd(action = "download_weather",
                           location = weather_dwd[1 : 25, "Station_ID"],
                           time_interval = c(20240101, 20240930),
                           stations_to_choose_from = 50,
                           station_list = weather_dwd,
                           drop_most = TRUE,
                           add.DATE = FALSE,
                           quiet = TRUE,
                           add_station_name = FALSE)

data_clean <- chillR::handle_dwd(data)

# Extract one station
data_observed <- data_clean[["Köln/Bonn"]]


# Make it to an hourly format
data_observed_hourly  <- stack_hourly_temps(data_observed, latitude = 50.62)

# Clean obeserved data format
data_observed_formatted <- data_observed_hourly$hourtemps %>%
  mutate(DATE = ymd(paste(Year, Month, Day))) %>%
  select(DATE, YEARMODA, Year, Month, Day, Hour, Tmin, Tmax, Temp)

## Find long-term mean of 30 years data

# Download 30 years temperature data
weather_dwd_mean <- chillR::handle_dwd(action = 'list_stations', location = c(long, lat), 
                                  time_interval = c(19940101, 20241231))

data_mean <- chillR::handle_dwd(action = "download_weather",
                           location = weather_dwd_mean[1 : 25, "Station_ID"],
                           time_interval = c(19940101, 20241231),
                           stations_to_choose_from = 50,
                           station_list = weather_dwd_mean,
                           drop_most = TRUE,
                           add.DATE = FALSE,
                           quiet = TRUE,
                           add_station_name = FALSE)

data_clean_mean <- chillR::handle_dwd(data_mean)

# Extract one station
data_observed_30year <- data_clean_mean[["Köln/Bonn"]]

# Find mean for 30 years

# Calculate 30-year long-term mean
longterm_mean <- data_observed_30year %>%
  group_by(Month, Day) %>%
  summarise(
    Tmin  = mean(Tmin,  na.rm = TRUE),
    Tmean = mean(Tmean, na.rm = TRUE),
    Tmax  = mean(Tmax,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!(Month == 2 & Day == 29)) %>%   # remove Feb 29
  mutate(Year   = 2025,
         YEARMODA = as.integer(sprintf("%04d%02d%02d", Year, Month, Day))) %>%
  select(YEARMODA, Year, Month, Day, Tmin, Tmax)
 

# Make it to an hourly format
data_observed_hourly_30year  <- stack_hourly_temps(longterm_mean, latitude = 50.62)

# Clean obeserved data format
data_observed_mean_formatted <- data_observed_hourly_30year$hourtemps %>%
  mutate(DATE = ymd(paste(Year, Month, Day))) %>%
  select(DATE, YEARMODA, Year, Month, Day, Hour, Tmin, Tmax, Temp)

# Patching with long-term mean
# mean_year <- 2025
# 
# if (max(Bonn_forecast_oct_2024$Year, na.rm = TRUE) < mean_year) {
#   
#   last_month <- tail(Bonn_forecast_oct_2024$Month, 1)
#   
#   patch_month <- data_observed_mean_formatted %>%
#     filter(Month > last_month) %>%
#     mutate(Year = max(Bonn_forecast_oct_2024$Year, na.rm = TRUE))
#   
#   forecast_mean_patched <- bind_rows(
#     Bonn_forecast_oct_2024,
#     patch_month,
#     data_observed_mean_formatted
#   )
#   
# } else {
#   
#   last_month <- tail(Bonn_forecast_oct_2024$Month, 1)
#   
#   patch_month <- data_observed_mean_formatted %>%
#     filter(Month > last_month) %>%
#     mutate(Year = max(Bonn_forecast_oct_2024$Year, na.rm = TRUE))
#   
#   forecast_mean_patched <- bind_rows(
#     Bonn_forecast_oct_2024,
#     patch_month)
# }


## Loop for 50 models

# Loop 
i <- 1:50
mean_year <- 2025

weather_combined <-  list()

for(i in unique(Bonn_forecast_oct_2024$model)){
  
  # Rename the column to remove error
  Bonn_forecast_2025 <- Bonn_forecast_oct_2024 %>%
    filter(model == i) %>%
    rename(Temp = temp)
  
  
  
  data_forecast <- Bonn_forecast_2025[Bonn_forecast_2025$model == i, ]
  
  # data_forecast_2025 <- data_forecast[data_forecast$Year == 2025, ]
  
  Bonn_forecast_2025_Oct_Mar_hourly <- interpolate_gaps_hourly(hourtemps = data_forecast,
                                                               latitude = 50.62,
                                                               daily_temps = NULL,
                                                               interpolate_remaining = TRUE,
                                                               return_extremes = FALSE,
                                                               minimum_values_for_solving = 4,
                                                               daily_patch_max_mean_bias = NA,
                                                               daily_patch_max_stdev_bias = NA)
  
  # Align format  
  data_forecast_model_clean <- Bonn_forecast_2025_Oct_Mar_hourly$weather %>%
    mutate(DATE = ymd(paste(Year, Month, Day))) %>%  
    mutate(YEARMODA = sprintf("%04d%02d%02d", Year, Month, Day)) %>%
    rename(Tmin = Tmin_source, Tmax = Tmax_source) %>%      
    mutate(
      Tmin = NA,                                        
      Tmax = NA,                                             
      Temp = Temp - 273.15                                    
    ) %>%
    select(DATE, YEARMODA, Year, Month, Day, Hour, Tmin, Tmax, Temp) 
  
  # Patch with long-term mean
  if (max(data_forecast_model_clean$Year, na.rm = TRUE) < mean_year) {
    
    last_month <- tail(data_forecast_model_clean$Month, 1)
    
    patch_month <- data_observed_mean_formatted %>%
      filter(Month > last_month) %>%
      mutate(Year = max(data_forecast_model_clean$Year, na.rm = TRUE))
    
    forecast_mean_patched <- rbind(
      data_forecast_model_clean,
      patch_month,
      data_observed_mean_formatted
    )
    
  } else {
    
    last_month <- tail(data_forecast_model_clean$Month, 1)
    
    patch_month <- data_observed_mean_formatted %>%
      filter(Month > last_month) %>%
      mutate(Year = max(data_forecast_model_clean$Year, na.rm = TRUE))
    
    forecast_mean_patched <- rbind(
      data_forecast_model_clean,
      patch_month)
  }
  
  
  
  phenoflex_2025_loop <- rbind(data_observed_formatted, forecast_mean_patched)
  
  #save it as list
  weather_combined[[i]] <-phenoflex_2025_loop
  
}

# Predict bloom dates for cherry blossom 
seasons <- 2025
bloom_dates <- list()

sea <- 1
for(i in seq_along(weather_combined)) {
  
  iSeason <- genSeason(weather_combined[[i]],
                       years = seasons)
  
  for (sea in 1:length(seasons))
  {season_data <- weather_combined[[i]][iSeason[[sea]], ]
  res <- PhenoFlex(temp = season_data$Temp,
                   times = c(1: length(season_data$Temp)),
                   A0 = 6193.884574,
                   A1 = 5.93991E+13,
                   E0 = 3372.80697,
                   E1 = 9900.312603,
                   slope = 1.064744846,
                   Tf = 1.681763589,
                   s1 = 0.110100121,
                   Tu = 23.72082621,
                   Tb = 1.757441439,
                   Tc = 39.93154563,
                   yc = 40.99954178,
                   Delta = 4,
                   Imodel = 0L,
                   zc = 180.8255616,
                   stopatzc = TRUE,
                   deg_celsius = TRUE,
                   basic_output = TRUE)
  
  
  bloom_dates[[i]] <- season_data$DATE[res[[sea]]]
  }
}

bloom_dates


# Convert list to vector of Date
bloom_vec <- as.Date(unlist(bloom_dates)) 

# Create data frame
predictions <- data.frame(
  Season = rep(2025, length(bloom_vec)),
  Prediction = bloom_vec
)


ggplot(predictions, aes(x = Prediction)) +
  geom_histogram(binwidth = 1, fill = "#69b3a2", color = "white") +
  xlab("Predicted bloom date") +
  ylab("Count") +
  theme_bw(base_size = 15)

# Show all dates
ggplot(predictions, aes(x = Prediction)) +
  geom_bar(fill = "#69b3a2", color = "white") +
  xlab("Predicted bloom date") +
  ylab("Count") +
  theme_bw(base_size = 15) +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # too messy

# Show bloom dates only
ggplot(predictions, aes(x = Prediction)) +
  geom_bar(fill = "#69b3a2", color = "white") +
  xlab("Predicted bloom date") +
  ylab("Count") +
  theme_bw(base_size = 15) +
  scale_x_date(
    breaks = sort(unique(predictions$Prediction)),
    date_labels = "%Y-%m-%d"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

