# install.packages('devtools')
# devtools::install_github('https://github.com/larscaspersen/addition_chillR')

library(LarsChill)
library(ncdf4)
library(CFtime)
library(tidyverse)
library(chillR)
library(dplyr)
library(patchwork)

# Download forecasts for phenoflex
# forecast_Bonn <- download_seasonal_forecast(year = 2025,
#                                             month = 1:5, 
#                                             area = c(60.1, 8, 60, 8.2), # increase grid size
#                                             leadtime_hour = seq(0, 24*7, by = 6), 
#                                             start_download = TRUE)


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

# fname <- 'season-forecast_dwd21_2m_temperature_2025_1-5_1_0-168_60.1-8-60-8.2.nc' #the new one

# Check the lat and lon of downloaded grid
nc <- nc_open(fname)

# Extract lat and lon to resolve grid error
ncvar_get(nc, "latitude")
ncvar_get(nc, "longitude")

# extract file
# The lat and lon became 60 and 8 and can't extract
# It seems I can only extract if the lat and lon are the same 
Bonn_forecast_2025_Jan_May <- extract_seasonal_forecast(file = fname, 
                                                        target_lat = 50.62,
                                                        target_lon = 7)

# write.csv(Bonn_forecast_2025_Jan_May, 'Bonn_forecast_2025_Jan_May.csv', row.names = FALSE)


# Download observed 
long <- 7
lat <- 50.62

weather_dwd <- chillR::handle_dwd(action = 'list_stations', location = c(long, lat), 
                                  time_interval = c(20240801, 20241231))


data <- chillR::handle_dwd(action = "download_weather",
                           location = weather_dwd[1 : 3, "Station_ID"],
                           time_interval = c(20240801, 20241231),
                           stations_to_choose_from = 50,
                           station_list = weather_dwd,
                           drop_most = TRUE,
                           add.DATE = FALSE,
                           quiet = TRUE,
                           add_station_name = FALSE)


data_clean <- chillR::handle_dwd(data)

# Extract one station
data_observed <- data_clean[["Königswinter-Heiderhof"]]

# Make it to an hourly format
data_observed_hourly  <- stack_hourly_temps(data_observed, latitude = 50.62)

# Clean obeserved data format
data_observed_formatted <- data_observed_hourly$hourtemps %>%
  mutate(DATE = ymd(paste(Year, Month, Day))) %>%
  select(DATE, YEARMODA, Year, Month, Day, Hour, Tmin, Tmax, Temp)

# Loop 
i <- 1:50

weather_combined <-  list()
# weather_df <- data.frame()


for(i in unique(Bonn_forecast_2025_Jan_May$model)){
  
  # Rename the column to remove error
  Bonn_forecast_2025 <- Bonn_forecast_2025_Jan_May %>%
    filter(model == i, Year == 2025) %>%
    rename(Temp = temp)
  
  
 
  data_forecast <- Bonn_forecast_2025[Bonn_forecast_2025$model == i, ]
  
  data_forecast_2025 <- data_forecast[data_forecast$Year == 2025, ]
  
  Bonn_forecast_2025_Jan_May_hourly <- interpolate_gaps_hourly(hourtemps = data_forecast_2025,
                                                               latitude = 50.62,
                                                               daily_temps = NULL,
                                                               interpolate_remaining = TRUE,
                                                               return_extremes = FALSE,
                                                               minimum_values_for_solving = 4,
                                                               daily_patch_max_mean_bias = NA,
                                                               daily_patch_max_stdev_bias = NA)

# Align format  
  data_forecast_model_clean <- Bonn_forecast_2025_Jan_May_hourly$weather %>%
    mutate(DATE = ymd(paste(Year, Month, Day))) %>%  
    mutate(YEARMODA = sprintf("%04d%02d%02d", Year, Month, Day)) %>%
    rename(Tmin = Tmin_source, Tmax = Tmax_source) %>%      
    mutate(
      Tmin = NA,                                        
      Tmax = NA,                                             
      Temp = Temp - 273.15                                    
    ) %>%
    select(DATE, YEARMODA, Year, Month, Day, Hour, Tmin, Tmax, Temp) 
  
  phenoflex_2025_loop <- rbind(data_observed_formatted, data_forecast_model_clean)
  
  #save it as list
  weather_combined[[i]] <-phenoflex_2025_loop
  
  #save as giant combined data.frame
  #weather_df <- rbind(weather_df, phenoflex_2025)
  
}

# View(weather_combined)

yc <- 20
zc <- 40
phenoflex_results <- list()

for(i in seq_along(weather_combined)) {

  model_data <- weather_combined[[i]]  
  
  iSeason <- genSeason(model_data, years = c(2025)) 
  
  season_data <- model_data[unlist(iSeason), ]  

  
  res <- PhenoFlex(
    temp = season_data$Temp,
    times = seq_along(season_data$Temp),
    zc = zc,
    stopatzc = TRUE,
    yc = yc,
    basic_output = FALSE
  )
  
  DBreakDay <- res$bloomindex
  
  seasontemps <- model_data[unlist(iSeason), ]
  seasontemps$x <- res$x
  seasontemps$y <- res$y
  seasontemps$z <- res$z
  seasontemps <- add_date(seasontemps)
  seasontemps$Date <- as.POSIXct(seasontemps$Date)
  
  CR_full <- seasontemps$Date[which(seasontemps$y >= yc)[1]]
  Bloom <- seasontemps$Date[which(seasontemps$z >= zc)[1]]

  if(DBreakDay == 0) {
    cat("No bloom detected for model:", names(weather_combined)[i], "\n")
    phenoflex_results[[i]] <- list(
      model = names(weather_combined)[i],  
      CR_full = NA,
      Bloom = NA,
      chill_plot = NULL,
      heat_plot = NULL
    )
    next 
  }
  

  
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
  
  
  phenoflex_results[[i]] <- list(
    model = names(weather_combined)[i],  
    CR_full = CR_full,
    Bloom = Bloom,
    chill_plot = chillplot,
    heat_plot = heatplot
  )
  
}

print(phenoflex_results[[1]]$chill_plot)
print(phenoflex_results[[1]]$heat_plot)
print(phenoflex_results[[2]]$chill_plot)
print(phenoflex_results[[2]]$heat_plot)
print(phenoflex_results[[3]]$chill_plot)
print(phenoflex_results[[3]]$heat_plot)
print(phenoflex_results[[4]]$chill_plot)
print(phenoflex_results[[4]]$heat_plot)
print(phenoflex_results[[5]]$chill_plot)
print(phenoflex_results[[5]]$heat_plot)


print(phenoflex_results[[50]]$chill_plot)
print(phenoflex_results[[50]]$heat_plot)



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
                 zc = zc,
                 stopatzc = TRUE,
                 yc = yc)

bloom_dates[[i]] <- season_data$DATE[res[[sea]]]

# if(sea == 1)
#   results <- season_data$DATE[res$bloomindex] else
#     results <- c(results,
#                  season_data$DATE[res$bloomindex])}
# 
# predictions <- data.frame(Season = seasons,
#                           Prediction = results)
# predictions$Prediction <-
#   ISOdate(2001,
#           substr(predictions$Prediction, 4, 5),
#           substr(predictions$Prediction, 1, 2))


}}

bloom_dates

a <- as.Date(bloom_dates[[1]])

typeof(bloom_dates[[1]])

typeof(a)
bloom_date <- ggplot(data = predictions,
                     aes(x = Season,
                         y = Prediction)) +
  geom_smooth() +
  geom_point() +
  ylab("Predicted bloom date") +
  theme_bw(base_size = 15)


