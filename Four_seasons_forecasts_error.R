library(LarsChill)
library(ncdf4)
library(CFtime)
library(tidyverse)
library(chillR)
library(dplyr)
library(patchwork)
library(lubridate)
library(purrr)

## Bloom prediction for four seasons

# Previously, in a season forecast, I found the last downloaded month ended on 28th. 
# So, a few days were missing when I did the patching. In order to avoid it,
# I will patch first and stack hourly temp at the last step.

month <- c(10, 11, 12, 1, 2, 3, 4) 
area <- c(50.78, 7, 50.62, 7.2)
year <- c(2020, 2021, 2022, 2023)

target_lat <- 50.62
target_lon <- 7
Bonn_forecast <- vector("list", length(year))
max_pos <- which.max(month) 

# Download forecast for a season of each year

# For the year 2020, DWD only has data for november and december. 
# So, when this happens, we don't want to stop the download but move to the next.

for (j in seq_along(year)) {
  
  for(i in seq_along(month)) { 
  
    fname <- sprintf("forecast_bonn_%d%02d.nc", 
                     ifelse(i <= max_pos, year[j], year[j] + 1), 
                     month[i])
    
    # Attempt download
    success <- tryCatch({
      download_seasonal_forecast(
        year = ifelse(i <= max_pos, year[j], year[j] + 1),
        month = month[i],
        area = area,
        leadtime_hour = seq(0, 24*30*6, by = 6),
        fname = fname,
        start_download = TRUE
      )
      TRUE   # mark as success
    }, error = function(e) {
      message("Download failed for ", fname, ": ", e$message)
      FALSE  # mark as failure
    })
    
    if (!success) {
      next  # move to next month/year
    }
  
  # Extract forecast for successful downloads
  Bonn_forecast_season <- extract_seasonal_forecast(fname,
                                                    target_lat = target_lat,
                                                    target_lon = target_lon)
  Bonn_forecast[[j]][[i]] <- Bonn_forecast_season
  
  }
}

# Save RDS
saveRDS(Bonn_forecast, "data/Bonn_forecast_four_seasons.rds")
Bonn_forecast <- readRDS("data/Bonn_forecast_four_seasons.rds")
str(Bonn_forecast)


# Download observed data for comparison
long <- 7
lat <- 50.62
time_interval = c(20200101, 20241231)

weather_dwd <- chillR::handle_dwd(action = 'list_stations', location = c(long, lat), 
                                  time_interval = time_interval)

data <- chillR::handle_dwd(action = "download_weather",
                           location = weather_dwd[1 : 25, "Station_ID"],
                           time_interval = time_interval,
                           stations_to_choose_from = 50,
                           station_list = weather_dwd,
                           drop_most = TRUE,
                           add.DATE = FALSE,
                           quiet = TRUE,
                           add_station_name = FALSE)

data_clean <- chillR::handle_dwd(data)

# Extract one station
data_observed <- data_clean[[1]]

# Get long-term mean data
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
data_observed_30year <- data_clean_mean[[1]]

# Find hourly temp to replace with observed data, not to patch but to fill the last list
data_observed_hourly  <- stack_hourly_temps(data_observed, latitude = 50.62)

# Clean obeserved data format
data_observed_formatted <- data_observed_hourly$hourtemps %>%
  mutate(DATE = ymd(paste(Year, Month, Day))) %>%
  select(DATE, YEARMODA, Year, Month, Day, Hour, Tmin, Tmax, Temp)


# Find mean
data_observed_hourly_30year  <- stack_hourly_temps(data_observed_30year, latitude = 50.62)
data_observed_hourly_30year_df <- bind_rows(data_observed_hourly_30year)
data_observed_hourly_30year_df <- data_observed_hourly_30year_df %>%
  unnest(hourtemps)

# Here we have four forecast years
forecast_year <- c(2021, 2022, 2023, 2024)

# Loop because we need to patch by year
data_observed_mean <- list()

for (i in seq_along(forecast_year)) {
  
  if ((forecast_year[i] %% 4 == 0 & forecast_year[i] %% 100 != 0) | (forecast_year[i] %% 400 == 0)) {
    # Leap year: keep Feb 29
    longterm_hourly_mean <- data_observed_hourly_30year_df %>%
      group_by(Month, Day, Hour) %>%
      summarise(
        Tmin  = mean(Tmin,  na.rm = TRUE),
        Tmax  = mean(Tmax,  na.rm = TRUE),
        Temp  = mean(Temp,  na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Year = forecast_year[i],
             YEARMODA = as.integer(sprintf("%04d%02d%02d", Year, Month, Day))) %>%
      select(YEARMODA, Year, Month, Day, Hour, Tmin, Tmax, Temp)
  } else {
    # Non-leap year: remove Feb 29
    longterm_hourly_mean <- data_observed_hourly_30year_df %>%
      group_by(Month, Day, Hour) %>%
      summarise(
        Tmin  = mean(Tmin,  na.rm = TRUE),
        Tmax  = mean(Tmax,  na.rm = TRUE),
        Temp  = mean(Temp,  na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!(Month == 2 & Day == 29)) %>%
      mutate(Year = forecast_year[i],
             YEARMODA = as.integer(sprintf("%04d%02d%02d", Year, Month, Day))) %>%
      select(YEARMODA, Year, Month, Day, Hour, Tmin, Tmax, Temp)
  }
  
  data_observed_mean[[i]] <- longterm_hourly_mean %>%
    mutate(DATE = ymd(paste(Year, Month, Day))) %>%
    select(DATE, YEARMODA, Year, Month, Day, Hour, Tmin, Tmax, Temp)
  
}

# Flatten this nested list so I can patch easier (I think) # let's see if i need this later
# observed_mean_df <- bind_rows(data_observed_mean)
# I didn't need this 

# Here we add a new list to the forecast for observed data
# Only create a new list if we have full observed data
# for (j in seq_along(Bonn_forecast)) {
#   
#   i <- j  # pair i with j (nested i loop add more lists than i want)
#   
#   obs_last <- as.Date(sprintf("%d-%02d-01",
#                               tail(data_observed_mean[[i]]$Year, 1),
#                               tail(data_observed_mean[[i]]$Month, 1)))
#   
#   forecast_last <- as.Date(sprintf("%d-%02d-01",
#                                    tail(Bonn_forecast[[j]][[length(Bonn_forecast[[j]])]]$Year, 1),
#                                    tail(Bonn_forecast[[j]][[length(Bonn_forecast[[j]])]]$Month, 1)))
#   
#   if (obs_last >= forecast_last) {
#     Bonn_forecast[[j]][[length(Bonn_forecast[[j]]) + 1]] <- Bonn_forecast[[j]][[length(Bonn_forecast)]]
#   }
# }
# When I patched this, since the last list is a copy of the last forecast, 
# my commands in the patch loop didn't match for the last copied list,so the list turned zero.
# So, I tried patching first and  stack hourly. 

## Loop for 50 models in each forecast period for each forecast year

# Loop 
weather_combined <- list()

for (j in seq_along(Bonn_forecast)) {
  k <- j
  df_j <- Bonn_forecast[[j]]   # list of months for year j
  
  # recompute obs_last and forecast_last here
  obs_last <- as.Date(sprintf("%d-%02d-01",
                              tail(data_observed_mean[[k]]$Year, 1),
                              tail(data_observed_mean[[k]]$Month, 1)))
  
  forecast_last <- as.Date(sprintf("%d-%02d-01",
                                   tail(df_j[[length(df_j)]]$Year, 1),
                                   tail(df_j[[length(df_j)]]$Month, 1)))
  
  weather_combined[[j]] <- list()
  
  for (m in seq_along(df_j)) {
    df_m <- df_j[[m]]   # data.frame with 50 models
    weather_combined[[j]][[m]] <- list()
    
    for (mod in unique(df_m$model)) {
      # Filter by model
      df_model <- df_m %>%
        filter(model == mod) %>%
        rename(Temp = temp)
      
      # patching observed mean
      last_month <- tail(df_j[[m]]$Month, 1)
      
      patch_month <- data_observed_mean[[k]] %>%
        filter(Month > last_month)
      
      forecast_mean_patched <- bind_rows(df_model, patch_month)
      
      # patch observed data before first forecast 
      first_year <-  head(df_j[[m]]$Year, 1)
      first_month <- head(df_j[[m]]$Month, 1)
      
      if (first_month == 1) {
        obs_year <- first_year - 1
        obs_month <- 12
      } else {
        obs_year <- first_year
        obs_month <- first_month - 1
      }
      
      # extract all forecast years in df_j month
      years_in_dfj <- unique(df_j[[m]]$Year)
      
      if (length(years_in_dfj) > 1) {
        patch_observe <- data_observed %>%
          filter(Year == obs_year & Month <= obs_month)
      } else {
        patch_observe <- data_observed %>%
          filter(Year == obs_year - 1 | Year == obs_year & Month <= obs_month)
      }
      
      phenoflex_loop <- bind_rows(patch_observe, forecast_mean_patched)
      
      # Align format
      phenoflex_loop_aligned <- phenoflex_loop %>%
        mutate(
          DATE = ymd(paste(Year, Month, Day)),
          YEARMODA = sprintf("%04d%02d%02d", Year, Month, Day),
          Temp = case_when(
            is.na(Temp) ~ NA_real_,        # keep NA
            unit == "K" ~ Temp - 273.15,   # convert Kelvin to Celsius
            unit == "C" ~ Temp,            # keep Celsius as is
            TRUE ~ Temp                    # fallback to original
          )
        ) %>%
        select(DATE, YEARMODA, Year, Month, Day, Hour, Tmin, Tmax, Temp)
      
      # save into nested structure
      weather_combined[[j]][[m]][[as.character(mod)]] <- phenoflex_loop_aligned
    } # end model loop
    
  } # end month loop
  
} # end year loop

saveRDS(weather_combined, "data/weather_combined_four.rds")
weather_combined <- readRDS("data/weather_combined_four.rds")
str(weather_combined)

# Here we create another loop to find hourly temp and swap
# I just think I should because now the for-loop is for weather_combined data
# Maybe I can continue in the previous loop but splitting seems more clearer to me

# When I run the loop I got error:Error in if (!alltimes[1] < alltimes[length(alltimes)]) { : 
# missing value where TRUE/FALSE needed 
# Solution: https://stackoverflow.com/questions/7355187/error-in-if-while-condition-missing-value-where-true-false-needed

is.na(df_mod)
## THis didn't work because I need two different functions to get hourly data for forecast
## and observed. See Lar's advice. 

i <- 1
weather_combined_four_seasons <- list()

for (i in seq_along(weather_combined)) {
  
  df_i <- weather_combined[[i]] 
  weather_combined_four_seasons[[i]] <- list()
  
  for (m in seq_along(df_i)) {
    df_m <- df_i[[m]]   # list of model-specific dfs
    weather_combined_four_seasons[[i]][[m]] <- list()
    
    for (mod in names(df_m)) {   # loop over model names
      df_mod <- df_m[[mod]]      # extract specific model dataframe
      
      # Interpolation to hourly
      Bonn_forecast_hourly <- interpolate_gaps_hourly(
        hourtemps = df_mod,
        latitude = 50.62,
        daily_temps = NULL,
        interpolate_remaining = TRUE,
        return_extremes = FALSE,
        minimum_values_for_solving = 4,
        daily_patch_max_mean_bias = NA,
        daily_patch_max_stdev_bias = NA
      )
      
      # Save into nested structure
      weather_combined_four_seasons[[i]][[m]][[mod]] <- Bonn_forecast_hourly
    }
  }
}

## Notes from Lars:
# So the forecast data stops at 28th of last month and you want to interpolate for 
# the remaining days of the month? Two thoughts on this: let's check the download function. 
# Usually it should go until the end of the month if you select leadtime_hour = 'all'. 
# Maybe that is caused by different number of days in the month? Let's assume everything is 
# in order with the download and everything is correct. If the forecast ends by 28th of the 
# month, there is not a lot we can do about it? The interpolation serves two purposes: 
# fill random gaps in the dataset when there is a corresponding observations somewhere else, 
# or change the time-resolution of your data (e.g. from daily or six-hourly to hourly data). 
# But the situation you are describing appears to be neither of the cases.

# Lar's comments: The function inerpolate_gaps_hourly() function can help you to 
# bring the six-hourly data into an hourly format, also without any additional 
# observations. All it needs to know is the six-hourly data, the latitude and you 
# need to set the argument minimum_values_for_solving to 4, because we have for 
# observations per day. 
# For the other data you need to use the function that turns 
# daily observation into hourly ones, for example the stack_hourly_temps()  function. 
# The data needs to be complete (so there should no missing data within the rows) and 
# it needs to have columns called Tmin, Tmax (and a date column I suppose). Also in that 
# case you do not need additional observations
  
