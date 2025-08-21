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
data_observed <- data_clean[["Köln/Bonn"]]

# Find hourly temp
data_observed_hourly  <- stack_hourly_temps(data_observed, latitude = 50.62)

# Clean obeserved data format
data_observed_formatted <- data_observed_hourly$hourtemps %>%
  mutate(DATE = ymd(paste(Year, Month, Day))) %>%
  select(DATE, YEARMODA, Year, Month, Day, Hour, Tmin, Tmax, Temp)

# Save hourly observed data
saveRDS(data_observed_formatted, "data/Bonn_observed_four_seasons.rds")
data_observed <- readRDS("data/Bonn_observed_four_seasons.rds")
str(data_observed)

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
data_observed_30year <- data_clean_mean[["Köln/Bonn"]]

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

# Save hourly long-term mean data
saveRDS(data_observed_mean, "data/Bonn_longterm_mean_four_seasons.rds")
data_observed_mean <- readRDS("data/Bonn_longterm_mean_four_seasons.rds")
str(data_observed_mean)

# Flatten this nested list so I can patch easier (I think) # let's see if i need this later
observed_mean_df <- bind_rows(data_observed_mean)
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
# my commands in the patch loop didn't match well for the last copied list-- add an extra year.
# So, I will try do this later. 

## Loop for 50 models in each forecast period for each forecast year

# Loop 
weather_combined <- list()

for (j in seq_along(Bonn_forecast)) {
  k <- j
  df_j <- Bonn_forecast[[j]]   # list of months for year j
  
  weather_combined[[j]] <- list()
  
  for (m in seq_along(df_j)) {
    df_m <- df_j[[m]]   # data.frame with 50 models
    weather_combined[[j]][[m]] <- list()
    
    for (mod in unique(df_m$model)) {
      # Filter by model
      df_model <- df_m %>%
        filter(model == mod) %>%
        rename(Temp = temp)
      
      # Interpolation to hourly
      Bonn_forecast_hourly <- interpolate_gaps_hourly(
        hourtemps = df_model,
        latitude = 50.62,
        daily_temps = NULL,
        interpolate_remaining = TRUE,
        return_extremes = FALSE,
        minimum_values_for_solving = 4,
        daily_patch_max_mean_bias = NA,
        daily_patch_max_stdev_bias = NA
      )
      
      # Align format
      data_forecast_model_clean <- Bonn_forecast_hourly$weather %>%
        mutate(DATE = ymd(paste(Year, Month, Day)),
               YEARMODA = sprintf("%04d%02d%02d", Year, Month, Day),
               Temp = Temp - 273.15) %>%
        rename(Tmin = Tmin_source, Tmax = Tmax_source) %>%
        select(DATE, YEARMODA, Year, Month, Day, Hour, Tmin, Tmax, Temp) 
      # %>%
      #   mutate(
      #     Tmin = as.numeric(Tmin),
      #     Tmax = as.numeric(Tmax),
      #     Temp = as.numeric(Temp)
      #   )
      
      # patching observed mean
      last_month <- tail(df_j[[m]]$Month, 1)
      
      patch_month <- data_observed_mean[[k]] %>%
        filter(Month > last_month) %>%
        mutate(
          YEARMODA = as.character(YEARMODA))
      #     Tmin = as.numeric(Tmin),
      #     Tmax = as.numeric(Tmax),
      #     Temp = as.numeric(Temp)
      #   )
      
      forecast_mean_patched <- bind_rows(data_forecast_model_clean, patch_month)
      
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
      
      # patch_observe <- patch_observe %>%
      #   mutate(
      #     YEARMODA = as.character(YEARMODA),
      #     Tmin = as.numeric(Tmin),
      #     Tmax = as.numeric(Tmax),
      #     Temp = as.numeric(Temp)
      #   )
      # 
      phenoflex_loop <- bind_rows(patch_observe, forecast_mean_patched)
      
      # Align format
      phenoflex_loop_aligned <- phenoflex_loop %>%
        mutate(
          DATE = ymd(paste(Year, Month, Day)),
          YEARMODA = sprintf("%04d%02d%02d", Year, Month, Day)
        ) %>%
        select(DATE, YEARMODA, Year, Month, Day, Hour, Tmin, Tmax, Temp)
      
      # save into nested structure
      weather_combined[[j]][[m]][[mod]] <- phenoflex_loop_aligned
      
    } # end model loop
    
  } # end month loop
  
} # end year loop

saveRDS(weather_combined, "data/weather_combined_four.rds")
weather_combined <- readRDS("data/weather_combined_four.rds")
str(weather_combined)

# Here we create another loop to swap
# I just think I should because now the for-loop is for weather_combined data
# Maybe I can continue in the previous loop but splitting seems more clearer to me

weather_combined_four_seasons <- list()

# Add a new list for observed data, if available and copy the last forecast

for (j in seq_along(weather_combined)) {
    
    i <- j  
    
    obs_last <- as.Date(sprintf("%d-%02d-01",
                                tail(data_observed_mean[[i]]$Year, 1),
                                tail(data_observed_mean[[i]]$Month, 1)))
    
    forecast_last <- as.Date(sprintf("%d-%02d-01",
                                     tail(Bonn_forecast[[j]][[length(Bonn_forecast[[j]])]]$Year, 1),
                                     tail(Bonn_forecast[[j]][[length(Bonn_forecast[[j]])]]$Month, 1)))
    
    if (obs_last >= forecast_last) {
      weather_combined[[j]][[length(weather_combined[[j]]) + 1]] <- 
        weather_combined[[j]][[length(weather_combined[[j]])]]
    }
    
    ## Now we will swap the last list with all observed data or observed + long-term mean data
    ## Swap the last list if we added a new list at the top
    
    if (obs_last >= forecast_last && j == length(weather_combined)) {
      
      # Loop through each model separately
      for (m in seq_along(weather_combined[[j]])) {
        
        for (model in names(weather_combined[[j]][[m]])) {
          
          weather_loop <- weather_combined[[j]][[m]][[model]]
        
        # Build keys for matching
        data_observed_keyed <- data_observed %>%
          mutate(key_obs = sprintf("%04d-%02d-%02d-%02d", Year, Month, Day, Hour)) %>%
          select(key_obs, Tmin_obs = Tmin, Tmax_obs = Tmax, Temp_obs = Temp)
        
        data_mean_keyed <- data_observed_mean %>%
          mutate(key_mean = sprintf("%02d-%02d-%02d", Month, Day, Hour)) %>%
          select(key_mean, Tmin_mean = Tmin, Tmax_mean = Tmax, Temp_mean = Temp)
        
        weather_loop <- weather_loop %>%
          mutate(
            key_obs  = sprintf("%04d-%02d-%02d-%02d", Year, Month, Day, Hour),
            key_mean = sprintf("%02d-%02d-%02d", Month, Day, Hour)
          ) %>%
          left_join(data_observed_keyed, by = "key_obs") %>%
          left_join(data_mean_keyed, by = "key_mean") %>%
          mutate(
            Tmin = coalesce(Tmin_obs, Tmin_mean),
            Tmax = coalesce(Tmax_obs, Tmax_mean),
            Temp = coalesce(Temp_obs, Temp_mean),
            DATE = make_date(Year, Month, Day),
            YEARMODA = sprintf("%04d%02d%02d", Year, Month, Day)
          ) %>%
          select(DATE, YEARMODA, Year, Month, Day, Hour, Tmin, Tmax, Temp)
        
        # Save back into structure
        weather_combined[[j]][[m]][[model]] <- weather_loop
        }
      }
    }
    
   # Save updated structure into new four seasons list
    weather_combined_four_seasons[[j]] <- weather_combined[[j]]
}

# Save RDS
saveRDS(weather_combined_four_seasons, "data/weather_combined_four_seasons.rds")
weather_combined_four_seasons <- readRDS("data/weather_combined_four_seasons.rds")

## Predict bloom dates for cherry blossom for four seasons
bloom_dates_four_seasons <- list()
seasons <- 2021:2024   # four years

for (j in seq_along(weather_combined_four_seasons)) {         
  bloom_dates_four_seasons[[j]] <- list()                  
  
  for (m in seq_along(weather_combined_four_seasons[[j]])) { 
    bloom_dates_four_seasons[[j]][[m]] <- list()
    
    for (model in names(weather_combined_four_seasons[[j]][[m]])) {   
      weather_data <- weather_combined_four_seasons[[j]][[m]][[model]]
      
      iSeason <- genSeason(weather_data, years = seasons)
      
      bloom_dates_four_seasons[[j]][[m]][[model]] <- list()
      
      for (sea in 1:length(seasons)) 
        {season_data <- weather_data[iSeason[[sea]], ]
          res <- PhenoFlex(
          temp = season_data$Temp,
          times = seq_along(season_data$Temp),
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
          basic_output = TRUE
        )
        
        # Store bloom date for this season
        bloom_dates_four_seasons[[j]][[m]][[model]] <- 
          season_data$DATE[res[[sea]]]
      }
    }
  }
}


bloom_dates_four_seasons

#-----------------------------------------------------------------------------#
#--- Troubleshooting---#

# Loop 
weather_combined <- list()
for (j in seq_along(Bonn_forecast)) {
  df_j <- Bonn_forecast[[j]]   # list of months for year j
  
  weather_combined[[j]] <- list()
  
  for (m in seq_along(df_j)) {
    df_m <- df_j[[m]]   # data.frame with 50 models
    weather_combined[[j]][[m]] <- list()
    
    for (mod in unique(df_m$model)) {
      # Filter by model
      df_model <- df_m %>%
        filter(model == mod) %>%
        rename(Temp = temp)
      
      # Interpolation to hourly
      Bonn_forecast_hourly <- interpolate_gaps_hourly(
        hourtemps = df_model,
        latitude = 50.62,
        daily_temps = NULL,
        interpolate_remaining = TRUE,
        return_extremes = FALSE,
        minimum_values_for_solving = 4,
        daily_patch_max_mean_bias = NA,
        daily_patch_max_stdev_bias = NA
      )
      
      # Align format
      data_forecast_model_clean <- Bonn_forecast_hourly$weather %>%
        filter(model == mod) %>%
        mutate(DATE = ymd(paste(Year, Month, Day)),
               YEARMODA = sprintf("%04d%02d%02d", Year, Month, Day),
               Temp = Temp - 273.15) %>%
        rename(Tmin = Tmin_source, Tmax = Tmax_source) %>%
        select(DATE, YEARMODA, Year, Month, Day, Hour, Tmin, Tmax, Temp)
      
      # save into nested structure
      weather_combined[[j]][[m]][[mod]] <- data_forecast_model_clean
    }
  }
}

saveRDS(weather_combined, "data/Bonn_hourly_forecast_four_seasons.rds")
weather_combined_e <- readRDS("data/Bonn_hourly_forecast_four_seasons.rds")
str(weather_combined)

##---- Test with one model ---##
weather_test1 <- weather_combined_e[[1]][[2]][[1]]
# Patch observed data before first forecast
first_year <- head(weather_test1$Year, 1)
first_month <- head(weather_test1$Month, 1)

if (first_month == 1) {
  obs_year <- first_year - 1
  obs_month <- 12
} else {
  obs_year <- first_year
  obs_month <- first_month - 1
}

years_in_dfj <- unique(weather_test1$Year)

if (length(years_in_dfj) > 1) {
  patch_observe <- data_observed %>%
    filter(Year == obs_year & Month <= obs_month)
} else {
  patch_observe <- data_observed %>%
    filter(Year == obs_year - 1 | (Year == obs_year & Month <= obs_month))
}

# solve the errors
patch_observe$YEARMODA <- ymd(as.character(patch_observe$YEARMODA))

weather_test1 <- weather_test1 %>%
  mutate(
    YEARMODA = ymd(YEARMODA),     
    Tmin = as.numeric(Tmin),
    Tmax = as.numeric(Tmax),
    Temp = as.numeric(Temp)
  )

# Try patching
phenoflex_loop <- bind_rows(patch_observe, weather_test1)



######

# Loop using hourly forecast
weather_combined_e <- readRDS("data/Bonn_hourly_forecast_four_seasons.rds")

weather_combined_observed <- list()

for (y in seq_along(weather_combined_e)) {
  df_y <- weather_combined_e[[y]]
  weather_combined_observed[[y]] <- list()
  
  for (m in seq_along(df_y)) {
    df_m <- df_y[[m]]
    weather_combined_observed[[y]][[m]] <- list()
    
    for (l in seq_along(df_m)) {
      k <- j
      
      # patch observed data before first forecast 
      first_year <-  head(df_m[[l]]$Year, 1)
      first_month <- head(df_m[[l]]$Month, 1)
      
      if (first_month == 1) {
        obs_year <- first_year - 1
        obs_month <- 12
      } else {
        obs_year <- first_year
        obs_month <- first_month - 1
      }
      
      # extract all forecast years in df_j month
      years_in_dfm <- unique(df_m[[l]]$Year)
      
      if (length(years_in_dfm) > 1) {
        patch_observe <- data_observed %>%
          filter(Year == obs_year & Month <= obs_month)
      } else {
        patch_observe <- data_observed %>%
          filter(Year == obs_year - 1 | Year == obs_year & Month <= obs_month)
      }
      
      patch_observe$YEARMODA <- ymd(as.character(patch_observe$YEARMODA))
      
      df_current <- df_m[[l]] %>%
        mutate(
          YEARMODA = ymd(YEARMODA),     
          Tmin = as.numeric(Tmin),
          Tmax = as.numeric(Tmax),
          Temp = as.numeric(Temp)
        )
      
      # patching observed mean
      last_month <- tail(df_m[[l]]$Month, 1)
      
      patch_month <- data_observed_mean[[k]] %>%
        filter(Month > last_month)
      
      patch_month$YEARMODA <- ymd(as.character(patch_month$YEARMODA))
      
      phenoflex_loop <- bind_rows(patch_observe, df_current, patch_month)
      
      # Align format
      phenoflex_loop_aligned <- phenoflex_loop %>%
        mutate(
          DATE = ymd(paste(Year, Month, Day)),
          YEARMODA = sprintf("%04d%02d%02d", Year, Month, Day)) %>%
        select(DATE, YEARMODA, Year, Month, Day, Hour, Tmin, Tmax, Temp)
      
      weather_combined_observed[[y]][[m]][[l]] <- phenoflex_loop_aligned
    }
  }
}

saveRDS(weather_combined_observed, "data/Bonn_weather_combined_four_seasons.rds")
weather_combined_e <- readRDS("data/Bonn_weather_combined_four_seasons.rds")

### To do later: fix years (try defining unique models) and check for missing days and fix codes for that
### add a new list to each season for observed + long-term mean--> feed into phenoflex


########

# Loop using 6-hourly forecast
Bonn_forecast <- readRDS("data/Bonn_forecast_four_seasons.rds")
weather_combined_observed <- list()

for (j in seq_along(Bonn_forecast)) {
  k <- j
  df_j <- Bonn_forecast[[j]]   # list of months for year j
  
  # recompute obs_last and forecast_last here
  # obs_last <- as.Date(sprintf("%d-%02d-01",
  #                             tail(data_observed_mean[[k]]$Year, 1),
  #                             tail(data_observed_mean[[k]]$Month, 1)))
  # 
  # forecast_last <- as.Date(sprintf("%d-%02d-01",
  #                                  tail(df_j[[length(df_j)]]$Year, 1),
  #                                  tail(df_j[[length(df_j)]]$Month, 1)))
  
  weather_combined_observed[[j]] <- list()
  
  for (m in seq_along(df_j)) {
    df_m <- df_j[[m]]   # data.frame with 50 models
    weather_combined_observed[[j]][[m]] <- list()
    
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
      weather_combined_observed[[j]][[m]][[as.character(mod)]] <- phenoflex_loop_aligned
    } # end model loop
    
  } # end month loop
  
} # end year loop


#######

