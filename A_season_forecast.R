library(LarsChill)
library(ncdf4)
library(CFtime)
library(tidyverse)
library(chillR)
library(dplyr)
library(patchwork)
library(lubridate)
library(purrr)
library(ggplot2)

## Do it for 7 months forecast from October 2023 to April 2024

# I think in reality, we will not have enough forecasts from DWD for current year--
# Now we are in August but the forecast is till March only (4-5 months behind)
# Long-term mean will become crucial data, unless we can find observed temp data on a rolling basis.
# Otherwise, we might need to calibrate long-term mean as well??-- which period and how many years

month <- c(10, 11, 12, 1, 2, 3, 4) 
area <- c(50.78, 7, 50.62, 7.2)
year <- 2023

target_lat <- 50.62
target_lon <- 7
Bonn_forecast <- vector("list", length(month))
max_pos <- which.max(month)   # position of max

# This loop only works for two years max-- only one max position
# but we don't need forecast of more than 2 years (I suppose)

for (i in seq_along(month)) {
  
  if (i <= max_pos) {
    fname <- sprintf("forecast_bonn_%d%02d.nc", year, month[i])
    download_seasonal_forecast(
      year = year,
      month = month[i],
      area = area,
      leadtime_hour = seq(0, 24*30*6, by = 6),
      fname = fname,
      start_download = TRUE
    )
  } else {
    fname <- sprintf("forecast_bonn_%d%02d.nc", year + 1, month[i])
    download_seasonal_forecast(
      year = year + 1,
      month = month[i],
      area = area,
      leadtime_hour = seq(0, 24*30*6, by = 6),
      fname = fname,
      start_download = TRUE
    )
  }
  
  # Extract forecast
  Bonn_forecast_season <- extract_seasonal_forecast(fname,
                                                    target_lat = target_lat,
                                                    target_lon = target_lon)
  Bonn_forecast[[i]] <- Bonn_forecast_season
  
}

# Save so that I don't need to download again
# str(Bonn_forecast)
# saveRDS(Bonn_forecast, "data/Bonn_forecast.rds")
Bonn_forecast <- readRDS("data/Bonn_forecast.rds")
str(Bonn_forecast)

# Download observed
# No need to loop for this but maybe this can be put inside a function 
# Maybe not if we need to choose the station manually?
long <- 7
lat <- 50.62
time_interval = c(20230101, 20241231)

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
data_observed <- data_clean[["Köln/Bonn"]] # can I put it as 1 for all?
# assuming Station 1 is the nearest and best data? <-- Ask Lars


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

## Find mean for 30 years

# Calculate 30-year long-term mean
# longterm_mean <- data_observed_30year %>%
#   group_by(Month, Day) %>%
#   summarise(
#     Tmin  = mean(Tmin,  na.rm = TRUE),
#     Tmean = mean(Tmean, na.rm = TRUE),
#     Tmax  = mean(Tmax,  na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   filter(!(Month == 2 & Day == 29)) %>%   # remove Feb 29
#   mutate(Year   = 2025,
#          YEARMODA = as.integer(sprintf("%04d%02d%02d", Year, Month, Day))) %>%
#   select(YEARMODA, Year, Month, Day, Tmin, Tmax)
# 
# 
# # Make it to an hourly format
# data_observed_hourly_30year  <- stack_hourly_temps(longterm_mean, latitude = 50.62)
# # here got 7 warnings that In `[<-.data.frame`(`*tmp*`, , pc, value = list(YEARMODA = c(20250101L,  :
# # provided 24 variables to replace 1 variables


# What if i find hourly temp first before calculating mean?
data_observed_hourly_30year  <- stack_hourly_temps(data_observed_30year, latitude = 50.62)

# Bind_rows to solve error: no applicable method for 'group_by' applied to an object of class "list"
data_observed_hourly_30year_df <- bind_rows(data_observed_hourly_30year)

# Unnest the nested data frames
data_observed_hourly_30year_df <- data_observed_hourly_30year_df %>%
  unnest(hourtemps)

# Check column names again
names(data_observed_hourly_30year_df)

# Find mean
# Here we may need Feb 29 if we want to predict bloom on a leap year
# Our forecast year is 2024-- a leap year
forecast_year <- 2024

# Every year that is exactly divisible by four is a leap year, except for years 
# that are exactly divisible by 100, but these centurial years are leap years if 
# they are exactly divisible by 400. Source: https://en.wikipedia.org/wiki/Leap_year

if ((forecast_year %% 4 == 0 & forecast_year %% 100 != 0) | (forecast_year %% 400 == 0)) {
  # Leap year: keep Feb 29
  longterm_hourly_mean <- data_observed_hourly_30year_df %>%
    group_by(Month, Day, Hour) %>%
    summarise(
      Tmin  = mean(Tmin,  na.rm = TRUE),
      Tmean = mean(Tmean, na.rm = TRUE),
      Tmax  = mean(Tmax,  na.rm = TRUE),
      Temp  = mean(Temp,  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Year = forecast_year,
           YEARMODA = as.integer(sprintf("%04d%02d%02d", Year, Month, Day))) %>%
    select(YEARMODA, Year, Month, Day, Hour, Tmin, Tmax, Temp)
} else {
  # Non-leap year: remove Feb 29
  longterm_hourly_mean <- data_observed_hourly_30year_df %>%
    group_by(Month, Day, Hour) %>%
    summarise(
      Tmin  = mean(Tmin,  na.rm = TRUE),
      Tmean = mean(Tmean, na.rm = TRUE),
      Tmax  = mean(Tmax,  na.rm = TRUE),
      Temp  = mean(Temp,  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!(Month == 2 & Day == 29)) %>%
    mutate(Year = forecast_year,
           YEARMODA = as.integer(sprintf("%04d%02d%02d", Year, Month, Day))) %>%
    select(YEARMODA, Year, Month, Day, Hour, Tmin, Tmax, Temp)
}

# Clean obeserved data format
data_observed_mean_formatted <- longterm_hourly_mean %>%
  mutate(DATE = ymd(paste(Year, Month, Day))) %>%
  select(DATE, YEARMODA, Year, Month, Day, Hour, Tmin, Tmax, Temp)
## Stack first and find mean later solved the warnings problem


## Here I want to see bloom dates with all obeserved data as well, and I don't want to
## download another six-month forecast just to have a list to replace, So i just create a new list
# Here I add one more list to the Bonn_forecast where i will put all observed + long-term mean data

# If obeserved data is not available for last month of forecast, then we dont' create a new list
# If available, we will create a new list and filled in the observed
# I think we can download forecast the month of May and then patched the observed till April, but
# this would save an extra download-- also useful when we don't have forecast, replaced with long-term mean

obs_last <- as.Date(sprintf("%d-%02d-01",
                            tail(data_observed_formatted$Year, 1),
                            tail(data_observed_formatted$Month, 1)))

forecast_last <- as.Date(sprintf("%d-%02d-01",
                                 tail(Bonn_forecast[[length(Bonn_forecast)]]$Year, 1),
                                 tail(Bonn_forecast[[length(Bonn_forecast)]]$Month, 1)))

if (obs_last >= forecast_last) {
  # remember to only run once (if already saved in global environment)--> add more list with more runs
  Bonn_forecast[[length(Bonn_forecast) + 1]] <- Bonn_forecast[[length(Bonn_forecast)]]
  
  # Put NA values to temp column
  # Bonn_forecast[[length(Bonn_forecast)]]$temp <- NA # let's see if i can still stack this later
  # I can't stack the NA value, so leave filled data for now and then tried to replace
} 

# Check if a new list is added or not
length(Bonn_forecast)
View(Bonn_forecast)


## Loop for 50 models in each forecast period

# Loop 
j <- 1:8
i <- 1:50
forecast_year <- 2024
weather_combined <- list()  # will store 8 forecasts, each with 50 models

obs_last <- as.Date(sprintf("%d-%02d-01",
                            tail(data_observed_formatted$Year, 1),
                            tail(data_observed_formatted$Month, 1)))

forecast_last <- as.Date(sprintf("%d-%02d-01",
                                 tail(Bonn_forecast[[length(Bonn_forecast)]]$Year, 1),
                                 tail(Bonn_forecast[[length(Bonn_forecast)]]$Month, 1)))


for(j in 1:length(Bonn_forecast)) {
  
  df_j <- Bonn_forecast[[j]]  # extract j-th forecast
  weather_combined[[j]] <- list()  # initialize nested list for 50 models
  
  for(i in unique(df_j$model)) {
    
    # Filter by model and rename column
    df_model <- df_j %>%
      filter(model == i) %>%
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
    
    # Patch with long-term mean
    last_month <- tail(data_forecast_model_clean$Month, 1)
    
    patch_month <- data_observed_mean_formatted %>%
      filter(Month > last_month) %>%
      mutate(Year = max(data_forecast_model_clean$Year, na.rm = TRUE))
    
    if (max(data_forecast_model_clean$Year, na.rm = TRUE) < forecast_year) {
      forecast_mean_patched <- rbind(
        data_forecast_model_clean,
        patch_month,
        data_observed_mean_formatted
      )
    } else {
      forecast_mean_patched <- rbind(
        data_forecast_model_clean,
        patch_month
      )
    }
    
    # Patch with observed data
    first_year <- head(forecast_mean_patched$Year, 1)
    first_month <- head(forecast_mean_patched$Month, 1)
    
    # handle previous month correctly, coz i got error with previous codes (month == 0)
    if(first_month == 1){
      obs_year <- first_year - 1
      obs_month <- 12
    } else {
      obs_year <- first_year
      obs_month <- first_month - 1
    }
    
    patch_observe <- data_observed_formatted %>%
      filter(Year < obs_year | (Year == obs_year & Month <= obs_month))
    
    phenoflex_loop <- rbind(patch_observe, forecast_mean_patched)
    
    # Save in nested list
    weather_combined[[j]][[as.character(i)]] <- phenoflex_loop
    
      ## Swap the last list if we added a new list at the top
    if (obs_last >= forecast_last && j == length(Bonn_forecast)) {
      
      # Loop through each model separately
      for(model in names(weather_combined[[j]])) {
        
        phenoflex_loop <- weather_combined[[j]][[model]]
        
        # Build keys for matching
        data_observed_keyed <- data_observed_formatted %>%
          mutate(key_obs = sprintf("%04d-%02d-%02d-%02d", Year, Month, Day, Hour)) %>%
          select(key_obs, Tmin_obs = Tmin, Tmax_obs = Tmax, Temp_obs = Temp)
        
        data_mean_keyed <- data_observed_mean_formatted %>%
          mutate(key_mean = sprintf("%02d-%02d-%02d", Month, Day, Hour)) %>%
          select(key_mean, Tmin_mean = Tmin, Tmax_mean = Tmax, Temp_mean = Temp)
        
        phenoflex_loop <- phenoflex_loop %>%
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
        
        # Update the nested list 
        weather_combined[[j]][[model]] <- phenoflex_loop
      }
    }
  }
}

# Save to file
# str(weather_combined)
# saveRDS(weather_combined, "data/weather_combined.rds")
weather_combined <- readRDS("data/weather_combined.rds")
str(weather_combined)


## Predict bloom dates for cherry blossom 
bloom_dates <- list()
seasons <- 2024

for(j in seq_along(weather_combined)) {         
  bloom_dates[[j]] <- list()                    
  
  for(model in names(weather_combined[[j]])) {   
    weather_data <- weather_combined[[j]][[model]]
    
    iSeason <- genSeason(weather_data, years = seasons)
    
    
    for (sea in 1:length(seasons))
    {season_data <- weather_data[iSeason[[sea]], ]
      res <- PhenoFlex(temp = season_data$Temp,
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
                       basic_output = TRUE)
      
      bloom_dates[[j]][[model]] <- season_data$DATE[res[[sea]]]
    }
  }
}


bloom_dates 

bloom_plot <- list()

for(j in seq_along(bloom_dates)) {
  
  # Convert list to vector of Date
  bloom_vec <- as.Date(unlist(bloom_dates[[j]])) 
  
  # Create data frame
  predictions <- data.frame(
    Season = rep(seasons, length(bloom_vec)),
    Prediction = bloom_vec
  )
  
  # Create plot
  bloom_plot[[j]] <- ggplot(predictions, aes(x = Prediction)) +
    geom_bar(fill = "#69b3a2", color = "white") +
    xlab("Predicted bloom date") +
    ylab("Count") +
    theme_bw(base_size = 15) +
    scale_x_date(
      breaks = sort(unique(predictions$Prediction)),
      date_labels = "%Y-%m-%d"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

bloom_plot[[1]] # Oct 2023 forecast
bloom_plot[[2]] # Nov 2023 forecast
bloom_plot[[3]] # Dec 2023 forecast
bloom_plot[[4]] # Jan 2024 forecast
bloom_plot[[5]] # Feb 2024 forecast
bloom_plot[[6]] # March 2024 forecast
bloom_plot[[7]] # April 2024 forecast
bloom_plot[[8]] # Observed data

##---------------------------------##

# Convert the nested list to a data frame
bloom_df <- list()

# Loop through the outer list (each of the 8 lists)
for(i in seq_along(df)) {
  temp_df <- data.frame(
    list = rep(i, length(unlist(df[[i]]))),
    bloom_JDay = unlist(df[[i]])
  )
  bloom_df[[length(bloom_df) + 1]] <- temp_df
}

# Combine all data frames into one
bloom_df <- bind_rows(bloom_df)

# Convert list to a factor for proper grouping
bloom_df$list <- as.factor(bloom_df$list)

# Get the season and observed JDay (assuming season is constant)
season <- 2024
observed_JDay <- as.numeric(bloom_df %>% filter(list == 8) %>% pull(bloom_JDay))

# Calculate RMSE for each forecast list
RMSE_df <- bloom_df %>%
  filter(list %in% 1:7) %>%
  group_by(list) %>%
  summarize(
    RMSE = chillR::RMSEP(as.numeric(bloom_JDay), rep(observed_JDay, length(bloom_JDay)), na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(season = season)

# Add month names and calendar year
RMSE_df <- RMSE_df %>%
  mutate(
    forecast_month = case_when(
      list == 1 ~ "October",
      list == 2 ~ "November",
      list == 3 ~ "December",
      list == 4 ~ "January",
      list == 5 ~ "February",
      list == 6 ~ "March",
      list == 7 ~ "April"
    ),
    # Add a new column for the calendar year
    forecast_year = case_when(
      list %in% c(1, 2, 3) ~ season - 1,
      TRUE ~ season
    )
  ) %>%
  select("forecast_year", "forecast_month", "RMSE")

# Display the final table
print(RMSE_df)

## April 2024 forecast and all observed data are the same, coz for April 2024 forecast
## I patched the observed data till March 2024, so if we make prediction, the final forecast
## we need will be the month of March if the usual bloom date is in April.
## But the final predicted bloom date of March 13 isn't in the forecasted range. 
## So, we will have the best prediction if we got observed data for March for April prediction-- need real time data.

## According to this blog, cherry blossom began to fall on 10 April 2024:
## https://www.kirschbluete-bonn.de/mittwoch-10-04-2024-fast-vorbei/
## So, the bloom date would be before that. I can't find the bloom dates in the blog,
## maybe coz I don't know enough German. 

## But then, according to this website:
## https://www.bonn.de/pressemitteilungen/maerz-2024/bonner-kirschbluete-koennte-frueher-beginnen.php?loc=en
## they predicted bloom dates in early April, and they posted the article on March 20, 2024

## So, based on these two webistes, I can guess the real bloom dates fell between
## March 21 and first week of April (April 5) (since April 10 is the petal fall date and it is still
## full bloom on April 7 based on this: https://stelmaatje.com/cherry-blossoms-in-bonn/#The_Blooming_Period_of_Cherry_Blossoms_in_Bonn)

## So, even our final prediction with all observed data isn't close enough:
## Best prediction: 2024-03-13
## Real bloom dates: Between 2024-03-21 and 2024-04-5, more likely on early April
## Did I do something wrongly or do we need a better data source?

