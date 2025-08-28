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

## Bloom prediction for four seasons

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

## Find hourly forecast 
Bonn_forecast <- readRDS("data/Bonn_forecast_four_seasons.rds")
# Loop 
weather_combined <- list()

for(j in 1:length(Bonn_forecast)) {
  
  df_j <- Bonn_forecast[[j]]  
  weather_combined[[j]] <- list()  
  
  for (k in 1:length(df_j)) {
    df_m <- df_j[[k]]
    weather_combined[[j]][[k]] <- list()
  
    for(i in unique(df_m$model)) {
        
        df_model <- df_m %>%
          filter(model == i) %>%
          rename(Temp = temp)
        
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
        
        data_forecast_model_clean <- Bonn_forecast_hourly$weather %>%
          mutate(DATE = ymd(paste(Year, Month, Day)),
                 YEARMODA = sprintf("%04d%02d%02d", Year, Month, Day),
                 Temp = Temp - 273.15) %>%
          rename(Tmin = Tmin_source, Tmax = Tmax_source) %>%
          select(DATE, YEARMODA, Year, Month, Day, Hour, Tmin, Tmax, Temp)
        
        # Save in list
        weather_combined[[j]][[k]][[i]] <- data_forecast_model_clean
      }
   }
}


saveRDS(weather_combined, "data/Bonn_hourly_forecast_four_seasons.rds")
weather_combined <- readRDS("data/Bonn_hourly_forecast_four_seasons.rds")

# Loop using hourly forecast

weather_combined_observed <- list()

for (y in seq_along(weather_combined)) {
  df_y <- weather_combined[[y]]
  weather_combined_observed[[y]] <- list()
  
  for (m in seq_along(df_y)) {
    df_m <- df_y[[m]]
    weather_combined_observed[[y]][[m]] <- list()
    
    for (l in seq_along(df_m)) {
      
      k <- y
      
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
          Tmin = as.numeric(ifelse(Tmin %in% c("interpolated", "solved"), NA, Tmin)), # to solve warnings
          Tmax = as.numeric(ifelse(Tmax %in% c("interpolated", "solved"), NA, Tmax)),
          Temp = as.numeric(Temp))
      
      # patching observed mean
      # I changed to date coz I want to have all days- some forecasts end on 28th
      last_date <- tail(df_m[[l]]$DATE, 1)
      
      patch_month <- data_observed_mean[[k]] %>%
        filter(DATE > last_date)
      
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
weather_combined_four_seasons <- readRDS("data/Bonn_weather_combined_four_seasons.rds")

## Add a new list and replace with observed data
# Here we add a new list to the forecast for observed data
# Only create a new list if we have full observed data
four_seasons_weather_data <- list()

Bonn_forecast <- readRDS("data/Bonn_forecast_four_seasons.rds")
data_observed <- readRDS("data/Bonn_observed_four_seasons.rds")
data_observed_mean <- readRDS("data/Bonn_longterm_mean_four_seasons.rds")
weather_combined_four_seasons <- readRDS("data/Bonn_weather_combined_four_seasons.rds")

for (y in seq_along(weather_combined_four_seasons)) {
  
  # Copy original 7 months
  df_y <- weather_combined_four_seasons[[y]]
  four_seasons_weather_data[[y]] <- df_y
  
  # Compute last observed and forecast dates for this year
  obs_last <- as.Date(sprintf(
    "%d-%02d-01",
    tail(data_observed_mean[[y]]$Year, 1),
    tail(data_observed_mean[[y]]$Month, 1)
  ))
  
  forecast_last <- as.Date(sprintf(
    "%d-%02d-01",
    tail(Bonn_forecast[[y]][[length(Bonn_forecast[[y]])]]$Year, 1),
    tail(Bonn_forecast[[y]][[length(Bonn_forecast[[y]])]]$Month, 1)
  ))
  
  # If observed extends beyond forecast, add 8th month with one model
  if (obs_last > forecast_last) {
    last_month_index <- length(df_y)
    model_index <- 1  # choose the model to copy
    
    # Extract the model dataframe from last month
    model_entry <- df_y[[last_month_index]][[model_index]]
    if (is.list(model_entry) && !inherits(model_entry, "data.frame")) {
      model_entry <- model_entry[[1]]  # unwrap if nested
    }
    
    # Append 8th month directly as a data.frame to make swapping easier
    four_seasons_weather_data[[y]][[last_month_index + 1]] <- model_entry
  }
}

## Swap the last list with observed data

four_seasons_phenoflex_loop <- four_seasons_weather_data

for (j in seq_along(four_seasons_weather_data)) {
  
  for (m in seq_along(four_seasons_weather_data[[j]])) {
    
    last_month_index <- length(four_seasons_weather_data[[j]])
    
    # Compute last observed and forecast dates
    obs_last <- as.Date(sprintf(
      "%d-%02d-01",
      tail(data_observed_mean[[j]]$Year, 1),
      tail(data_observed_mean[[j]]$Month, 1)
    ))
    
    forecast_last <- as.Date(sprintf(
      "%d-%02d-01",
      tail(Bonn_forecast[[j]][[length(Bonn_forecast[[j]])]]$Year, 1),
      tail(Bonn_forecast[[j]][[length(Bonn_forecast[[j]])]]$Month, 1)
    ))
    
    if (obs_last >= forecast_last && m == length(four_seasons_weather_data[[j]])) {
      
      # Extract dataframe for last month
      
      phenoflex_loop <- four_seasons_weather_data[[j]][[last_month_index]]
      
      
      # Key observed
      data_observed_keyed <- data_observed %>%
        mutate(key_obs = sprintf(
          "%04d-%02d-%02d-%02d",
          as.numeric(Year), as.numeric(Month), as.numeric(Day), as.numeric(Hour)
        )) %>%
        select(key_obs, Tmin_obs = Tmin, Tmax_obs = Tmax, Temp_obs = Temp)
      
      data_mean_keyed <- data_observed_mean[[j]] %>%
        mutate(key_mean = sprintf(
          "%02d-%02d-%02d",
          as.numeric(Month), as.numeric(Day), as.numeric(Hour)
        )) %>%
        select(key_mean, Tmin_mean = Tmin, Tmax_mean = Tmax, Temp_mean = Temp)
      
      # Patch last month
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
      
      # Replace last month but turned to a list again so I can loop it in phenoflex
      four_seasons_phenoflex_loop[[j]][[last_month_index]] <- list(phenoflex_loop)
    }
  }
}

saveRDS(four_seasons_phenoflex_loop, "data/Bonn_four_seasons_phenoflex_loop.rds")
four_seasons_phenoflex_loop <- readRDS("data/Bonn_four_seasons_phenoflex_loop.rds")


## Predict bloom dates for cherry blossom for four seasons

# Initialize output
bloom_dates_four_seasons <- vector("list", length(four_seasons_phenoflex_loop))

for (j in seq_along(four_seasons_phenoflex_loop)) {
  bloom_dates_four_seasons[[j]] <- vector("list", length(four_seasons_phenoflex_loop[[j]]))
  
  for (m in seq_along(four_seasons_phenoflex_loop[[j]])) {
    bloom_dates_four_seasons[[j]][[m]] <- vector("list", length(four_seasons_phenoflex_loop[[j]][[m]]))
    
    for (k in seq_along(four_seasons_phenoflex_loop[[j]][[m]])) {
      weather_data <- four_seasons_phenoflex_loop[[j]][[m]][[k]]
      
      # Get the forecast year from the last date
      forecast_year <- as.integer(format(max(weather_data$DATE), "%Y"))
      
      preseason_start <- as.Date(paste0(forecast_year - 1, "-08-01")) # bloom_dates change with this
      season_end <- as.Date(paste0(forecast_year, "-06-30")) # follow the default of genSeason
      
      # Filter seasonal data
      season_data <- weather_data[weather_data$DATE >= preseason_start & weather_data$DATE <= season_end, ]
      
      if (nrow(season_data) == 0) {
        bloom_dates_four_seasons[[j]][[m]][[k]] <- NA
        next
      }
      
      # Run PhenoFlex
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
      
      # Extract bloom date
      bloom_idx <- res$bloomindex
      bloom_date <- if (!is.null(bloom_idx) && bloom_idx > 0) {
        bd <- season_data$DATE[bloom_idx]
        bd <- bd[bd >= as.Date(paste0(forecast_year, "-01-01")) & bd <= season_end]
        if (length(bd) == 0) NA else bd
      } else {
        NA
      }
      
      # Save bloom date
      bloom_dates_four_seasons[[j]][[m]][[k]] <- bloom_date
    }
  }
}

bloom_dates_four_seasons

## Save table
# Initialize empty data frame
# Calculate total number of entries
n <- sum(sapply(bloom_dates_four_seasons, function(x) sum(sapply(x, length))))

# Preallocate vectors
season_vec <- integer(n)
list_vec <- integer(n)
model_vec <- integer(n)
bloom_date_vec <- as.Date(rep(NA, n))
bloom_JDay_vec <- integer(n)

index <- 1

# Flatten nested list efficiently
for (j in seq_along(bloom_dates_four_seasons)) {
  for (m in seq_along(bloom_dates_four_seasons[[j]])) {
    for (k in seq_along(bloom_dates_four_seasons[[j]][[m]])) {
      bloom_value <- bloom_dates_four_seasons[[j]][[m]][[k]]
      
      season_vec[index] <- j
      list_vec[index] <- m
      model_vec[index] <- k
      
      if (!is.na(bloom_value)) {
        bloom_date_vec[index] <- bloom_value       # Already Date
        bloom_JDay_vec[index] <- as.POSIXlt(bloom_value)$yday + 1
      } else {
        bloom_JDay_vec[index] <- NA
      }
      
      index <- index + 1 # not to overwrite the current index
    }
  }
}

# Create final data frame
bloom_dates_table <- data.frame(
  season = season_vec,
  list = list_vec,
  model = model_vec,
  bloom_date = bloom_date_vec,
  bloom_JDay = bloom_JDay_vec
)

# Map year index to starting year
start_year <- 2020

# Initialize forecast_year and forecast_month columns
bloom_dates_table$forecast_year <- bloom_dates_table$season
bloom_dates_table$forecast_month <- bloom_dates_table$list

# Convert month index to calendar month and adjust year
bloom_dates_table <- within(bloom_dates_table, {
  forecast_month <- case_when(
    list == 1 ~ 10,
    list == 2 ~ 11,
    list == 3 ~ 12,
    list == 4 ~ 1,
    list == 5 ~ 2,
    list == 6 ~ 3,
    list == 7 ~ 4,
    list == 8 ~ NA_integer_
  )
  
  forecast_year <- case_when(
    list %in% 1:3 ~ start_year + season - 1,  # Oct–Dec of current year
    list %in% 4:7 ~ start_year + season,      # Jan–Apr of next year
    list == 8 ~ NA_integer_
  )
})

# Reorder column

bloom_dates_table_full <- bloom_dates_table[, c(
  "season", "list", "forecast_year", "forecast_month", "model", "bloom_date", "bloom_JDay"
)]


bloom_dates_table <- bloom_dates_table[, c(
  "forecast_year", "forecast_month", "model", "bloom_date", "bloom_JDay"
)]

# Check
head(bloom_dates_table)
tail(bloom_dates_table)

# Save to CSV
write.csv(bloom_dates_table, "data/bloom_dates_table.csv", row.names = FALSE)
write.csv(bloom_dates_table_full, "data/bloom_dates_table_full.csv", row.names = FALSE)

# load the csv
df <- read.csv("data/bloom_dates_table_full.csv")

plots <- list()

for(sea in 1:4){
  
  # Filter for season and lists <= 8
  df_season <- df %>%
    filter(season == sea, list <= 8) %>%
    mutate(
      list = factor(list, levels = sort(unique(list)))
    ) %>%
    filter(!is.na(bloom_JDay)) 
  
  # JDay for list 8
  jday_list8 <- df %>%
    filter(season == sea, list == 8) %>%
    pull(bloom_JDay) 
  
  # Data frame for legend
  legend_df <- data.frame(x = jday_list8, label = "Observed bloom date")
  
  # Identify lists with constant values for bold points
  constant_lists <- df_season %>%
    group_by(list) %>%
    summarize(sd_val = sd(bloom_JDay, na.rm = TRUE), .groups = "drop") %>%
    filter(sd_val == 0) %>%
    pull(list) 
  
  # Get all unique levels and remove '8' for y-axis labels
  y_levels <- unique(df_season$list)
  y_levels <- y_levels[y_levels != 8]
  
  # Plot
  p <- ggplot(df_season, aes(y = list, x = bloom_JDay)) +
    # Violin for lists other than 7
    geom_violin(data = df_season %>% filter(!(list %in% constant_lists)),
                aes(fill = list), trim = FALSE, scale = "width") +
    # Add horizontal lines for 5th, 50th, and 95th percentiles for violins
    stat_summary(
      data = df_season %>% filter(!(list %in% constant_lists)),
      fun.data = function(x) {
        data.frame(
          ymin = quantile(x, 0.05, na.rm = TRUE),
          ymax = quantile(x, 0.95, na.rm = TRUE),
          y = quantile(x, 0.50, na.rm = TRUE)
        )
      },
      geom = "pointrange",
      color = "black",
      size = 1,
      fatten = 2
    ) +
    # A single point for list 7
    geom_point(data = df_season %>% filter(list %in% constant_lists),
               aes(y = list, x = bloom_JDay, fill = list),
               color = "black", size = 3, shape = 21) +
    # Observed JDay for list 8 (dotted line)
    geom_vline(data = legend_df, aes(xintercept = x, color = label),
               linetype = "dotted", size = 1) +
    # Change legend labels to month names
    scale_fill_discrete(
      labels = c("1" = "October", 
                 "2" = "November", 
                 "3" = "December", 
                 "4" = "January", 
                 "5" = "February", 
                 "6" = "March", 
                 "7" = "April")
    ) +
    
    scale_color_manual(values = c("Observed bloom date" = "black")) +
    labs(
      y = "Forecast month list",
      x = "Bloom Julian Day",
      fill = "Forecast List",
      color = "",
      title = paste("Violin Plot of Bloom Julian Day for Season", 
                    sea, 
                    sprintf("[%d-%d]", min(df_season$forecast_year, na.rm = TRUE), 
                            min(df_season$forecast_year, na.rm = TRUE) + 1))
      ) +
    scale_y_discrete(limits = rev(y_levels)) +
    scale_x_continuous(breaks = c(
      # Breaks before jday_list8
      seq(from = jday_list8 - 43, to = jday_list8 - 25, by = 25),
      # Breaks at jday_list8 and after
      seq(from = jday_list8, to = 350, by = 25)
    )) +
    theme_minimal() +
    theme(axis.text.y = element_text(angle = 0, hjust = 1))
  
  plots[[paste0("season", sea)]] <- p
}

# Display a plot 
plots$season1
plots$season2
plots$season3
plots$season4

# Remove the legend from the first three plots
plots$season1 <- plots$season1 + theme(legend.position = "none")
plots$season2 <- plots$season2 + theme(legend.position = "none")
plots$season3 <- plots$season3 + theme(legend.position = "none")

# Combine the plots
combined_plot <- plots$season1 + plots$season2 + plots$season3 + plots$season4 +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(title = "Violin Plots of Bloom Julian Day for All Seasons") 


combined_plot

## Find RMSE for each forecast month
df <- read.csv("data/bloom_dates_table_full.csv")

RMSE_df <- data.frame()

for(sea in 1:4){ 
  
  # Filter for observed value
  observed <- df %>%
    filter(season == sea, list == 8) %>%
    pull(bloom_JDay)
  
  # Filter for predicted values (lists 3 through 7) to avoid the NA values
  predicted_df <- df %>%
    filter(season == sea, list >= 1, list <= 7)
  
  # Calculate RMSE for each forecast list
  rmse_results <- predicted_df %>%
    group_by(list) %>%
    summarize(
      RMSE = chillR::RMSEP(bloom_JDay, rep(observed, length(bloom_JDay)), na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(season = sea)
  
  # Append results to the main data frame
  RMSE_df <- bind_rows(RMSE_df, rmse_results)
  
}

print(RMSE_df)


RMSE_df <- RMSE_df %>%
  mutate(
    # Assign forecast month based on list
    forecast_month = case_when(
      list == 1 ~ "October",
      list == 2 ~ "November",
      list == 3 ~ "December",
      list == 4 ~ "January",
      list == 5 ~ "February",
      list == 6 ~ "March",
      list == 7 ~ "April",
      TRUE ~ NA_character_
    ),
    # Assign forecast year based on season and list
    forecast_year = case_when(
      season == 1 & list %in% 1:3 ~ 2020,
      season == 1 & list %in% 4:7 ~ 2021,
      season == 2 & list %in% 1:3 ~ 2021,
      season == 2 & list %in% 4:7 ~ 2022,
      season == 3 & list %in% 1:3 ~ 2022,
      season == 3 & list %in% 4:7 ~ 2023,
      season == 4 & list %in% 1:3 ~ 2023,
      season == 4 & list %in% 4:7 ~ 2024,
      TRUE ~ NA_real_
    )
  ) %>%
  select(season, list, forecast_year, forecast_month, RMSE) %>%
  arrange(season, list) 

print(RMSE_df)


# Save the RMSE_df data frame to a CSV file
write.csv(RMSE_df, "data/RMSE_results.csv", row.names = FALSE)

##-----------------------------------------------------##
# Troubleshooting phenoflex_loop
##------------------------------------------------------##
month_data <- four_seasons_phenoflex_loop[[4]][[5]]
weather_data <- month_data[[1]]
iSeason <- genSeason(weather_data, years = 2023)
#  for (sea in seq_along(seasons)) {
season_data <- weather_data[iSeason[[1]], ]

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


res


##------------------------------------------------------##


weather_data <- four_seasons_phenoflex_loop[[4]][[1]][[1]]

iSeason <- genSeason(weather_data, years = 2023)

season_data <- weather_data[iSeason[[1]], ]
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

res
season_data$DATE[res[[1]]]

# Pick enough pre-season rows
View(weather_data)
preseason_start <- as.Date("2023-01-01")
season_rows <- which(weather_data$DATE >= preseason_start & 
                       weather_data$DATE <= as.Date("2024-12-31"))
season_data <- weather_data[season_rows, ]

iSeason <- genSeason(season_data, years = 2021)
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
res$bloomindex
season_data$DATE[res[[1]]]

summary(season_data$Temp)

## So, I can't use the phenoflex codes from tree_phenology class because
## as the seasons (years) loop, data got overwritten and returned nothing



