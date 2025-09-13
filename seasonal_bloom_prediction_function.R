
## Build a function to predict bloom dates seasonally based on weather data
# Note: need to be at least two seasons

seasonal_bloom_prediction <- function(month,
                                      area,
                                      download_year,
                                      target_lat,
                                      target_lon,
                                      time_interval,
                                      forecast_year,
                                      season_start,
                                      season_end,
                                      A0 = 6319.5,
                                      A1 = 5.939917e+13,
                                      E0 = 3372.8,
                                      E1 = 9900.3,
                                      slope = 1.6,
                                      Tf = 4,
                                      s1 = 0.5,
                                      Tu = 25,
                                      Tb = 4,
                                      Tc = 36,
                                      yc = 40,
                                      Delta = 4,
                                      Imodel = 0L,
                                      zc = 190,
                                      stopatzc = TRUE,
                                      deg_celsius = TRUE,
                                      basic_output = TRUE) {
  
  # =======================
  # Forecast temp
  # =======================
  
  forecast <- vector("list", length(download_year))
  max_pos <- which.max(month) 
  
  for (j in seq_along(download_year)) {
    forecast[[j]] <- vector("list", length(month))
    
    for(i in seq_along(month)) { 
      
      fname <- sprintf("forecast_%d%02d_%.2f_%.2f.nc",
                       ifelse(i <= max_pos, download_year[j], download_year[j] + 1),
                       month[i], target_lon, target_lat)
      
      success <- tryCatch({
        LarsChill::download_seasonal_forecast(
          year = ifelse(i <= max_pos, download_year[j], download_year[j] + 1),
          month = month[i],
          area = area,
          leadtime_hour = "all",
          fname = fname,
          start_download = TRUE
        )
        TRUE   
      }, error = function(e) {
        message("Download failed for ", fname, ": ", e$message)
        FALSE 
      })
      
      if (!success) next 
      
      forecast_season <- LarsChill::extract_seasonal_forecast(fname,
                                                              target_lat = target_lat,
                                                              target_lon = target_lon)
      forecast[[j]][[i]] <- forecast_season
    }
  }
  
  # Save forecasts
  saveRDS(forecast,"seasonal_forecast_data.rds")
  
  weather_combined <- list()
  
  for(j in 1:length(forecast)) {
    
    df_j <- forecast[[j]]  
    weather_combined[[j]] <- list()  
    
    for (k in 1:length(df_j)) {
      df_m <- df_j[[k]]
      weather_combined[[j]][[k]] <- list()
      
      for(i in unique(df_m$model)) {
        
        filtered_df <- dplyr::filter(df_m, model == i)
        df_model <- dplyr::rename(filtered_df, Temp = temp)
        
        forecast_hourly <- chillR::interpolate_gaps_hourly(
          hourtemps = df_model,
          latitude = target_lat,
          daily_temps = NULL,
          interpolate_remaining = TRUE,
          return_extremes = FALSE,
          minimum_values_for_solving = 4,
          daily_patch_max_mean_bias = NA,
          daily_patch_max_stdev_bias = NA
        )
        
        data_forecast_model_clean <- dplyr::select(
          dplyr::rename(
            dplyr::mutate(
              forecast_hourly$weather,
              DATE = lubridate::ymd(paste(Year, Month, Day)),
              YEARMODA = sprintf("%04d%02d%02d", Year, Month, Day),
              Temp = Temp - 273.15
            ),
            Tmin = Tmin_source,
            Tmax = Tmax_source
          ),
          DATE, YEARMODA, Year, Month, Day, Hour, Tmin, Tmax, Temp
        )
        
        weather_combined[[j]][[k]][[i]] <- data_forecast_model_clean
      }
    }
  }
  
  
  saveRDS(weather_combined, "seasonal_hourly_forecast_data.rds")
  
  # =======================
  # Observed temp
  # =======================
  weather_dwd <- chillR::handle_dwd(action = 'list_stations', 
                                    location = c(target_lon, target_lat), 
                                    time_interval = time_interval)
  
  data <- chillR::handle_dwd(action = "download_weather",
                             location = weather_dwd[1:25, "Station_ID"],
                             time_interval = time_interval,
                             stations_to_choose_from = 50,
                             station_list = weather_dwd,
                             drop_most = TRUE,
                             add.DATE = FALSE,
                             quiet = TRUE,
                             add_station_name = FALSE)
  
  data_clean <- chillR::handle_dwd(data)
  
  data_observed <- data_clean[[1]]
  
  data_observed_hourly  <- chillR::stack_hourly_temps(data_observed, latitude = target_lat)
  
  data_observed_formatted <- dplyr::select(
    dplyr::mutate(
      data_observed_hourly$hourtemps,
      DATE = lubridate::ymd(paste(Year, Month, Day))
    ),
    DATE, YEARMODA, Year, Month, Day, Hour, Tmin, Tmax, Temp
  )
  
  # save observed data
  saveRDS(data_observed_formatted, "seasonal_hourly_observed_data.rds")
  
  
  # =======================
  # Long-term mean temp 
  # =======================
  
  weather_dwd_mean <- chillR::handle_dwd(action = 'list_stations', location = c(target_lon, 
                                                                                target_lat), 
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
  
  data_observed_30year <- data_clean_mean[[1]]
  
  data_observed_hourly_30year  <- chillR::stack_hourly_temps(data_observed_30year, latitude = target_lat)
  data_observed_hourly_30year_df <- dplyr::bind_rows(data_observed_hourly_30year)
  data_observed_hourly_30year_df <- tidyr::unnest(data_observed_hourly_30year_df, hourtemps)
  
  data_observed_mean <- list()
  
  for (i in seq_along(forecast_year)) {
    
    if ((forecast_year[i] %% 4 == 0 & forecast_year[i] %% 100 != 0) | (forecast_year[i] %% 400 == 0)) {
      
      longterm_hourly_mean <- dplyr::select(
        dplyr::mutate(
          dplyr::summarise(
            dplyr::group_by(
              data_observed_hourly_30year_df,
              Month, Day, Hour
            ),
            Tmin  = mean(Tmin, na.rm = TRUE),
            Tmax  = mean(Tmax, na.rm = TRUE),
            Temp  = mean(Temp, na.rm = TRUE),
            .groups = "drop"
          ),
          Year = forecast_year[i],
          YEARMODA = as.integer(sprintf("%04d%02d%02d", Year, Month, Day))
        ),
        YEARMODA, Year, Month, Day, Hour, Tmin, Tmax, Temp
      )
      
    } else {
      
      longterm_hourly_mean <- dplyr::select(
        dplyr::mutate(
          dplyr::filter(
            dplyr::summarise(
              dplyr::group_by(
                data_observed_hourly_30year_df,
                Month, Day, Hour
              ),
              Tmin  = mean(Tmin, na.rm = TRUE),
              Tmax  = mean(Tmax, na.rm = TRUE),
              Temp  = mean(Temp, na.rm = TRUE),
              .groups = "drop"
            ),
            !(Month == 2 & Day == 29)
          ),
          Year = forecast_year[i],
          YEARMODA = as.integer(sprintf("%04d%02d%02d", Year, Month, Day))
        ),
        YEARMODA, Year, Month, Day, Hour, Tmin, Tmax, Temp
      )
      
    }
    
    data_observed_mean[[i]] <- dplyr::select(
      dplyr::mutate(
        longterm_hourly_mean,
        DATE = lubridate::ymd(paste(Year, Month, Day))
      ),
      DATE, YEARMODA, Year, Month, Day, Hour, Tmin, Tmax, Temp
    )
    
  }
  
  # Save hourly long-term mean data
  saveRDS(data_observed_mean, "longterm_mean_hourly_data.rds")
  
  # =======================
  # Data patching
  # =======================
  
  weather_combined_observed <- list()
  
  for (y in seq_along(weather_combined)) {
    df_y <- weather_combined[[y]]
    weather_combined_observed[[y]] <- list()
    
    for (m in seq_along(df_y)) {
      df_m <- df_y[[m]]
      weather_combined_observed[[y]][[m]] <- list()
      
      for (l in seq_along(df_m)) {
        
        k <- y
        
        first_year <-  head(df_m[[l]]$Year, 1)
        first_month <- head(df_m[[l]]$Month, 1)
        
        if (first_month == 1) {
          obs_year <- first_year - 1
          obs_month <- 12
        } else {
          obs_year <- first_year
          obs_month <- first_month - 1
        }
        
        years_in_dfm <- unique(df_m[[l]]$Year)
        
        if (length(years_in_dfm) > 1) {
          
          patch_observe <- dplyr::filter(
            data_observed_formatted,
            Year == obs_year & Month <= obs_month
          )
          
        } else {
          
          patch_observe <- dplyr::filter(
            data_observed_formatted,
            Year == (obs_year - 1) | (Year == obs_year & Month <= obs_month)
          )
        }
        
        patch_observe$YEARMODA <- lubridate::ymd(as.character(patch_observe$YEARMODA))
        
        df_current <- dplyr::mutate(
          df_m[[l]],
          YEARMODA = lubridate::ymd(YEARMODA),
          Tmin = as.numeric(ifelse(Tmin %in% c("interpolated", "solved"), NA, Tmin)),
          Tmax = as.numeric(ifelse(Tmax %in% c("interpolated", "solved"), NA, Tmax)),
          Temp = as.numeric(Temp)
        )
        
        last_date <- tail(df_m[[l]]$DATE, 1)
        
        patch_month <- dplyr::filter(data_observed_mean[[k]], DATE > last_date)
        
        patch_month$YEARMODA <- lubridate::ymd(as.character(patch_month$YEARMODA))
        
        phenoflex_loop <- dplyr::bind_rows(patch_observe, df_current, patch_month)
        
        phenoflex_loop_aligned <- dplyr::select(
          dplyr::mutate(
            phenoflex_loop,
            DATE = lubridate::ymd(paste(Year, Month, Day)),
            YEARMODA = sprintf("%04d%02d%02d", Year, Month, Day)
          ),
          DATE, YEARMODA, Year, Month, Day, Hour, Tmin, Tmax, Temp
        )
        
        weather_combined_observed[[y]][[m]][[l]] <- phenoflex_loop_aligned
      }
    }
  }
  
  saveRDS(weather_combined_observed, "seasonal_hourly_patched_data.rds")
  
  # =========================
  # Create data for PhenoFlex
  # =========================
  
  phenoflex_weather_data <- list()
  
  for (y in seq_along(weather_combined_observed)) {
    
    df_y <- weather_combined_observed[[y]]
    phenoflex_weather_data[[y]] <- df_y
    
    obs_last <- as.Date(sprintf(
      "%d-%02d-01",
      tail(data_observed_mean[[y]]$Year, 1),
      tail(data_observed_mean[[y]]$Month, 1)
    ))
    
    forecast_last <- as.Date(sprintf(
      "%d-%02d-01",
      tail(forecast[[y]][[length(forecast[[y]])]]$Year, 1),
      tail(forecast[[y]][[length(forecast[[y]])]]$Month, 1)
    ))
    
    if (obs_last > forecast_last) {
      last_month_index <- length(df_y)
      model_index <- 1  
      
      model_entry <- df_y[[last_month_index]][[model_index]]
      if (is.list(model_entry) && !inherits(model_entry, "data.frame")) {
        model_entry <- model_entry[[1]] 
      }
      
      phenoflex_weather_data[[y]][[last_month_index + 1]] <- model_entry
    }
  }
  
  
  phenoflex_loop <- phenoflex_weather_data
  
  for (j in seq_along(phenoflex_weather_data)) {
    
    for (m in seq_along(phenoflex_weather_data[[j]])) {
      
      last_month_index <- length(phenoflex_weather_data[[j]])
      
      obs_last <- as.Date(sprintf(
        "%d-%02d-01",
        tail(data_observed_mean[[j]]$Year, 1),
        tail(data_observed_mean[[j]]$Month, 1)
      ))
      
      forecast_last <- as.Date(sprintf(
        "%d-%02d-01",
        tail(forecast[[j]][[length(forecast[[j]])]]$Year, 1),
        tail(forecast[[j]][[length(forecast[[j]])]]$Month, 1)
      ))
      
      if (obs_last >= forecast_last && m == length(phenoflex_weather_data[[j]])) {
        
        
        phenoflex <- phenoflex_weather_data[[j]][[last_month_index]]
        
        data_observed_keyed <- dplyr::select(
          dplyr::mutate(
            data_observed_formatted,
            key_obs = sprintf(
              "%04d-%02d-%02d-%02d",
              as.numeric(Year), as.numeric(Month), as.numeric(Day), as.numeric(Hour)
            )
          ),
          key_obs,
          Tmin_obs = Tmin,
          Tmax_obs = Tmax,
          Temp_obs = Temp
        )
        
        data_mean_keyed <- dplyr::select(
          dplyr::mutate(
            data_observed_mean[[j]],
            key_mean = sprintf(
              "%02d-%02d-%02d",
              as.numeric(Month), as.numeric(Day), as.numeric(Hour)
            )
          ),
          key_mean,
          Tmin_mean = Tmin,
          Tmax_mean = Tmax,
          Temp_mean = Temp
        )
        
        phenoflex <- dplyr::select(
          dplyr::mutate(
            dplyr::left_join(
              dplyr::left_join(
                dplyr::mutate(
                  phenoflex,
                  key_obs  = sprintf("%04d-%02d-%02d-%02d", Year, Month, Day, Hour),
                  key_mean = sprintf("%02d-%02d-%02d", Month, Day, Hour)
                ),
                data_observed_keyed,
                by = "key_obs"
              ),
              data_mean_keyed,
              by = "key_mean"
            ),
            Tmin = dplyr::coalesce(Tmin_obs, Tmin_mean),
            Tmax = dplyr::coalesce(Tmax_obs, Tmax_mean),
            Temp = dplyr::coalesce(Temp_obs, Temp_mean),
            DATE = lubridate::make_date(Year, Month, Day),
            YEARMODA = sprintf("%04d%02d%02d", Year, Month, Day)
          ),
          DATE, YEARMODA, Year, Month, Day, Hour, Tmin, Tmax, Temp
        )
        
        phenoflex_loop[[j]][[last_month_index]] <- list(phenoflex)
      }
    }
  }
  
  saveRDS(phenoflex_loop, "phenoflex_data.rds")
  
  # =========================
  # The PhenoFlex
  # =========================
  
  seasonal_bloom_dates <- vector("list", length(phenoflex_loop))
  
  for (j in seq_along(phenoflex_loop)) {
    seasonal_bloom_dates[[j]] <- vector("list", length(phenoflex_loop[[j]]))
    
    for (m in seq_along(phenoflex_loop[[j]])) {
      seasonal_bloom_dates[[j]][[m]] <- vector("list", length(phenoflex_loop[[j]][[m]]))
      
      for (k in seq_along(phenoflex_loop[[j]][[m]])) {
        weather_data <- phenoflex_loop[[j]][[m]][[k]]
        
        # detect forecast year from last date in dataset
        forecast_year_j <- as.integer(format(max(weather_data$DATE), "%Y"))
        
        # preseason window
        preseason_start <- as.Date(paste0(forecast_year_j - 1, season_start)) 
        preseason_end   <- as.Date(paste0(forecast_year_j, season_end)) 
        
        # filter weather data
        season_data <- weather_data[weather_data$DATE >= preseason_start & 
                                      weather_data$DATE <= preseason_end, ]
        
        # run PhenoFlex
        res <- chillR::PhenoFlex(
          temp = season_data$Temp,
          times = seq_along(season_data$Temp),
          A0 = A0, 
          A1 = A1, 
          E0 = E0, 
          E1 = E1,
          slope = slope, 
          Tf = Tf, 
          s1 = s1,
          Tu = Tu, 
          Tb = Tb, 
          Tc = Tc, 
          yc = yc,
          Delta = Delta, 
          Imodel = Imodel, 
          zc = zc,
          stopatzc = stopatzc, 
          deg_celsius = deg_celsius,
          basic_output = basic_output
        )
        
        # extract bloom date
        bloom_idx <- res$bloomindex
        bloom_date <- if (!is.null(bloom_idx) && bloom_idx > 0) {
          bd <- season_data$DATE[bloom_idx]
          bd <- bd[bd >= as.Date(paste0(forecast_year_j, "-01-01")) & bd <= preseason_end]
          if (length(bd) == 0) NA else bd
        } else {
          NA
        }
        
        # save bloom date (not index!)
        seasonal_bloom_dates[[j]][[m]][[k]] <- bloom_date
      }
    }
  }
  
  saveRDS(seasonal_bloom_dates, "seasonal_bloom_dates.rds")
  return(seasonal_bloom_dates)
  
}
