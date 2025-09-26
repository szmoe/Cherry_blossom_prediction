interpolate_hourly_forecast <- function(bias_corrected_df,
                                        latitude,
                                        daily_temps = NULL,
                                        interpolate_remaining = TRUE,     
                                        return_extremes = FALSE,
                                        minimum_values_for_solving = 4,
                                        daily_patch_max_mean_bias = NA,
                                        daily_patch_max_stdev_bias = NA) {
  

  df_model <- dplyr::rename(bias_corrected_df, Temp = temp_corrected)
  

  hourly_input <- dplyr::select(df_model, Year, Month, Day, Hour, Temp)

  forecast_hourly <- chillR::interpolate_gaps_hourly(
    hourtemps = hourly_input,
    latitude = latitude,
    daily_temps = daily_temps,
    interpolate_remaining = interpolate_remaining,     
    return_extremes = return_extremes,
    minimum_values_for_solving = minimum_values_for_solving,
    daily_patch_max_mean_bias = daily_patch_max_mean_bias,
    daily_patch_max_stdev_bias = daily_patch_max_stdev_bias
  )
  

  weather <- forecast_hourly$weather
  weather$DATE <- lubridate::ymd(paste(weather$Year, weather$Month, weather$Day))
  weather$YEARMODA <- sprintf("%04d%02d%02d", weather$Year, weather$Month, weather$Day)
  weather$model <- unique(df_model$model)
  
  hourly_forecast_data <- dplyr::select(
    weather, DATE, YEARMODA, Year, Month, Day, Hour, Temp, model
  )
  
  return(hourly_forecast_data)
}
