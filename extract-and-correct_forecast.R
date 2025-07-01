library(tidyverse)
library(ncdf4)
library(CFtime)
#function to extract forecast from GRIB format

#function to extract the data from .nc file
#if you specify target_lat and target_lon, then the data is only extracted for the pixel closest to the coordinates
#otherwise all the pixel values will be extracted
#file is just the path to the .nc file. Usually, it should be located in the folder of your Rproject

#the function returns a data.frame with time, temperature, unit, target coordinates and actual coordinates of the pixel
extract_seasonal_forecast <- function(file, target_lat = NULL, target_lon = NULL){
  
  
  #open file
  nc <- ncdf4::nc_open("data/Bonn_forecast_test.nc")
  
  #check variables
  vars <- names(nc$var)
  #expect vars to be valid time and t2m
  if(any(vars != c("valid_time", "t2m"))) {
    warning(paste('Expected ncdf file to contain "valid_time", and "t2m" as variables. However, this file contains:', paste(vars, collapse = ', '), 
                  '\nExtracting values might malfunction for the variables you downloaded'))
  }
  
  #check dimensions
  dims <- names(nc$dim)
  if(any(dims != c("number", "forecast_reference_time", "forecast_period", "latitude", "longitude"))) {
    stop(paste('Expected dimensions of ncdf file to contain: "number", "forecast_reference_time", "forecast_period", "latitude", "longitude". However, this file contains:', paste(dims, collapse = ', '), 
                  '\nExtracting values might malfunction for the variables you downloaded'))
  }
  
  #extract dimensions
  #extract values stored in ncdf file
  lon <- ncdf4::ncvar_get(nc, "longitude")  # or "longitude"
  lat <- ncdf4::ncvar_get(nc, "latitude")  # or "latitude"
  number <- ncdf4::ncvar_get(nc,"number")
  fcst_ref_time <- ncdf4::ncvar_get(nc,"forecast_reference_time")
  fcst_period <- ncdf4::ncvar_get(nc,"forecast_period")
  
  #-----------------#
  #TIME
  #-----------------#
  
  #extract time
  time_array <- ncdf4::ncvar_get(nc,vars[1])
  time_units <- ncdf4::ncatt_get(nc,vars[1],"units")
  
  # decode time
  cf <- CFtime::CFtime(time_units$value, calendar = "proleptic_gregorian", time_array) # convert time to CFtime class
  timestamps <- CFtime::as_timestamp(cf) # get character-string times

  #bring into proper format
  time_cf <- CFtime::parse_timestamps(cf, timestamps)
  
  #------------------#
  #LOCATION
  #------------------#

  grd <-expand.grid(1:length(lon), 1:length(lat))
  lon_idx <- grd$Var1
  lat_idx <- grd$Var2
  
  if(!(is.null(target_lat) & is.null(target_lon))){
    

    if(!all(is.numeric(target_lat)) & all(is.numeric(target_lon))) {
      stop('Target latitude and longitude need to be numeric')
    }
    
    #check if lat and lon are covered
    covered_lon <- min(lon) <= min(target_lon) & max(target_lon) <= max(lon)
    covered_lat <- min(lat) <= min(target_lat) & max(target_lon) <= max(lat)
    
    if(covered_lon == FALSE | covered_lat == FALSE){
      stop('Target latitude or longitude not covered by downloaded grid. Either check if you downloaded the right area or check if the target coordinates are right')
    }
    
    lat_idx <- c()
    lon_idx <- c()
    #find closest pixel to the target coordinates
    for(i in 1:length(target_lat)){
      lon_i <- which.min(abs(lon - target_lon[i]))
      lat_i <- which.min(abs(lat - target_lat[i]))
      
      lat_idx <- c(lat_idx, lat_i)
      lon_idx <- c(lon_idx, lon_i)
    }
    


  }

  
  #-----------------#
  #TEMPERATURE
  #-----------------#
  
  #extract data
  temp_array <- ncdf4::ncvar_get(nc,vars[2])
  #extract unit
  temp_units <- ncdf4::ncatt_get(nc,vars[2],"units")
  
  #container for extracted data
  temp_df <- data.frame()
  
  print(dim(temp_array))  
  for(i in 1:length(number)){
    for(j in 1:length(lon_idx)){
      temp <- as.vector(temp_array[ , , i])
      
      int_df <- time_cf  %>% 
        mutate(temperature = round(temp, digits = 4),
               unit = temp_units$value,
               latitude = lat[lat_idx[j]],
               target_lat = target_lat[j],
               longitude = lon[lon_idx[j]],
               target_lon = target_lon[j],
               model = i)
      
      temp_df <- rbind(temp_df, int_df)
    }

    
  }
  
  ncdf4::nc_close(nc)
  
  temp_df %>% 
    mutate(Month = month,
           Year = year,
           Day = day,
           Hour = hour,
           temp = temperature) %>% 
    dplyr::select(Year, Month, Day, Hour, temp, unit, model, latitude, longitude, target_lat, target_lon) %>% 
    return()

}


#mean and variance adjustment
#input are two data.frames: observed weather and predicted weather
#observed weather can be hourly or daily. It should contain either a column called Tmean, Temp or Tmin and Tmax
#also it needs to contain columns called Month and Year

#predicted can be the same format as provided by the extraction function, just make sure
#it only contains the coordinates belonging to the observed data
#(so provide target_lat and target_lon when extracting)

#output will be of similar format as predicted, but with additional columns containing the correction factor and the
#corrected observation

#I would advise to supply temperature in K instead of C when doing the correction, because sub-zero temperatures can 
#create weird correction factors. 
#maybe I will ad an argument asking in what unit the temperature was supplied, so that the correction is done in K behind
#the scenes


mva_bias_correction_forecast <- function(observed, predicted){
  
  #summarize both on a monthly basis
  if(all(c('Month', 'Year') %in% colnames(observed)) == FALSE){
    stop('Columns "Month" and "Year" need to be present in object "observed"')
  }
  
  
  fcst_sum <- predicted %>% 
    group_by(Year, Month, model) %>% 
    summarise(mean_fcst = mean(temp) %>%  round(digits = 4)) %>% 
    mutate(year_mo = lubridate::ym(paste(Year, Month)))
    
  
  
  #select target column
  if('Temp' %in% colnames(observed)){
    observed$target_col = observed$Temp
  } else if('Tmean' %in% colnames(observed)){
    observed$target_col = observed$Tmean
  } else if(all(c("Tmin", "Tmax") %in% colnames(observed))){
    observed$target_col = (observed$Tmin + observed$Tmax)/2
  } else {
    stop('Temperature column with the name "Tmean", "Temp", "Tmin" together with "Tmax" need to be present in object "observed".')
  }
  

  #summarize temperature for each month
  observed_sum = observed %>% 
    mutate(year_mo = lubridate::ym(paste(Year, Month))) %>% 
    filter(year_mo %in% fcst_sum$year_mo) %>% 
    group_by(Year, Month) %>% 
    summarise(mean_obs = mean(target_col) %>%  round(digits = 4))
  
  #----------------#
  #correct monthly bias
  #----------------#
  #calculate mean temperature per month
  clim.obs = mean(observed_sum$mean_obs, na.rm = T)
  clim.fcst = mean(fcst_sum$mean_fcst, na.rm = T)
  
  #calculate sd per month
  sigma.e = sd(fcst_sum$mean_fcst, na.rm = T)
  sigma.ref = sd(observed_sum$mean_obs, na.rm = T)
  
  #corrected monthly temperature of forecast data
  fcst_call = ((fcst_sum$mean_fcst - clim.fcst) * (sigma.ref/sigma.e)) + clim.obs
  
  #add info to forecast summary
  fcst_sum$fcst_call = fcst_call

  #calc correction factor that is used to scale the daily observation of forecast
  fcst_sum <- fcst_sum %>% 
    mutate(corr_fact = abs(fcst_call / mean_fcst))
  
  
  predicted %>% 
    merge(fcst_sum,
          by = c('Year', 'Month', 'model')) %>% 
    mutate(temp_corrected = temp * corr_fact) %>% 
    return()
}



#here is a example how I used the functions

#extract data from the nc file
file <- 'data/Bonn_forecast_test.nc'

# Open NetCDF file
nc <- nc_open('data/Bonn_forecast_test.nc')

# Extract lat and lon
lats <- ncvar_get(nc, "latitude")
lons <- ncvar_get(nc, "longitude")
cat("Latitude range:", range(lats), "\n")
cat("Longitude range:", range(lons), "\n")

#coordiantes of the weahter station
target_lat <- 50.62
target_lon <- 7

#extract data from forecast
predicted <- extract_seasonal_forecast(file,target_lat, target_lon)

# download observed weather
long <- 7
lat <- 50.62

weather_dwd <- chillR::handle_dwd(action = 'list_stations', location = c(long, lat), 
                                  time_interval = c(19800101, 20251231))

data <- chillR::handle_dwd(action = "download_weather",
                           location = weather_dwd[1 : 3, "Station_ID"],
                           time_interval = c(19800101, 20250531),
                           stations_to_choose_from = 25,
                           station_list = weather_dwd,
                           drop_most = TRUE,
                           add.DATE = FALSE,
                           quiet = TRUE,
                           add_station_name = FALSE)

data_clean <- chillR::handle_dwd(data)
View(data_clean)
names(data_clean)
str(data_clean)
data <- data_clean[["Bonn-Friesdorf"]] # lots of NA
data1 <- data_clean[["NeuenahrBad-Ahrweiler"]]

write.csv(data1, "data/NeuenahrBad_Ahrweiler.csv", row.names = FALSE)

#observed weather, downloaded with chillR
observed <- read.csv('data/NeuenahrBad_Ahrweiler.csv')

#calculate Tmean, also bring to K because the bias correction works better when we avoid sub-zero values
#alos the forecast temperature is currently in K
observed$Tmean = 273.15  + (observed$Tmax + observed$Tmin) / 2

#run bias correction
predicted_bias_corrected <- mva_bias_correction_forecast(observed, predicted)


#prepare observed data so it can be plotted together with extracted
observed_sub <- observed %>% 
  mutate(Date = lubridate::ymd_h(paste(Year, Month, Day, 12))) %>% 
  filter(Year == 2024, Month == 11)


predicted_bias_corrected %>% 
  mutate(Date = lubridate::ymd_h(paste(Year, Month, Day, Hour))) %>% 
  filter(model %in% 1:3) %>% 
  filter(Year == 2024, Month %in% c(11)) %>% 
  ggplot(aes(x = Date)) +
  geom_line(aes(y = temp_corrected - 273.15, col = as.factor(model),
                linetype = 'corrected')) +
  geom_line(aes(y = temp - 273.15, col = as.factor(model),
                linetype = 'raw')) +
  geom_line(data = observed_sub, 
            aes(x = Date, y = Tmean - 273.15, 
                linetype = 'observed'), col = 'black') 


observed_sub <- observed %>% 
  mutate(Date = lubridate::ymd_h(paste(Year, Month, Day, 12))) %>% 
  filter(Year == 2024, Month == 12)

predicted_bias_corrected %>% 
  mutate(Date = lubridate::ymd_h(paste(Year, Month, Day, Hour))) %>% 
  filter(model %in% 1:3) %>% 
  filter(Year == 2024, Month %in% c(12)) %>% 
  ggplot(aes(x = Date)) +
  geom_line(aes(y = temp_corrected - 273.15, col = as.factor(model),
                linetype = 'corrected')) +
  geom_line(aes(y = temp - 273.15, col = as.factor(model),
                linetype = 'raw')) +
  geom_line(data = observed_sub, 
            aes(x = Date, y = Tmean - 273.15, 
                linetype = 'observed'), col = 'black') 




















