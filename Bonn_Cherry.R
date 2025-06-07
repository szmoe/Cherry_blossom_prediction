library(chillR)
Bonn_station_list_dwd<-handle_dwd(action="list_stations",
                                location=c(7.1, 50.8),
                                time_interval=c(20201231,20250330))

install.packages("GSODR")
library(GSODR)
germany <- get_GSOD(years = 2024, country = "Germany")
Bonn <- get_GSOD(years = 2024, station = "105130-99999")
write.csv(Bonn, file = "Bonn_GSODR.csv", row.names = FALSE)

install.packages("terra")
library(terra)
grib <- "UK_Met_forecast.grib"
past_forecast <- rast(grib)
df <- as.data.frame(past_forecast, xy = TRUE) 
write.csv(df, "Bonn_UKMet_forecast.csv", row.names = FALSE)


#--------------#


### Use Lars' codes (Compare historical and forecasts)

#rnomads is stupid, gribr is stupid, they all need a ton of extra software and I don't want that
library(tidyverse)

#downloaded for march
#test <- terra::rast('69d58496fb16850236ba61219dbe7c85.grib')
test <- terra::rast('Bonn_forecast_Jan_23-24-25.grib')

# terra::time(test)
# #terra::sources(test)
# terra::describe(test)

#           long  lat
pt <- cbind(7.1, 50.7)

test_extracted <- terra::extract(test, pt)
#temperatures are in K
test_extracted <- test_extracted %>% t()
rownames(test_extracted) <- NULL
test_out <- test_extracted %>% 
  as.data.frame() %>% 
  mutate(temperature = (V1 - 273.15) %>% round(digits = 2),
         V1 = round(V1, digits = 2),
         time = terra::time(test),
         hour = lubridate::hour(time), 
         date = lubridate::date(time))

#there are 50 ensemble members
nrow(test_out) / 50
test_out$model <- rep(1:50, each = 2176)

test_out %>% 
  mutate(model = as.factor(model),
         date = as.Date(time)) %>% 
  ggplot(aes(x = date, y = temperature)) +
  geom_line(aes(group = model, col = as.factor(model))) +
  scale_x_date(date_minor_breaks = "week", date_breaks = "2 week")

test_out %>% 
  mutate(model = as.factor(model),
         date = as.Date(time)) %>% 
  filter(model %in% c(1,2)) %>% 
  ggplot(aes(x = date, y = temperature)) +
  geom_line(aes(group = model, col = as.factor(model))) +
  scale_x_date(date_minor_breaks = "week", date_breaks = "2 week")



#---------------------------#
#compare with köln bonn weather
#----------------------------#

long <- 7.0871843
lat <- 50.7341602

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
data_clean$
  
  #downloaded for march
  test <- terra::rast('Bonn_forecast_Jan_23-24-25.grib')
#test <- terra::rast('UK_Met_forecast.grib')

#           long  lat
pt <- cbind(long, lat)

test_extracted <- terra::extract(test, pt)
#temperatures are in K
test_extracted <- test_extracted %>% t()
rownames(test_extracted) <- NULL
test_out <- test_extracted %>% 
  as.data.frame() %>% 
  mutate(temperature = (V1 - 273.15) %>% round(digits = 2),
         V1 = round(V1, digits = 2),
         time = terra::time(test),
         hour = lubridate::hour(time), 
         date = lubridate::date(time),
         yday = lubridate::yday(date),
         year = lubridate::year(date),
         season = ifelse(yday >= 274, yes = year+1, no = year))

#there are 50 ensemble members
row_per_model <- nrow(test_out) / 50
test_out$model <- rep(1:50, each = row_per_model)
test_out$id <- paste(test_out$model, test_out$season, sep = '--')

p1 <- test_out %>% 
  mutate(model = as.factor(model),
         date = as.Date(time)) %>% 
  #filter(model %in% c(1,2)) %>% 
  ggplot() +
  geom_line(aes(x = time, y = temperature,
                group = id, col = as.factor(id))) 
p1

hourtemps <- data_clean$`Köln/Bonn` %>% 
  chillR::stack_hourly_temps(latitude = lat) %>% 
  purrr::pluck('hourtemps') %>% 
  mutate(time = lubridate::ymd_h(paste(Year, Month, Day, Hour, sep = '-')),
         runn_mean = chillR::runn_mean(Temp, runn_mean = 24*7))

p1 +
  geom_line(data = hourtemps, aes(x = time, y = runn_mean)) +
  coord_cartesian(xlim = c(lubridate::ymd_h('2023-01-01 01'),
                           lubridate::ymd_h('2025-05-31 01')))


#summarize the seaon forecast data (median, 10% quant and 90%)

forecast_sum <- test_out %>% 
  mutate(
    hour = lubridate::hour(time), 
    date = lubridate::date(time),
    yday = lubridate::yday(date),
    year = lubridate::year(date),
    season = ifelse(yday >= 274, yes = year+1, no = year)) %>% 
  group_by(time,season) %>% 
  summarise(med_temp = median(temperature),
            q10_temp = quantile(temperature, probs = 0.05),
            q_90_temp = quantile(temperature, probs = 0.95)) %>% 
  ungroup() %>% 
  mutate(med_run = chillR::runn_mean(med_temp, runn_mean = 4 * 7),
         q10_run = chillR::runn_mean(q10_temp, runn_mean = 4 * 7),
         q90_run = chillR::runn_mean(q_90_temp, runn_mean = 4 * 7))

forecast_sum %>% 
  ggplot() +
  geom_ribbon(aes(x = time, ymin = q10_run, ymax = q90_run,
                  group = as.factor(season),
                  fill = 'Interval: Q05 to Q95'),
              alpha = 0.8) +
  geom_line(aes(x = time, y = med_run,
                group = as.factor(season),
                col = 'Forecast')) +
  geom_line(data = hourtemps, aes(x = time, y = runn_mean,
                                  col = 'Observed')) +
  coord_cartesian(xlim = c(lubridate::ymd_h('2021-11-01 01'),
                           lubridate::ymd_h('2024-05-01 01'))) +
  scale_fill_manual(values = 'grey70') +
  theme_bw()
ggsave('Bonn_Forecast_and_Predicted_2023_2024.jpeg',
       height = 15,
       width = 20,
       units = 'cm',
       device = 'jpeg')

p1

