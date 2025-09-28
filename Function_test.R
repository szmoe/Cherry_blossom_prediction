
source("seasonal_bloom_prediction_function.R")

##====================
# Bonn/ One season
##====================

bonn_bloom <- seasonal_bloom_prediction( month = 3,
                                         area = c(50.78, 7, 50.62, 7.2),
                                         download_year = 2024,
                                         target_lat = 50.62,
                                         target_lon = 7,
                                         observed_time_interval = c(20230101, 20241231),
                                         mean_time_interval = c(20200101, 20241231),
                                         forecast_year = 2024,
                                         season_start = "-08-01",
                                         season_end = "-06-30",
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
                                         zc = 180.8255616 )


bonn_bloom <- readRDS("seasonal_bloom_dates.rds")
bonn_phenoflex <- readRDS("phenoflex_data.rds")
bonn_data <- readRDS("seasonal_hourly_patched_data.rds")

##====================
# Bonn/ Two seasons
##====================

bonn_bloom2 <- seasonal_bloom_prediction( month = 3,
                                         area = c(50.78, 7, 50.62, 7.2),
                                         download_year = c(2023,2024),
                                         target_lat = 50.62,
                                         target_lon = 7,
                                         observed_time_interval = c(20230101, 20241231),
                                         mean_time_interval = c(20200101, 20241231),
                                         forecast_year = c(2023,2024),
                                         season_start = "-08-01",
                                         season_end = "-06-30",
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
                                         zc = 180.8255616 )

bonn_bloom2 <- readRDS("data/Bonn2_seasonal_bloom_dates.rds")
bonn_phenoflex2 <- readRDS("Bonn2_phenoflex_data.rds")
bonn_data2 <- readRDS("Bonn2_seasonal_hourly_patched_data.rds")
bonn_observed2 <- readRDS("Bonn2_seasonal_hourly_observed_data.rds")

##==========================================
# Bonn/ four seasons/ seven forecast months
##==========================================

bonn_bloom3 <- seasonal_bloom_prediction( month = c(10, 11, 12, 1, 2, 3, 4) ,
                                          area = c(50.78, 7, 50.62, 7.2),
                                          download_year = c(2020, 2021, 2022, 2023),
                                          target_lat = 50.62,
                                          target_lon = 7,
                                          observed_time_interval = c(20200101, 20241231),
                                          mean_time_interval = c(20200101, 20241231),
                                          forecast_year = c(2021, 2022, 2023, 2024),
                                          season_start = "-08-01",
                                          season_end = "-06-30",
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
                                          zc = 180.8255616 )


##========================================
# Berlin/ Two seasons/ Two forecast months
##========================================

berlin_bloom <- seasonal_bloom_prediction( month = c(2,3),
                                          area = c(52.6755, 13.4, 52.5, 13.7612),
                                          download_year = c(2023,2024),
                                          target_lat = 52.5,
                                          target_lon = 13.4,
                                          observed_time_interval = c(20230101, 20241231),
                                          mean_time_interval = c(20200101, 20241231),
                                          forecast_year = c(2023,2024),
                                          season_start = "-08-01",
                                          season_end = "-06-30",
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
                                          zc = 180.8255616 )

Berlin_bloom <- readRDS("data/Berlin_seasonal_bloom_dates.rds")




