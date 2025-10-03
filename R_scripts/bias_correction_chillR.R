library(chillR)

# Read csv
Bonn <- read.csv("data/Bonn_forecast_2021_2024.csv")

# This add DATE column in R format (error coz no Tmin and Tmax)
View(Bonn)
# Bonn_chillR <- Bonn %>%
#   make_all_day_table()

# Interpolate the gaps for Temperature (output: interp- numeric vector with all gaps
# linearly interpolated and missing- boolean vector, either TRUE (gap) or FALSE
# (no gap))
T_forecast_int <- interpolate_gaps(Bonn[, "V1"])

# Add interpolated to weather dataset
Bonn_inter <- Bonn %>%
  mutate(T_interpolated = T_forecast_int)


# fill in all gaps
fixed_all_days <- Bonn %>%
  fix_weather()

# Check the quality control summary with fix_weather()
Bonn_QC <- fix_weather(Bonn)$QC

