library(chillR)
Bonn_station_list_dwd<-handle_dwd(action="list_stations",
                                location=c(7.1, 50.8),
                                time_interval=c(20201231,20250330))

install.packages("GSODR")
library(GSODR)
germany <- get_GSOD(years = 2024, country = "Germany")
Bonn <- get_GSOD(years = 2024, station = "105130-99999")
write.csv(Bonn, file = "Bonn_GSODR.csv", row.names = FALSE)
