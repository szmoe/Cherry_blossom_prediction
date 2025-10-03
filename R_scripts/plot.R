library(chillR)
library(ggplot2)
library(lubridate)

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
      
      preseason_start <- as.Date(paste0(forecast_year - 1, "-10-01")) # bloom_dates change with this
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

plots <- list()

df <- bloom_dates_table_full

for(sea in 1:4){
  
  # Filter for season and lists <= 8
  df_season <- df %>%
    filter(season == sea, list <= 8) %>%
    mutate(
      list = factor(list, levels = y_levels)
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
  
  # Plot
  p <- ggplot(df_season, aes(y = list, x = bloom_JDay)) +
    geom_violin(data = df_season %>% filter(!(list %in% constant_lists)),
                aes(fill = list), trim = FALSE, scale = "width") +
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
    geom_point(data = df_season %>% filter(list %in% constant_lists),
               aes(y = list, x = bloom_JDay, fill = list),
               color = "black", size = 3, shape = 21) +
    geom_vline(data = legend_df, aes(xintercept = x, color = label),
               linetype = "dotted", linewidth = 1) +
    
    scale_fill_manual(
      name = "Forecast List",
      values = c("1" = "#E69F00", "2" = "#56B4E9", "3" = "#009E73",
                 "4" = "#F0E442", "5" = "#0072B2", "6" = "#D55E00",
                 "7" = "#CC79A7"),
      breaks = y_levels,
      labels = c("1" = "October", "2" = "November", "3" = "December",
                 "4" = "January", "5" = "February", "6" = "March",
                 "7" = "April")
    ) +
    
    scale_color_manual(
      name = "Legend",
      values = c("Observed bloom date" = "black"),
      breaks = "Observed bloom date",
      labels = "Observed bloom date",
      guide = "legend"
    ) +
    
    labs(
      y = "Forecast month list",
      x = "Bloom Julian Day",
      title = paste("Violin Plot of Bloom Julian Day for Season",
                    sea,
                    sprintf("[%d-%d]", min(df_season$forecast_year, na.rm = TRUE),
                            min(df_season$forecast_year, na.rm = TRUE) + 1))
    ) +
    scale_y_discrete(limits = rev(y_levels)) +
    scale_x_continuous(breaks = c(seq(from = min(df_season$bloom_JDay), 
                           to = 350, by = 25), jday_list8)) +
    theme_minimal() +
    # theme(
    #   legend.position = "bottom",
    #   legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    #   legend.title = element_text(face = "bold"),
    #   legend.text = element_text(size = 10)
    # )
    
    theme(axis.text.y = element_text(angle = 0, hjust = 1),
          legend.title = element_text(face = "bold"))
  
  plots[[paste0("season", sea)]] <- p
}

plots$season1
plots$season2
plots$season3
plots$season4
plots$season1 <- plots$season1 + theme(legend.position = "none")
#plots$season2 <- plots$season2  + theme(legend.position = "none")
plots$season3 <- plots$season3 + theme(legend.position = "none")
plots$season4 <- plots$season4  + theme(legend.position = "none")

combined_plot <- plots$season1 + plots$season2 + plots$season3 + plots$season4 +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(title = "Violin Plots of Bloom Julian Day for All Seasons") 

combined_plot <- (plots$season1 + plots$season2) / (plots$season3 + plots$season4) +
  plot_annotation(title = "Violin Plots of Bloom Julian Day for All Seasons")
combined_plot


####

## PLot the temp over the years

# unnest the lists
weather_combined <- readRDS("data/Bonn_hourly_forecast_four_seasons.rds")
hourly_forecast <- rbind(weather_combined

