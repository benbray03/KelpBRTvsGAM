library(readr)
library(ggplot2)
library(sf)

giant_ci <- read_csv("csv/gams_training/macpyr_gams_training_historical_roms_0-30_5year.csv")

california <- st_read("shp/ca_hires_WGS.shp")

ggplot(giant_ci, aes(x = longitude, y = latitude, color = macpyr_log)) +
  geom_sf(data = california, fill = "gray80", color = "gray50", linewidth = 0.3, inherit.aes = FALSE) +
  geom_point(alpha = 0.5, size = 0.8) +
  scale_color_viridis_c(direction = -1) +
  coord_sf(
  ) +
  labs(x = "Longitude", y = "Latitude",
       color = "Log Kelp Density") +
  theme_void() +
  theme(legend.position = "none")


ggplot(giant_ci, aes(x = longitude, y = latitude)) +
  geom_sf(data = california, fill = "gray80", color = "gray50", linewidth = 0.3, inherit.aes = FALSE) +
  geom_point(alpha = 0.5, size = 0.8, color = "#3B528B") +
  coord_sf(
    xlim = c(-120, -119.4),
    ylim = c(33.88, 34.10)
  ) +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw() +
  theme(legend.position = "none")
