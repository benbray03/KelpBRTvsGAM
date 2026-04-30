library(sf)
library(ggplot2)

california <- st_read("shp/california.shp")

map_df <- gam %>%
  group_by(latitude, longitude) %>%
  summarise(gam_mean = mean(macpyr_log, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    brt %>%
      group_by(latitude, longitude) %>%
      summarise(brt_mean = mean(macpyr_log, na.rm = TRUE), .groups = "drop"),
    by = c("latitude", "longitude")
  ) %>%
  mutate(
    diff       = brt_mean - gam_mean,
    brt_higher = diff > 0
  )

ggplot(map_df, aes(x = longitude, y = latitude, color = brt_higher)) +
  geom_point(alpha = 0.6, size = 1.5) +
  scale_color_manual(
    values = c("TRUE" = "#006E90", "FALSE" = "#F18F01"),
    labels = c("TRUE" = "BRT > GAM", "FALSE" = "GAM > BRT")
  ) +
  coord_sf(
  ) +
  labs(
    x = "Longitude", y = "Latitude",
    color = "Model Comparison"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

