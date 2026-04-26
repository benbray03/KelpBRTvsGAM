library(sf)

california <- st_read("shp/california.shp")

ggplot(map_df, aes(x = longitude, y = latitude, color = brt_higher)) +
  geom_sf(data = california, fill = "gray80", color = "gray50", linewidth = 0.3, inherit.aes = FALSE) +
  geom_point(alpha = 0.6, size = 1.5) +
  scale_color_manual(
    values = c("TRUE" = "#2ca25f", "FALSE" = "#e34a33"),
    labels = c("TRUE" = "BRT > GAM", "FALSE" = "GAM > BRT")
  ) +
  coord_sf(
    xlim = range(map_df$longitude),
    ylim = range(map_df$latitude)
  ) +
  labs(x = "Longitude", y = "Latitude", color = "Model Comparison") +
  theme_minimal() +
  theme(legend.position = "none ")

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
    values = c("TRUE" = "#2ca25f", "FALSE" = "#e34a33"),
    labels = c("TRUE" = "BRT > GAM", "FALSE" = "GAM > BRT")
  ) +
  coord_sf(
  ) +
  labs(
    x = "Longitude", y = "Latitude",
    color = "Model Comparison"
  ) +
  theme_minimal() +
  theme(legend.position = "left")


# Round coords before joining
map_df <- gam %>%
  mutate(latitude = round(latitude, 4), longitude = round(longitude, 4)) %>%
  group_by(latitude, longitude) %>%
  summarise(gam_mean = mean(macpyr_log, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    brt %>%
      mutate(latitude = round(latitude, 4), longitude = round(longitude, 4)) %>%
      group_by(latitude, longitude) %>%
      summarise(brt_mean = mean(macpyr_log, na.rm = TRUE), .groups = "drop"),
    by = c("latitude", "longitude")
  ) %>%
  mutate(diff = brt_mean - gam_mean, brt_higher = diff > 0)

map_df <- gam %>%
  group_by(latitude, longitude) %>%
  summarise(gam_mean = mean(macpyr_log, na.rm = TRUE), .groups = "drop") %>%
  inner_join(
    brt %>%
      group_by(latitude, longitude) %>%
      summarise(brt_mean = mean(macpyr_log, na.rm = TRUE), .groups = "drop"),
    by = c("latitude", "longitude")
  ) %>%
  mutate(
    diff       = brt_mean - gam_mean,
    brt_higher = diff > 0
  )

# Check how many cells survive the join
nrow(map_df)
