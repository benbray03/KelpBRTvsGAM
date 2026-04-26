library(readr)
library(ggplot2)
library(tidyverse)
library(sf)

giant_ci_pred <- read_csv("csv/brt_predictions/giant_se_future_predictions_300res_ci.csv")

species <- "giant"
region <- "se"
response <- "macpyr_log"
figure_folder <- paste0("figures/brt/kelp_projections/", species, "_", region)
model <- "brt"
california <- st_read("shp/california.shp")

# plot each of the points on a map
ggplot(giant_ci_pred, aes(x = longitude, y = latitude, color = macpyr_log)) +
  geom_point(alpha = 0.5) +
  scale_color_viridis_c() +
  labs(title = "Predicted log kelp density (future)") +
  theme_minimal()
ggsave(paste0(figure_folder, "/", species, "_", region, "_future_predictions_map_", model, ".png"), width = 8, height = 6)

giant_ci_pred_periods <- giant_ci_pred |> 
  mutate(period = case_when(
    year >= 2001 & year <= 2025 ~ "2001-2025",
    year >= 2026 & year <= 2050 ~ "2026-2050",
    year >= 2051 & year <= 2075 ~ "2051-2075",
    year >= 2076 & year <= 2100 ~ "2076-2100"
  )) |> 
  filter(!is.na(period)) |> 
  group_by(latitude, longitude, period) |> 
  summarise(macpyr_log = mean(macpyr_log, na.rm = TRUE)) |> 
  mutate(period = factor(period, levels = c("2001-2025", "2026-2050", "2051-2075", "2076-2100")))

ggplot(giant_ci_pred_periods, aes(x = longitude, y = latitude, color = macpyr_log)) +
  geom_sf(data = california, fill = "gray80", color = "gray50", linewidth = 0.3, inherit.aes = FALSE) +
  geom_point(alpha = 0.5) +
  scale_color_viridis_c(direction = -1) +
  facet_wrap(~ period, ncol = 1) +
  labs(x = "Longitude", y = "Latitude",
       color = "Log Kelp Density") +
  coord_sf(
    xlim = range(giant_ci_pred_periods$longitude),
    ylim = range(giant_ci_pred_periods$latitude)
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(-120, -119.4, by = 0.2)) +
  theme(legend.position = "left")

ggsave(paste0(figure_folder, "/", species, "_future_predictions_map_periods_", model, ".png"), 
       width = 10, height = 8, dpi = 300)
