library(readr)
library(ggplot2)
library(tidyverse)

giant_ci_pred <- read_csv("csv/gam_predictions/giant_se_future_predictions_300res_ci.csv")

species <- "giant"
region <- "se"
response <- "macpyr_log"
figure_folder <- paste0("figures/gam/kelp_projections/", species, "_", region)
model <- "gam"

# plot each of the points on a map
ggplot(giant_ci_pred, aes(x = longitude, y = latitude, color = macpyr_log)) +
  geom_point(alpha = 0.5) +
  scale_color_viridis_c() +
  labs(title = "Predicted log kelp density (future)") +
  theme_minimal()
ggsave(paste0(figure_folder, "/", species, "_", "_future_predictions_map_", model, ".png"), width = 8, height = 6)

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
  geom_point(alpha = 0.5) +
  scale_color_viridis_c() +
  facet_wrap(~ period, ncol = 2) +
  labs(title = "Mean predicted log kelp density by 25-year period (GAM)",
       color = "log kelp density") +
  theme_minimal()

ggsave(paste0(figure_folder, "/", species, "_future_predictions_map_periods_", model, ".png"), 
       width = 10, height = 8, dpi = 300)
