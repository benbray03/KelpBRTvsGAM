library(readr)
library(dplyr)
library(ggplot2)


gam <- read_csv("csv/gam_predictions/giant_se_future_predictions_300res_ci.csv")
brt <- read_csv("csv/brt_predictions/giant_se_future_predictions_300res.csv")

# GAM annual mean by ESM + ensemble
gam_ensemble <- gam %>%
  group_by(latitude, longitude, year, model) %>%
  summarize(ensemble = mean(macpyr_log), .groups = "drop") %>%
  group_by(year, model) %>%
  summarize(annual_mean = mean(ensemble), .groups = "drop")

gam_esm_ensemble <- gam_ensemble %>%
  group_by(year) %>%
  summarize(annual_mean = mean(annual_mean), .groups = "drop") %>%
  mutate(model = "ensemble")

gam_plot_data <- bind_rows(gam_esm_ensemble, gam_ensemble) %>%
  mutate(model = factor(model, levels = c("ensemble", "gfdl", "hadl", "ipsl")))

# BRT annual mean by ESM + ensemble
brt_ensemble <- brt %>%
  group_by(latitude, longitude, year, model) %>%
  summarize(ensemble = mean(macpyr_log), .groups = "drop") %>%
  group_by(year, model) %>%
  summarize(annual_mean = mean(ensemble), .groups = "drop")

brt_esm_ensemble <- brt_ensemble %>%
  group_by(year) %>%
  summarize(annual_mean = mean(annual_mean), .groups = "drop") %>%
  mutate(model = "ensemble")

brt_plot_data <- bind_rows(brt_esm_ensemble, brt_ensemble) %>%
  mutate(model = factor(model, levels = c("ensemble", "gfdl", "hadl", "ipsl")))

vlines <- c(2025, 2050, 2075)
y_limits <- range(c(gam_esm_ensemble$annual_mean, brt_esm_ensemble$annual_mean))

# GAM plot
ggplot(gam_esm_ensemble, aes(x = year, y = annual_mean)) +
  geom_vline(xintercept = vlines, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_line(linewidth = 1, color = "#1f78b4") +
  coord_cartesian(ylim = y_limits) +
  labs(x = "Year", y = "Mean Predicted Kelp Density (log)",
       title = "GAM — Giant Kelp (Ensemble)") +
  theme_classic()
ggsave("figures/gam_giant_se_ensemble_ts.png", width = 14, height = 4, dpi = 300)

# BRT plot
ggplot(brt_esm_ensemble, aes(x = year, y = annual_mean)) +
  geom_vline(xintercept = vlines, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_line(linewidth = 1, color = "#e66101") +
  coord_cartesian(ylim = y_limits) +
  labs(x = "Year", y = "Mean Predicted Kelp Density (log)",
       title = "BRT — Giant Kelp (Ensemble)") +
  theme_classic()
ggsave("figures/brt_giant_se_ensemble_ts.png", width = 14, height = 4, dpi = 300)
