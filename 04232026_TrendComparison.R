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

# GAM plot
ggplot(gam_plot_data, aes(x = year, y = annual_mean, color = model)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~ model, nrow = 1) +
  labs(x = "Year", y = "Mean predicted kelp density (log)",
       color = "ESM", title = "GAM — Giant Kelp") +
  theme_classic()
ggsave("figures/gam_giant_se_ensemble_ts.png", width = 10, height = 4, dpi = 300)

# BRT plot
ggplot(brt_plot_data, aes(x = year, y = annual_mean, color = model)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~ model, nrow = 1) +
  labs(x = "Year", y = "Mean predicted kelp density (log)",
       color = "ESM", title = "BRT — Giant Kelp") +
  theme_classic() + 
  theme(legend.position = "none")
ggsave("figures/brt_giant_se_ensemble_ts.png", width = 10, height = 4, dpi = 300)