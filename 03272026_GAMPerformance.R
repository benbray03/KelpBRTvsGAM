# Compare 

library(readr)
library(ggplot2)
library(dplyr)

giant_ci_observed <- read_csv("csv/gams_training/macpyr_gams_training_historical_roms_0-30_5year.csv")
gam_historical <- read_csv("csv/gam_predictions/giant_se_historical_predictions_300res_ci.csv")

range(gam_historical$year)

# Summarize observed by year
observed_summary <- giant_ci_observed |>
  group_by(year) |>
  summarise(
    mean = mean(macpyr_log, na.rm = TRUE),
    se = sd(macpyr_log, na.rm = TRUE) / sqrt(n())
  ) |>
  mutate(source = "Observed")

# Summarize GAM predictions by year
gam_summary <- gam_historical |>
  group_by(year) |>
  summarise(
    mean = mean(macpyr_log, na.rm = TRUE),
    se = sd(macpyr_log, na.rm = TRUE) / sqrt(n())
  ) |>
  mutate(source = "GAM Predicted")

# Combine with observed
bar_comparison_gam <- bind_rows(observed_summary, gam_summary) |>
  filter(year %in% intersect(unique(observed_summary$year), unique(gam_summary$year)))

# Plot
ggplot(bar_comparison_gam, aes(x = year, y = mean, fill = source)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge(0.9),
                width = 0.2) +
  scale_fill_manual(values = c("Observed" = "steelblue", "GAM Predicted" = "orange2")) +
  labs(x = "Year",
       y = "Mean log kelp density",
       fill = "Data source",
       title = "GAM Predicted vs Observed - Channel Islands") +
  theme_classic()

ggsave("figures/gam/kelp_projections/giant_se/giant_se_gam_observed_bars.png",
       width = 8, height = 5, dpi = 300)

