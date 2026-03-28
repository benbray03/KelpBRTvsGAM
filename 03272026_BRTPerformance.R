# Compare BRT Predictions to historical ROMS models (NOT USEFUL)

library(readr)

giant_ci_observed <- read_csv("csv/gams_training/macpyr_gams_training_historical_roms_0-30_5year.csv")
brt_historical <- read_csv("csv/brt_predictions/giant_se_historical_predictions_300res_ci.csv")

range(giant_ci_observed$year)
range(brt_historical$year)

# Summarize observed by year
observed_summary <- giant_ci_observed |>
  group_by(year) |>
  summarise(
    mean = mean(macpyr_log, na.rm = TRUE),
    se = sd(macpyr_log, na.rm = TRUE) / sqrt(n())
  ) |>
  mutate(source = "Observed")

# Summarize BRT predictions by year
brt_summary <- brt_historical |>
  group_by(year) |>
  summarise(
    mean = mean(macpyr_log, na.rm = TRUE),
    se = sd(macpyr_log, na.rm = TRUE) / sqrt(n())
  ) |>
  mutate(source = "BRT Predicted")

# Combine
bar_comparison <- bind_rows(observed_summary, brt_summary)

# Check overlapping years
observed_years <- unique(observed_summary$year)
brt_years <- unique(brt_summary$year)
shared_years <- intersect(observed_years, brt_years)
cat("Shared years:", shared_years, "\n")

# Filter to shared years only
bar_comparison <- bar_comparison |>
  filter(year %in% shared_years)

# Plot
ggplot(bar_comparison, aes(x = year, y = mean, fill = source)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge(0.9),
                width = 0.2) +
  scale_fill_manual(values = c("Observed" = "steelblue", "BRT Predicted" = "orange2")) +
  labs(x = "Year",
       y = "Mean log kelp density",
       fill = "Data source",
       title = "BRT Predicted vs Observed - Channel Islands") +
  theme_classic()

ggsave("figures/brt/kelp_projections/giant_se/giant_se_brt_observed_bars.png",
       width = 8, height = 5, dpi = 300)
