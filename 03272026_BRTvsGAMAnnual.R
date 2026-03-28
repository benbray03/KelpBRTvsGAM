library(readr)
library(ggplot2)

gam_historical <- read_csv("csv/gam_predictions/giant_se_future_predictions_300res_ci.csv")
brt_historical <- read_csv("csv/brt_predictions/giant_se_future_predictions_300res_ci.csv")


# Summarize BRT predictions by year
brt_summary <- brt_historical |>
  group_by(year) |>
  summarise(
    mean = mean(macpyr_log, na.rm = TRUE),
    se = sd(macpyr_log, na.rm = TRUE) / sqrt(n())
  ) |>
  mutate(source = "BRT Predicted")

# Summarize GAM predictions by year
gam_summary <- gam_historical |>
  group_by(year) |>
  summarise(
    mean = mean(macpyr_log, na.rm = TRUE),
    se = sd(macpyr_log, na.rm = TRUE) / sqrt(n())
  ) |>
  mutate(source = "GAM Predicted")

# Combine with observed
bar_comparison_gam <- bind_rows(brt_summary, gam_summary) |>
  filter(year %in% intersect(unique(brt_summary$year), unique(gam_summary$year)))

# Plot
ggplot(bar_comparison_gam, aes(x = year, y = mean, fill = source)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge(0.9),
                width = 0.2) +
  scale_fill_manual(values = c("BRT Predicted" = "steelblue", "GAM Predicted" = "orange2")) +
  labs(x = "Year",
       y = "Mean log kelp density",
       fill = "Data source",
       title = "GAM vs BRT - Channel Islands") +
  theme_classic()

ggsave("figures/giant_se_gam_brt_bars.png",
       width = 8, height = 5, dpi = 300)

