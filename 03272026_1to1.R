library(ggplot2)
library(readr)

brt_historical <- read_csv("csv/brt_predictions/giant_se_historical_predictions_300res_ci.csv")
gam_historical <- read_csv("csv/gam_predictions/giant_se_historical_predictions_300res_ci.csv")

colnames(brt_historical)
colnames(gam_historical)
nrow(brt_historical)
nrow(gam_historical)

# Join by shared location and year columns
comparison <- brt_historical |>
  select(latitude, longitude, year, macpyr_log) |>
  rename(pred_brt = macpyr_log) |>
  inner_join(
    gam_historical |>
      select(latitude, longitude, year, macpyr_log) |>
      rename(pred_gam = macpyr_log),
    by = c("latitude", "longitude", "year")
  )

# One to one plot
ggplot(comparison, aes(x = pred_brt, y = pred_gam)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(x = "BRT predicted (log kelp density)",
       y = "GAM predicted (log kelp density)",
       title = "BRT vs GAM historical predictions - Channel Islands") +
  theme_classic()

ggsave("figures/giant_se_brt_vs_gam_1to1.png",
       width = 6, height = 5, dpi = 300)
