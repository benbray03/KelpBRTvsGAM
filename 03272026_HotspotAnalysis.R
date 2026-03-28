library(readr)
library(ggplot2)

giant_ci_pred <- read_csv("csv/gam_predictions/giant_se_future_predictions_300res_ci.csv")

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

ggplot()+ 
  geom_histogram(data = giant_ci_pred_periods, aes(x = macpyr_log), fill = "steelblue", color = "black")

giant_ci_historical <- giant_ci_pred_periods |> 
  filter(period == "2001-2025") |> 
  group_by(latitude, longitude) |> 
  summarise(macpyr_log = mean(macpyr_log, na.rm = TRUE))  

ggplot() + 
  geom_histogram(data = giant_ci_historical, aes(x = macpyr_log), fill = "steelblue", color = "black")

giant_ci_near <- giant_ci_pred_periods |>
  filter(period == "2026-2050") |>
  group_by(latitude, longitude) |>
  summarise(macpyr_log = mean(macpyr_log, na.rm = TRUE))

ggplot() +
  geom_histogram(data = giant_ci_near, aes(x = macpyr_log), fill = "steelblue", color = "black")

# Get tertile thresholds from historical period
historical_tertiles <- quantile(giant_ci_historical$macpyr_log, 
                                probs = c(1/3, 2/3), na.rm = TRUE)
near_tertiles <- quantile(giant_ci_near$macpyr_log, 
                          probs = c(1/3, 2/3), na.rm = TRUE)

giant_ci_mid <- giant_ci_pred_periods |>
  filter(period == "2051-2075") |>
  group_by(latitude, longitude) |>
  summarise(macpyr_log = mean(macpyr_log, na.rm = TRUE))

# Join historical and near future by location
scatter_comparison <- giant_ci_historical |>
  rename(macpyr_log_hist = macpyr_log) |>
  inner_join(
    giant_ci_mid |> rename(macpyr_log_near = macpyr_log),
    by = c("latitude", "longitude")
  )



# Plot
ggplot(scatter_comparison, aes(x = macpyr_log_hist, y = macpyr_log_near)) +
  geom_point(alpha = 1, color = "steelblue") +
  geom_vline(xintercept = historical_tertiles[1], linetype = "dashed", color = "red3") +
  geom_vline(xintercept = historical_tertiles[2], linetype = "dashed", color = "orange2") +
  geom_hline(yintercept = historical_tertiles[1], linetype = "dashed", color = "red3") +
  geom_hline(yintercept = historical_tertiles[2], linetype = "dashed", color = "orange2") +
  geom_abline(slope = 1, intercept = 0, color = "gray50") +
  labs(x = "Historical (2001-2025) mean log kelp density",
       y = "Mid future (2051-2075) mean log kelp density",
       title = "Historical vs Near Future kelp density - Channel Islands") +
  theme_classic()

ggsave("figures/giant_se_gam_hist_vs_near_scatter.png",
       width = 7, height = 6, dpi = 300)