library(readr)
library(ggplot2)

giant_ci <- read_csv("csv/gams_training/macpyr_gams_training_historical_roms_0-30_5year.csv") |>
  mutate(
    site = as.factor(site),
    year_factor = as.factor(year - 2000)
  ) |>
  filter(
    latitude >= 33.8 & latitude <= 34.1,
    longitude >= -120.5 & longitude <= -119.5
  )

ggplot(giant_ci, aes(x = macpyr_log)) +
  geom_histogram(bins = 30, fill = "#440154", color = "white") +
  labs(x = "Log Kelp Density", y = "Count") +
  theme_bw()

giant_ci |>
  filter(year >= 2000) |>
  group_by(year) |>
  summarise(mean_macpyr_log = mean(macpyr_log, na.rm = TRUE)) |>
  ggplot(aes(x = year, y = mean_macpyr_log)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_point(color = "steelblue", size = 2) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "gray40", linewidth = 0.7) +
  scale_x_continuous(limits = c(2000, 2100), breaks = seq(2000, 2100, by = 20)) +
  scale_y_continuous(limits = c(0, 0.2)) +
  labs(x = "Year", y = "Mean Log Kelp Density") +
  theme_minimal()

heatwaves <- tibble(
  xmin = c(2014),
  xmax = c(2016),
  label = c("Blob")
)

giant_ci |>
  filter(year >= 2000) |>
  group_by(year) |>
  summarise(mean_macpyr_log = mean(macpyr_log, na.rm = TRUE)) |>
  ggplot(aes(x = year, y = mean_macpyr_log)) +
  geom_rect(data = heatwaves, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "firebrick", alpha = 0.15, inherit.aes = FALSE) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_point(color = "steelblue", size = 2) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "gray40", linewidth = 0.7) +
  scale_x_continuous(limits = c(2000, 2100), breaks = seq(2000, 2100, by = 20)) +
  labs(x = "Year", y = "Mean Log Kelp Density",) +
  theme_minimal()
