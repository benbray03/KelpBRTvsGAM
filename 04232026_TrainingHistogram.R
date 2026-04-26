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
