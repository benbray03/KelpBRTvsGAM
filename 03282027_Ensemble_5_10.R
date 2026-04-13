library(readr)
library(dplyr)
library(zoo)

bull_nc_raw <- read_csv("brt_projections_by_rock/bull_northcentral_future_projections_300res.csv")

bull_nc_5_plot <- bull_nc_raw %>%
  filter(model %in% c("hadl", "gfdl", "ipsl")) %>%
  mutate(year_bin = ceiling(year / 5) * 5) %>%
  group_by(longitude, latitude, year_bin, model) %>%
  summarise(nerlue_by_rock = mean(nerlue_by_rock, na.rm = TRUE), .groups = "drop") %>%
  # now compute ensemble mean and variance across models
  group_by(year_bin) %>%
  summarise(
    total_abundance = sum(nerlue_by_rock, na.rm = TRUE),       # sum across all sites and models
    model_variance  = var(nerlue_by_rock, na.rm = TRUE),        # variance across models
    .groups = "drop"
  )

write_csv(bull_nc_5_plot, "brt_projections_by_rock/bull_nc_5yr_barplot.csv")

# ensemble mean across models
bull_nc_ensemble <- bull_nc_raw %>%
  group_by(longitude, latitude, year) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>%
  mutate(model = "ensemble")
write_csv(bull_nc_ensemble, "brt_projections_by_rock/bull_nc_future_projections_300res_ensemble.csv")

# 5-year intervals
bull_nc_5 <- bull_nc_ensemble %>%
  mutate(year_bin = ceiling(year / 5) * 5) %>%
  group_by(longitude, latitude, year_bin) %>%
  summarise(across(where(is.numeric) & !matches("^year$"), mean, na.rm = TRUE), .groups = "drop") %>%
  rename(year = year_bin)
write_csv(bull_nc_5, "brt_projections_by_rock/bull_nc_future_projections_300res_ensemble_5yr.csv")

# 10-year intervals
# bull_nc_10 <- bull_nc_ensemble %>%
#   mutate(year_bin = ceiling(year / 10) * 10) %>%
#   group_by(longitude, latitude, year_bin) %>%
#   summarise(across(where(is.numeric) & !matches("^year$"), mean, na.rm = TRUE), .groups = "drop") %>%
#   rename(year = year_bin)
# write_csv(bull_nc_10, "brt_projections_by_rock/bull_nc_future_projections_300res_ensemble_10yr.csv")

# --- Giant ----
giant_csw_raw <- read_csv("brt_projections_by_rock/giant_csw_future_projections_300res.csv")
giant_se_raw <- read_csv("brt_projections_by_rock/giant_se_future_projections_300res.csv")
giant_raw <- bind_rows(giant_csw_raw, giant_se_raw)

giant_ensemble <- giant_raw %>%
  group_by(longitude, latitude, year) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>%
  mutate(model = "ensemble")
write_csv(giant_ensemble, "brt_projections_by_rock/giant_ensemble_future_projections_300res.csv")

giant_5 <- giant_ensemble %>%
  mutate(year_bin = ceiling(year / 5) * 5) %>%
  group_by(longitude, latitude, year_bin) %>%
  summarise(across(where(is.numeric) & !matches("^year$"), mean, na.rm = TRUE), .groups = "drop") %>%
  rename(year = year_bin)
giant_5 <- giant_5 %>%
  filter(latitude < 37.1)

write_csv(giant_5, "brt_projections_by_rock/giant_ensemble_future_projections_300res_5yr.csv")

giant_5_plot <- giant_raw %>%
  filter(model %in% c("hadl", "gfdl", "ipsl")) %>%
  mutate(year_bin = ceiling(year / 5) * 5) %>%
  group_by(longitude, latitude, year_bin, model) %>%
  summarise(macpyr_by_rock = mean(macpyr_by_rock, na.rm = TRUE), .groups = "drop") %>%
  # now compute ensemble mean and variance across models
  group_by(year_bin) %>%
  summarise(
    total_abundance = sum(macpyr_by_rock, na.rm = TRUE),       # sum across all sites and models
    model_variance  = var(macpyr_by_rock, na.rm = TRUE),        # variance across models
    .groups = "drop"
  )

write_csv(giant_5_plot, "brt_projections_by_rock/giant_5yr_barplot.csv")
