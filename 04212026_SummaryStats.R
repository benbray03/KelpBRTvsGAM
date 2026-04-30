library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggrepel)

# ============================================================
gam <- read_csv("csv/gam_predictions/giant_se_future_predictions_300res_ci.csv")
brt <- read_csv("csv/brt_predictions/giant_se_future_predictions_300res_ci.csv")

# ============================================================
gam_summary <- gam %>%
  group_by(longitude, latitude) %>%
  summarise(mean_pred = mean(macpyr_log, na.rm = TRUE), .groups = "drop")

brt_summary <- brt %>%
  group_by(longitude, latitude) %>%
  summarise(mean_pred = mean(macpyr_log, na.rm = TRUE), .groups = "drop")

# ============================================================
summary_df <- gam_summary %>%
  rename(gam_mean = mean_pred) %>%
  left_join(brt_summary %>% rename(brt_mean = mean_pred), by = c("longitude", "latitude")) %>%
  pivot_longer(cols = c(gam_mean, brt_mean), names_to = "model", values_to = "mean_prediction")

summary_df_wide <- summary_df %>%
  pivot_wider(names_from = model, values_from = mean_prediction)

# ============================================================
within_site_summary <- bind_rows(
  gam %>% mutate(model = "GAM"),
  brt %>% mutate(model = "BRT")
) %>%
  group_by(longitude, latitude, model) %>%
  summarise(
    n      = n(),
    mean   = mean(macpyr_log, na.rm = TRUE),
    median = median(macpyr_log, na.rm = TRUE),
    sd     = sd(macpyr_log, na.rm = TRUE),
    cv     = sd / mean,
    iqr    = IQR(macpyr_log, na.rm = TRUE),
    q25    = quantile(macpyr_log, 0.25, na.rm = TRUE),
    q75    = quantile(macpyr_log, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

print(within_site_summary)

across_site_summary <- within_site_summary %>%
  group_by(model) %>%
  summarise(
    n_locations      = n_distinct(paste(longitude, latitude)),
    grand_mean       = mean(mean, na.rm = TRUE),
    grand_sd         = sd(mean, na.rm = TRUE),
    grand_cv         = grand_sd / grand_mean,
    range_mean       = max(mean) - min(mean),
    site_min_mean    = min(mean),
    site_max_mean    = max(mean),
    median_within_sd = median(sd, na.rm = TRUE),
    .groups = "drop"
  )

print(across_site_summary)

signal_noise <- across_site_summary %>%
  select(model, grand_sd, median_within_sd) %>%
  mutate(between_to_within_ratio = grand_sd / median_within_sd)

print(signal_noise)

# ============================================================
# Within-location spread: faceted boxplot
bind_rows(
  gam %>% mutate(model = "GAM"),
  brt %>% mutate(model = "BRT")
) %>%
  mutate(loc = paste0(round(longitude, 3), ",", round(latitude, 3))) %>%
  ggplot(aes(x = reorder(loc, macpyr_log, median), y = macpyr_log, fill = model)) +
  geom_boxplot(outlier.size = 0.8, alpha = 0.8) +
  labs(title = "Within-Location Prediction Spread by Model",
       x = "Location (ordered by median)", y = "Predicted Kelp (log)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Across-location variation: point + error bar
within_site_summary %>%
  mutate(loc = paste0(round(longitude, 3), ",", round(latitude, 3))) %>%
  ggplot(aes(x = reorder(loc, mean), y = mean, color = model, group = model)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0.3, position = position_dodge(0.5)) +
  geom_point(size = 3, position = position_dodge(0.5)) +
  labs(title = "Mean ± SD of Predictions Across Locations",
       x = "Location (ordered by GAM mean)", y = "Mean Predicted Kelp (log)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# CV boxplot
within_site_summary %>%
  mutate(cv = abs(cv)) %>%
  ggplot(aes(x = model, y = cv, fill = model)) +
  geom_boxplot(alpha = 0.8, outlier.size = 1.5) +
  scale_fill_manual(values = c("GAM" = "steelblue", "BRT" = "firebrick")) +
  labs(x = "Model", y = "Coefficient of Variation") +
  theme_minimal() +
  theme(legend.position = "none")

# ============================================================
# T-test / Wilcoxon on CV
# ============================================================
cv_gam <- within_site_summary %>%
  filter(model == "GAM") %>%
  mutate(cv = abs(cv)) %>%
  pull(cv)

cv_brt <- within_site_summary %>%
  filter(model == "BRT") %>%
  mutate(cv = abs(cv)) %>%
  pull(cv)

shapiro.test(cv_gam)
shapiro.test(cv_brt)

wilcox.test(cv_gam, cv_brt)

library(rstatix)
cv_df <- within_site_summary %>% mutate(cv = abs(cv))
wilcox_effsize(cv ~ model, data = cv_df)

# CV heatmap — too many locations for a tile per point, so bin by rounding
within_site_summary %>%
  mutate(cv = abs(cv),
         lon_bin = round(longitude, 2),
         lat_bin = round(latitude, 2)) %>%
  group_by(lon_bin, lat_bin, model) %>%
  summarise(cv = mean(cv, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = lon_bin, y = lat_bin, fill = cv)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "steelblue", mid = "lightyellow", high = "firebrick",
                       midpoint = median(abs(within_site_summary$cv), na.rm = TRUE)) +
  facet_wrap(~model) +
  labs(title = "Coefficient of Variation by Location & Model",
       x = "Longitude", y = "Latitude", fill = "CV") +
  theme_minimal()

# GAM vs BRT scatter
axis_range <- range(c(summary_df_wide$gam_mean, summary_df_wide$brt_mean), na.rm = TRUE)

summary_df_wide %>%
  ggplot(aes(x = gam_mean, y = brt_mean, fill = )) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 2, alpha = 0.6, color = "steelblue") +
  scale_x_continuous(limits = axis_range) +
  scale_y_continuous(limits = axis_range) +
  coord_fixed() +
  labs(x = "GAM Mean Prediction (log)", y = "BRT Mean Prediction (log)") +
  theme_bw()

summary_df_wide %>%
  left_join(
    bind_rows(
      gam %>% select(longitude, latitude, depth) %>% distinct(),
      brt %>% select(longitude, latitude, depth) %>% distinct()
    ) %>% distinct(),
    by = c("longitude", "latitude")
  ) %>%
  ggplot(aes(x = gam_mean, y = brt_mean, color = depth)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_viridis_c(name = "Depth (m)") +
  scale_x_continuous(limits = axis_range) +
  scale_y_continuous(limits = axis_range) +
  coord_fixed() +
  labs(x = "GAM Mean Prediction (log)", y = "BRT Mean Prediction (log)") +
  theme_bw()


# ============
# t-test

# ============================================================
# Paired t-test: GAM vs BRT mean predictions by location
# ============================================================

# Check normality of differences
differences <- summary_df_wide$gam_mean - summary_df_wide$brt_mean
shapiro.test(differences)

# Paired t-test
t_result <- t.test(summary_df_wide$gam_mean, summary_df_wide$brt_mean, paired = TRUE)
print(t_result)

library(effectsize)

cohens_d(summary_df_wide$gam_mean, summary_df_wide$brt_mean, paired = TRUE)

# If Shapiro p < 0.05, use Wilcoxon signed-rank instead
wilcox.test(summary_df_wide$gam_mean, summary_df_wide$brt_mean, paired = TRUE)
