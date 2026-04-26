library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggrepel)

# ============================================================
gam <- read_csv("csv/gam_predictions/giant_se_future_predictions_300res_ci.csv") %>%
  mutate(site = as.character(site))

brt <- read_csv("csv/brt_predictions/giant_se_future_predictions_300res_ci.csv") %>%
  mutate(site = as.character(site))

# ============================================================

gam_summary <- gam %>%
  group_by(site) %>%
  summarise(mean_pred = mean(macpyr_log))

brt_summary <- brt %>%
  group_by(site) %>%
  summarise(mean_pred = mean(macpyr_log))

# ============================================================
summary_df <- gam_summary %>%
  rename(gam_mean = mean_pred) %>%
  left_join(brt_summary %>% rename(brt_mean = mean_pred), by = "site") %>%
  pivot_longer(cols = c(gam_mean, brt_mean), names_to = "model", values_to = "mean_prediction")

ggplot(summary_df, aes(x = site, y = mean_prediction, fill = model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Predictions by Site and Model",
       x = "Site",
       y = "Mean Prediction (log scale)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# proportion of sites where BRT is greater than GAM
summary_df_wide <- summary_df %>%
  pivot_wider(names_from = model, values_from = mean_prediction)

# =====================
within_site_summary <- bind_rows(
  gam %>% mutate(model = "GAM"),
  brt %>% mutate(model = "BRT")
) %>%
  group_by(site, model) %>%
  summarise(
    n          = n(),
    mean       = mean(macpyr_log, na.rm = TRUE),
    median     = median(macpyr_log, na.rm = TRUE),
    sd         = sd(macpyr_log, na.rm = TRUE),
    cv         = sd / mean,           # coefficient of variation
    iqr        = IQR(macpyr_log, na.rm = TRUE),
    q25        = quantile(macpyr_log, 0.25, na.rm = TRUE),
    q75        = quantile(macpyr_log, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

print(within_site_summary)

across_site_summary <- within_site_summary %>%
  group_by(model) %>%
  summarise(
    n_sites         = n_distinct(site),
    grand_mean      = mean(mean, na.rm = TRUE),
    grand_sd        = sd(mean, na.rm = TRUE),    # variability *between* site means
    grand_cv        = grand_sd / grand_mean,
    range_mean      = max(mean) - min(mean),
    site_min_mean   = min(mean),
    site_max_mean   = max(mean),
    median_within_sd = median(sd, na.rm = TRUE), # typical within-site spread
    .groups = "drop"
  )

print(across_site_summary)

signal_noise <- across_site_summary %>%
  select(model, grand_sd, median_within_sd) %>%
  mutate(between_to_within_ratio = grand_sd / median_within_sd)

print(signal_noise)

# Within-site spread: faceted boxplot
bind_rows(
  gam %>% mutate(model = "GAM"),
  brt %>% mutate(model = "BRT")
) %>%
  ggplot(aes(x = reorder(site, macpyr_log, median), y = macpyr_log, fill = model)) +
  geom_boxplot(outlier.size = 0.8, alpha = 0.8) +
  labs(title = "Within-Site Prediction Spread by Model",
       x = "Site (ordered by median)", y = "Predicted Kelp (log)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Across-site variation: point + error bar
within_site_summary %>%
  ggplot(aes(x = reorder(site, mean), y = mean, color = model, group = model)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0.3, position = position_dodge(0.5)) +
  geom_point(size = 3, position = position_dodge(0.5)) +
  labs(title = "Mean ± SD of Predictions Across Sites",
       x = "Site (ordered by GAM mean)", y = "Mean Predicted Kelp (log)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

within_site_summary %>%
  mutate(cv = abs(cv)) %>%
  ggplot(aes(x = model, y = cv, fill = model)) +
  geom_boxplot(alpha = 0.8, outlier.size = 1.5) +
  scale_fill_manual(values = c("GAM" = "steelblue", "BRT" = "firebrick")) +
  labs(
    x = "Model",
    y = "Coefficient of Variation"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# =====================
# T- test
# =====================

# Extract CV vectors for each model
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
# BRT fails shapiro wilkes test, use Wilcox

wilcox.test(cv_gam, cv_brt)

library(rstatix)

cv_df <- within_site_summary %>%
  mutate(cv = abs(cv))

wilcox_effsize(cv ~ model, data = cv_df)


# Signal-to-noise: CV per site as a heatmap
within_site_summary %>%
  mutate(cv = abs(cv)) %>%
  ggplot(aes(x = model, y = site, fill = cv)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "steelblue", mid = "lightyellow",
                       high = "firebrick", midpoint = median(abs(within_site_summary$cv))) +
  labs(title = "Coefficient of Variation by Site & Model",
       x = "Model", y = "Site", fill = "CV") +
  theme_minimal()

axis_range <- range(c(summary_df_wide$gam_mean, summary_df_wide$brt_mean), na.rm = TRUE)

summary_df_wide %>%
  ggplot(aes(x = gam_mean, y = brt_mean, label = "")) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 3, color = "steelblue") +
  ggrepel::geom_text_repel(size = 3) +
  scale_x_continuous(limits = axis_range) +
  scale_y_continuous(limits = axis_range) +
  coord_fixed() +
  labs(
       x = "GAM Mean Prediction (log)", y = "BRT Mean Prediction (log)") +
  theme_bw()
