library(readr)
library(mgcv)
library(tidyverse)
library(ggplot2)

giant_ci <- read_csv("csv/gams_training/macpyr_gams_training_observed_waves.csv") |>
  mutate(
    site = as.factor(site),
    macpyr_log = log10(den_macpyr + 1),
    urchin_log = log10(den_urchin + 1),
    year_factor = as.factor(year - 2000)
  ) |>
  rename("depth" = "final_depth") |>
  filter(
    latitude >= 33.8 & latitude <= 34.1,
    longitude >= -120.5 & longitude <= -119.5
  )

# Train/test split — temporal
giant_ci_train <- giant_ci |> filter(year <= 2015)
giant_ci_test  <- giant_ci |> filter(year > 2015)

formula_ci <- as.formula(macpyr_log ~ 
                           s(depth, k = 4, bs = "cr") + 
                           s(Mean_Upwelling_Nitrate, k = 4, bs = "cr") + 
                           s(Mean_Summer_Temp, k = 4, bs = "cr") + 
                           s(Wave_95prc, k = 4, bs = "cr") + 
                           s(urchin_log, k = 4, bs = "cr") + 
                           s(year_factor, bs = "re") +
                           s(latitude, longitude)
)

model_gam_ci <- gam(formula_ci,
                    data = giant_ci_train,
                    method = "REML",
                    family = "tw")

summary(model_gam_ci)


# Generate predictions on test set
giant_ci_test$pred_gam <- predict(model_gam_ci,
                                  newdata = giant_ci_test,
                                  type = "response",
                                  exclude = "s(year_factor)")
# Basic performance metrics
rmse <- sqrt(mean((giant_ci_test$macpyr_log - giant_ci_test$pred_gam)^2))
r_squared <- cor(giant_ci_test$macpyr_log, giant_ci_test$pred_gam)^2

cat("RMSE:", rmse, "\n")
cat("R-squared:", r_squared, "\n")

# Plot observed vs predicted
ggplot(giant_ci_test, aes(x = macpyr_log, y = pred_gam)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Observed (log kelp density)", 
       y = "Predicted (log kelp density)",
       title = "GAM: Observed vs Predicted - Channel Islands") +
  theme_classic()
