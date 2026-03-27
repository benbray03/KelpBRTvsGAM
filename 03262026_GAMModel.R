library(readr)
library(mgcv)
library(tidyverse)
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

# Train/test split — temporal
giant_ci_train <- giant_ci |> filter(year <= 2015)
giant_ci_test  <- giant_ci |> filter(year > 2015)

formula_ci <- as.formula(macpyr_log ~ 
                           s(taucross_surface_upwelling, k = 4, bs = "cr") +
                           s(no3_deepest_summer, k = 4, bs = "cr") +
                           s(taualong_surface_upwelling, k = 4, bs = "cr") +
                           s(depth, k = 4, bs = "cr") +
                           s(temp_deepest_upwelling, k = 4, bs = "cr") +
                           s(urchin_log, k = 4, bs = "cr")
)

model_gam_ci <- gam(formula_ci,
                    data = giant_ci_train,
                    method = "REML",
                    family = "tw")

saveRDS(model_gam_ci, "rds/giant_se_gam.rds")


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

# ---- Getting Manual VIP ----
# Get deviance explained for full model
full_dev <- summary(model_gam_ci)$dev.expl

# Fit models with each variable dropped
vars <- c("depth", "taucross_surface_upwelling", "no3_deepest_summer", 
          "taualong_surface_upwelling", "temp_deepest_upwelling", "")

importance <- map_dfr(vars, function(v) {
  formula_drop <- as.formula(paste(
    "macpyr_log ~ ",
    paste(paste0("s(", vars[vars != v], ", k = 4, bs = 'cr')"), collapse = " + "),
    "+ s(year_factor, bs = 're') + s(latitude, longitude)"
  ))
  
  model_drop <- gam(formula_drop,
                    data = giant_ci_train,
                    method = "REML",
                    family = "tw")
  
  tibble(
    variable = v,
    dev_expl = summary(model_drop)$dev.expl,
    importance = full_dev - summary(model_drop)$dev.expl
  )
})

# Plot
ggplot(importance, aes(x = reorder(variable, importance), y = importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(x = "Variable", 
       y = "Drop in deviance explained",
       title = "GAM Variable Importance - Channel Islands") +
  theme_classic()


