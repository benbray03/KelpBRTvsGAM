# Get deviance explained for full model
full_dev <- summary(model_gam_ci)$dev.expl

# Fit models with each variable dropped
vars <- c("depth", "Mean_Upwelling_Nitrate", "Mean_Summer_Temp", 
          "Wave_95prc", "urchin_log")

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
