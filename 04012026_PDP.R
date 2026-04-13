# ============================================================
# Partial Dependence Plot Comparison: BRT vs GAM
# ============================================================

library(tidyverse)
library(mgcv)
library(pdp)
library(tidymodels)
library(xgboost)
library(vip)
library(patchwork)

# ============================================================
# 1. LOAD MODELS AND DATA
# ============================================================

# --- BRT ---
brt_model <- readRDS("rds/giant_se_brt_deploy_poisson.rds")
brt_fit   <- extract_fit_parsnip(brt_model)$fit  # xgb.Booster object

# Training data for BRT (used for rug plots and partial() train matrix)
brt_data  <- read_csv("csv/brt_predictions/giant_se_future_predictions_300res_ci.csv")  # <-- update path if needed

# --- GAM ---
gam_model <- readRDS("rds/giant_se_gam.rds")

# Training data for GAM (used for prediction grid in GAM PDPs)
# No year_factor or site columns expected — loaded as-is
gam_data  <- read_csv("csv/gam_predictions/giant_se_future_predictions_300res_ci.csv")  # <-- update path if needed

# ============================================================
# 2. DEFINE SHARED VARIABLES TO COMPARE
# ============================================================

# --- BRT: extract feature names from xgb.Booster via importance table ---
brt_imp      <- xgb.importance(model = brt_fit)
brt_features <- brt_imp$Feature
cat("BRT features:", paste(brt_features, collapse = ", "), "\n")

# --- GAM: extract variable names from smooth terms ---
gam_terms    <- as.character(attr(terms(gam_model$formula), "term.labels"))
gam_features <- gam_terms |>
  str_extract("(?<=s\\(|te\\(|ti\\()\\w+") |>   # handles s(), te(), ti()
  na.omit() |>
  as.character()
cat("GAM features:", paste(gam_features, collapse = ", "), "\n")

# --- Shared predictors ---
shared_features <- intersect(brt_features, gam_features)
cat("Shared features:", paste(shared_features, collapse = ", "), "\n")

# Safety check
if (length(shared_features) == 0) {
  stop("No shared features found between BRT and GAM. Check feature names above.")
}

# ============================================================
# 3. GENERATE PDP DATA FOR BRT (xgboost-compatible)
# ============================================================

# xgb.Booster requires a numeric matrix for partial()
brt_matrix <- as.matrix(brt_data[, brt_features])

pdp_brt_list <- list()

for (k in seq_along(shared_features)) {
  feat <- shared_features[k]
  
  feat_range <- range(brt_data[[feat]], na.rm = TRUE)
  grid_vals  <- seq(feat_range[1], feat_range[2], length.out = 50)
  
  pred_grid <- brt_data |>
    summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE))) |>
    slice(rep(1, 50)) |>
    mutate(site = as.character(round(site)))  # match training type
  
  pred_grid[[feat]] <- grid_vals
  
  pred_vals <- predict(brt_model, new_data = pred_grid)$.pred
  
  df <- data.frame(
    value   = grid_vals,
    yhat    = pred_vals,
    feature = feat,
    model   = "BRT"
  )
  
  pdp_brt_list[[k]] <- df
}

pdp_brt_all <- bind_rows(pdp_brt_list)
# ============================================================
# 4. GENERATE PDP DATA FOR GAM
# ============================================================

pdp_gam_list <- list()

for (k in seq_along(shared_features)) {
  feat <- shared_features[k]
  
  # Grid of values spanning the observed range of this predictor
  feat_range <- range(gam_data[[feat]], na.rm = TRUE)
  grid_vals  <- seq(feat_range[1], feat_range[2], length.out = 50)
  
  # Prediction grid: all numeric predictors held at their mean
  pred_grid <- gam_data |>
    summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE))) |>
    slice(rep(1, 50))
  
  # If any factor columns exist (e.g. site), use the most common level
  factor_cols <- names(gam_data)[sapply(gam_data, is.factor)]
  for (fc in factor_cols) {
    pred_grid[[fc]] <- factor(
      names(sort(table(gam_data[[fc]]), decreasing = TRUE))[1],
      levels = levels(gam_data[[fc]])
    )
  }
  
  # Set the focal variable to the grid values
  pred_grid[[feat]] <- grid_vals
  
  # Predict on the link scale; no random effects to exclude in this GAM
  pred_vals <- predict(gam_model,
                       newdata            = pred_grid,
                       type               = "link",
                       newdata.guaranteed = TRUE)
  
  df <- data.frame(
    value   = grid_vals,
    yhat    = as.numeric(pred_vals),
    feature = feat,
    model   = "GAM"
  )
  
  pdp_gam_list[[k]] <- df
}

pdp_gam_all <- bind_rows(pdp_gam_list)

# ============================================================
# 5. COMBINE AND CENTER FOR COMPARISON
# ============================================================

pdp_combined <- bind_rows(pdp_brt_all, pdp_gam_all)

# Center each model's response per feature so shapes are directly comparable
# (removes intercept differences between model types)
pdp_combined <- pdp_combined |>
  group_by(feature, model) |>
  mutate(yhat_centered = yhat - mean(yhat)) |>
  ungroup()

# ============================================================
# 6. RUG DATA
# ============================================================

rug_data <- brt_data |>
  select(all_of(shared_features)) |>
  pivot_longer(cols = everything(), names_to = "feature", values_to = "value")

# ============================================================
# 7. PLOT
# ============================================================

# Clean up axis labels (replace underscores with spaces)
pdp_combined <- pdp_combined |>
  mutate(feature_label = gsub("_", " ", feature))

rug_data <- rug_data |>
  mutate(feature_label = gsub("_", " ", feature))

# --- Option A: Overlaid lines (BRT and GAM on same panel per variable) ---
p_overlay <- ggplot() +
  geom_line(data     = pdp_combined,
            aes(x = value, y = yhat_centered, color = model, linetype = model),
            linewidth = 1) +
  geom_rug(data      = rug_data,
           aes(x = value),
           sides      = "b", alpha = 0.15, inherit.aes = FALSE) +
  facet_wrap(~ feature_label, scales = "free_x", ncol = 3) +
  scale_color_brewer(palette = "Dark2") +
  labs(x        = "Predictor value",
       y        = "Partial response (centered)",
       color    = "Model",
       linetype = "Model",
       title    = "Partial Dependence: BRT vs GAM — Giant Kelp SE") +
  theme_classic() +
  theme(legend.position  = "top",
        strip.background = element_blank(),
        strip.text       = element_text(face = "bold"))

p_overlay

ggsave("figures/giant_se_pdp_brt_vs_gam_overlay.png",
       plot = p_overlay, width = 10, height = 8, dpi = 300)

# --- Option B: Side-by-side rows (model as facet row, variable as column) ---
p_facet <- ggplot() +
  geom_line(data     = pdp_combined,
            aes(x = value, y = yhat_centered, color = model),
            linewidth = 1) +
  geom_rug(data      = rug_data,
           aes(x = value),
           sides      = "b", alpha = 0.15, inherit.aes = FALSE) +
  facet_grid(model ~ feature_label, scales = "free_x") +
  scale_color_brewer(palette = "Dark2") +
  labs(x     = "Predictor value",
       y     = "Partial response (centered)",
       title = "Partial Dependence: BRT vs GAM — Giant Kelp SE") +
  theme_classic() +
  theme(legend.position  = "none",
        strip.background = element_blank(),
        strip.text       = element_text(face = "bold"))

p_facet

ggsave("figures/giant_se_pdp_brt_vs_gam_faceted.png",
       plot = p_facet, width = 14, height = 6, dpi = 300)

# -----
# testing
# -----

test_grid2 <- brt_data |>
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE))) |>
  slice(rep(1, 5)) |>
  mutate(site = as.character(round(site)))  # convert to character to match training

test_grid2$depth <- c(0, 5, 10, 20, 30)

predict(brt_model, new_data = test_grid2)
