library(tidyverse)
library(mgcv)
library(tidymodels)
library(xgboost)
library(patchwork)
set.seed(42)

# ============================================================
# 1. LOAD MODELS AND DATA
# ============================================================

brt_model <- readRDS("rds/giant_se_brt_deploy_poisson.rds")
brt_fit   <- extract_fit_parsnip(brt_model)$fit

brt_data  <- read_csv("csv/brt_predictions/giant_se_future_predictions_300res_ci.csv") |>
  mutate(site = as.character(site))


brt_df_sample <- brt_data[sample(nrow(brt_data), 10000), ]

gam_model <- readRDS("rds/giant_se_gam.rds")
gam_data  <- read_csv("csv/gam_predictions/giant_se_future_predictions_300res_ci.csv")

# ============================================================
# 2. DEFINE SHARED FEATURES
# ============================================================

brt_imp      <- xgb.importance(model = brt_fit)
brt_features <- brt_imp$Feature

gam_terms    <- as.character(attr(terms(gam_model$formula), "term.labels"))
gam_features <- gam_terms |>
  str_extract("(?<=s\\(|te\\(|ti\\()\\w+") |>
  na.omit() |>
  as.character()

shared_features <- intersect(brt_features, gam_features)
cat("Shared features:", paste(shared_features, collapse = ", "), "\n")

if (length(shared_features) == 0) stop("No shared features found.")

# ============================================================
# 3. MANUAL PDP FUNCTION (full data, workflow-based)
# ============================================================

manual_pdp_workflow <- function(workflow, train_df, feat, grid.resolution = 50) {
  grid_vals <- seq(min(train_df[[feat]], na.rm = TRUE),
                   max(train_df[[feat]], na.rm = TRUE),
                   length.out = grid.resolution)
  
  yhat_avg <- sapply(grid_vals, function(val) {
    tmp <- train_df
    tmp[[feat]] <- val
    mean(predict(workflow, new_data = tmp)$.pred)
  })
  
  data.frame(value = grid_vals, yhat = yhat_avg, feature = feat)
}

# ============================================================
# 4. BRT PDPs (full data)
# ============================================================

pdp_brt_list <- list()

for (feat in shared_features) {
  cat("BRT PDP for:", feat, "\n")
  df <- manual_pdp_workflow(brt_model, brt_df_sample, feat)
  df$model <- "BRT"
  pdp_brt_list[[feat]] <- df
}

pdp_brt_all <- bind_rows(pdp_brt_list)

# ============================================================
# 5. GAM PDPs
# ============================================================

pdp_gam_list <- list()

for (feat in shared_features) {
  cat("GAM PDP for:", feat, "\n")
  
  feat_range <- range(gam_data[[feat]], na.rm = TRUE)
  grid_vals  <- seq(feat_range[1], feat_range[2], length.out = 50)
  
  pred_grid <- gam_data |>
    summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE))) |>
    slice(rep(1, 50))
  
  factor_cols <- names(gam_data)[sapply(gam_data, is.factor)]
  for (fc in factor_cols) {
    pred_grid[[fc]] <- factor(
      names(sort(table(gam_data[[fc]]), decreasing = TRUE))[1],
      levels = levels(gam_data[[fc]])
    )
  }
  
  pred_grid[[feat]] <- grid_vals
  
  pred_vals <- predict(gam_model,
                       newdata            = pred_grid,
                       type               = "link",
                       newdata.guaranteed = TRUE)
  
  pdp_gam_list[[feat]] <- data.frame(
    value   = grid_vals,
    yhat    = as.numeric(pred_vals),
    feature = feat,
    model   = "GAM"
  )
}

pdp_gam_all <- bind_rows(pdp_gam_list)

# ============================================================
# 6. COMBINE AND CENTER
# ============================================================

pdp_combined <- bind_rows(pdp_brt_all, pdp_gam_all) |>
  group_by(feature, model) |>
  mutate(yhat_centered = yhat - mean(yhat)) |>
  ungroup() |>
  mutate(feature_label = factor(gsub("_", " ", feature),
                                levels = gsub("_", " ", shared_features)))

# ============================================================
# 7. RUG DATA
# ============================================================

rug_data <- brt_data |>
  select(all_of(shared_features)) |>
  pivot_longer(cols = everything(),
               names_to  = "feature",
               values_to = "value") |>
  mutate(feature_label = factor(gsub("_", " ", feature),
                                levels = gsub("_", " ", shared_features)))

# ============================================================
# 8. PLOT — OVERLAY
# ============================================================

p_overlay <- ggplot() +
  geom_line(data = pdp_combined,
            aes(x = value, y = yhat_centered, color = model, linetype = model),
            linewidth = 1) +
  geom_rug(data = rug_data,
           aes(x = value),
           sides = "b", alpha = 0.15, inherit.aes = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "gray50", linewidth = 0.4) +
  facet_wrap(~ feature_label, scales = "free_x", ncol = 3) +
  scale_color_brewer(palette = "Dark2") +
  labs(x        = "Predictor value",
       y        = "Partial response (centered)",
       color    = "Model",
       linetype = "Model",
       title    = "Partial Dependence: BRT vs GAM — Giant Kelp SE") +
  theme_classic(base_size = 12) +
  theme(legend.position  = "top",
        strip.background = element_blank(),
        strip.text       = element_text(face = "bold"),
        panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3))

p_overlay

ggsave("figures/giant_se_pdp_brt_vs_gam_overlay.png",
       plot = p_overlay, width = 10, height = 8, dpi = 300)

# ============================================================
# 9. PLOT — FACETED
# ============================================================

p_facet <- ggplot() +
  geom_line(data = pdp_combined,
            aes(x = value, y = yhat_centered, color = model),
            linewidth = 1) +
  geom_rug(data = rug_data,
           aes(x = value),
           sides = "b", alpha = 0.15, inherit.aes = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "gray50", linewidth = 0.4) +
  facet_grid(model ~ feature_label, scales = "free",
             labeller = label_wrap_gen(width = 15)) +  # wrap long titles
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(guide = guide_axis(angle = 45)) +  # angle x labels
  labs(x     = "Predictor value",
       y     = "Partial response (centered)",
       title = "Partial Dependence: BRT vs GAM — Giant Kelp SE") +
  theme_classic(base_size = 12) +
  theme(legend.position    = "none",
        strip.background   = element_blank(),
        strip.text         = element_text(face = "bold", size = 9),  # smaller strip text
        strip.text.x       = element_text(margin = margin(b = 5)),   # padding below title
        panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
        axis.text.x        = element_text(size = 7),   # smaller x axis text
        panel.spacing.x    = unit(0.8, "cm"))           # more space between columns
p_facet

ggsave("figures/giant_se_pdp_brt_vs_gam_faceted.png",
       plot = p_facet, width = 14, height = 6, dpi = 300)
