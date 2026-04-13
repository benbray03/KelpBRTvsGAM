library(tidyverse)
library(pdp)
library(xgboost)
library(patchwork)
library(tidymodels)

# ============================================================
# 1. LOAD MODEL AND DATA
# ============================================================

brt_model <- readRDS("rds/giant_se_brt_deploy_poisson.rds")
brt_fit   <- extract_fit_parsnip(brt_model)$fit  # xgb.Booster object

brt_data  <- read_csv("csv/brt_predictions/giant_se_future_predictions_300res_ci.csv")

# ============================================================
# 2. GET FEATURES
# ============================================================

brt_imp      <- xgb.importance(model = brt_fit)
brt_features <- brt_imp$Feature

# Build numeric matrix for partial()
brt_matrix <- as.matrix(brt_data[, brt_features])

# ============================================================
# 3. GENERATE PDPs VIA partial()
# ============================================================

pdp_brt_list <- list()

for (feat in brt_features) {
  cat("Computing PDP for:", feat, "\n")
  
  pd <- partial(
    object    = brt_fit,
    pred.var  = feat,
    train     = brt_matrix,
    grid.resolution = 50,
    type      = "regression"   # xgb with poisson still predicts on log scale here
  )
  
  pdp_brt_list[[feat]] <- data.frame(
    value   = pd[[feat]],
    yhat    = pd$yhat,
    feature = feat
  )
}

pdp_brt_all <- bind_rows(pdp_brt_list)

# ============================================================
# 4. CENTER AND LABEL
# ============================================================

pdp_brt_all <- pdp_brt_all |>
  group_by(feature) |>
  mutate(yhat_centered  = yhat - mean(yhat),
         feature_label  = gsub("_", " ", feature)) |>
  ungroup()

# Reorder panels by BRT feature importance
pdp_brt_all <- pdp_brt_all |>
  mutate(feature_label = factor(feature_label,
                                levels = gsub("_", " ", brt_features)))

# ============================================================
# 5. RUG DATA
# ============================================================

rug_data <- brt_data |>
  select(all_of(brt_features)) |>
  pivot_longer(cols = everything(),
               names_to  = "feature",
               values_to = "value") |>
  mutate(feature_label = gsub("_", " ", feature),
         feature_label = factor(feature_label,
                                levels = gsub("_", " ", brt_features)))

# ============================================================
# 6. PLOT
# ============================================================

p_pdp <- ggplot() +
  geom_line(data = pdp_brt_all,
            aes(x = value, y = yhat_centered),
            color = "#1D6FA4", linewidth = 1) +
  geom_rug(data = rug_data,
           aes(x = value),
           sides = "b", alpha = 0.15, inherit.aes = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "gray50", linewidth = 0.4) +
  facet_wrap(~ feature_label, scales = "free_x", ncol = 3) +
  labs(x     = "Predictor value",
       y     = "Partial dependence (centered)",
       title = "Partial Dependence — Giant Kelp SE (BRT)") +
  theme_classic(base_size = 12) +
  theme(strip.background = element_blank(),
        strip.text       = element_text(face = "bold"),
        panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3))

p_pdp

ggsave("figures/giant_se_pdp_brt.png",
       plot = p_pdp, width = 11, height = 8, dpi = 300)

# # -----------
# # Manual BRT
# 
# # Manual PDP function for xgb.Booster
# manual_pdp <- function(model, train_matrix, feat, grid.resolution = 50) {
#   grid_vals <- seq(min(train_matrix[, feat]),
#                    max(train_matrix[, feat]),
#                    length.out = grid.resolution)
#   
#   yhat_avg <- sapply(grid_vals, function(val) {
#     mat_temp <- train_matrix
#     mat_temp[, feat] <- val
#     preds <- predict(model, mat_temp)
#     mean(preds)
#   })
#   
#   data.frame(value = grid_vals, yhat = yhat_avg, feature = feat)
# }
# 
# # Test on one feature first
# set.seed(42)
# brt_matrix_sample <- brt_matrix[sample(nrow(brt_matrix), 3000), ]
# 
# pd_test <- manual_pdp(brt_fit, brt_matrix_sample, "urchin_log")
# range(pd_test$yhat)
# plot(pd_test$value, pd_test$yhat, type = "l")
# 
# 
# # Does prediction change at all when we feed garbage?
# test1 <- brt_matrix_sample
# test2 <- brt_matrix_sample
# test2[, "urchin_log"] <- 999  # extreme nonsense value
# 
# mean(predict(brt_fit, test1))
# mean(predict(brt_fit, test2))  # if identical to test1, the booster is broken
