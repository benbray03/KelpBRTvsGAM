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

brt_data  <- read_csv("csv/brt_predictions/giant_se_future_predictions_300res_ci.csv") %>%
  mutate(site = as.character(site))

# ============================================================
# 2. GET FEATURES
# ============================================================

brt_imp      <- xgb.importance(model = brt_fit)
brt_features <- brt_imp$Feature

# Build numeric matrix for partial()
brt_matrix <- as.matrix(brt_data[, brt_features])

set.seed(42)
brt_df_sample <- brt_data[sample(nrow(brt_data), 10000), ]

# ============================================================
# 3. Manual PDP function for xgb.Booster
# ============================================================
manual_pdp_workflow <- function(workflow, train_df, feat, grid.resolution = 50) {
  grid_vals <- seq(min(train_df[[feat]]),
                   max(train_df[[feat]]),
                   length.out = grid.resolution)
  
  yhat_avg <- sapply(grid_vals, function(val) {
    tmp <- train_df
    tmp[[feat]] <- val
    mean(predict(workflow, new_data = tmp)$.pred)
  })
  
  data.frame(value = grid_vals, yhat = yhat_avg, feature = feat)
}


# ============================================================
# 4. GENERATE PDPs VIA partial()
# ============================================================
pdp_brt_list <- list()

for (feat in brt_features) {
  cat("Computing PDP for:", feat, "\n")
  pdp_brt_list[[feat]] <- manual_pdp_workflow(brt_model, brt_df_sample, feat)
}

pdp_brt_all <- bind_rows(pdp_brt_list) |>
  group_by(feature) |>
  mutate(yhat_centered  = yhat - mean(yhat),
         feature_label  = gsub("_", " ", feature)) |>
  ungroup() |>
  mutate(feature_label = factor(feature_label,
                                levels = gsub("_", " ", brt_features)))

rug_data <- brt_data |>
  select(all_of(brt_features)) |>
  pivot_longer(cols = everything(),
               names_to  = "feature",
               values_to = "value") |>
  mutate(feature_label = gsub("_", " ", feature),
         feature_label = factor(feature_label,
                                levels = gsub("_", " ", brt_features)))

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
  theme(strip.background    = element_blank(),
        strip.text          = element_text(face = "bold"),
        panel.grid.major.y  = element_line(color = "gray90", linewidth = 0.3))

p_pdp

ggsave("figures/giant_se_pdp_brt.png",
       plot = p_pdp, width = 11, height = 8, dpi = 300)
