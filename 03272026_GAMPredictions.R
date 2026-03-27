# New kelp projection script

library(tidymodels)
library(tidyverse)
library(mgcv)

# load urchin rds
urchin_historical <- readRDS("csv/brt_predictions/historical_for_kelp_projections_300res_poisson.rds")
urchin_future <- readRDS("csv/brt_predictions/future_for_kelp_projections_300res_poisson.rds")

# MACPYR SE ====
species <- "giant"
region <- "se"
response <- "macpyr_log"
figure_folder <- paste0("figures/gam/kelp_projections/", species, "_", region)

gam_model <- readRDS("rds/giant_se_gam.rds")

historical <- urchin_historical |> 
  mutate(site = as.character(site)) |> 
  filter(latitude >= 33.9 & latitude <= 34.08,
         longitude >= -120 & longitude <= -119.3) 
future <- urchin_future |> 
  mutate(site = as.character(site)) |> 
  filter(latitude >= 33.9 & latitude <= 34.08,
         longitude >= -120 & longitude <= -119.3)

project_historical <- predict(gam_model,
                              newdata = historical |> mutate(year_factor = NA),
                              type = "response",
                              exclude = "s(year_factor)")

project_historical_df <- historical |> 
  mutate(macpyr_log = as.numeric(project_historical)) |> 
  relocate(urchin_log, .after = site) |> 
  relocate(macpyr_log, .before = year)

# GAM predictions - future
project_future <- predict(gam_model,
                          newdata = future |> mutate(year_factor = NA),
                          type = "response",
                          exclude = "s(year_factor)")

project_future_df <- future |> 
  mutate(macpyr_log = as.numeric(project_future)) |> 
  relocate(urchin_log, .after = site) |> 
  relocate(macpyr_log, .before = year)
head(project_historical_df)      

# Save outputs
write_csv(project_future_df, paste0("csv/gam_predictions/", species, "_", region, "_future_predictions_300res_ci.csv"))
write_csv(project_historical_df, paste0("csv/gam_predictions/", species, "_", region, "_historical_predictions_300res_ci.csv"))

kelp_future_summary <- project_future_df |> 
  group_by(year, model) |> 
  reframe(macpyr_log_mean = mean(get(response)),
          macpyr_log_min = min(get(response)),
          macpyr_log_max = max(get(response)),
          macpyr_log_se = sd(get(response))/sqrt(n()),
          macpyr_log_sd = sd(get(response))) |> 
  na.omit() 

kelp_historical_summary <- project_historical_df |> 
  mutate(model = "historical") |> 
  group_by(year, model) |> 
  reframe(macpyr_log_mean = mean(get(response)),
          macpyr_log_min = min(get(response)),
          macpyr_log_max = max(get(response)),
          macpyr_log_se = sd(get(response))/sqrt(n()),
          macpyr_log_sd = sd(get(response))) |> 
  na.omit()

kelp_summary <- full_join(kelp_historical_summary, kelp_future_summary) |> 
  mutate(model = factor(model, levels = c("historical", "gfdl", "hadl", "ipsl")))

write_csv(kelp_summary, paste0("csv/gam_predictions/", species, "_", region, "_future_predictions_summary_300res.csv"))

ggplot()+
  geom_ribbon(data = kelp_summary, aes(x = year, ymin = macpyr_log_min, ymax = macpyr_log_max, fill = model), alpha = 0.3)+
  geom_line(data = kelp_summary, aes(x = year, y = macpyr_log_mean, color = model)) +
  scale_color_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  labs(x = "Year", y = "Mean predicted kelp abundance (log)", color = "ESM", fill = "ESM", title = paste(species, region))+
  theme_classic()
ggsave(paste0(figure_folder, "_ts_maxmin.png"), width = 6, height = 4, dpi = 300)

ggplot()+
  geom_line(data = kelp_summary, aes(x = year, y = macpyr_log_mean, color = model)) +
  scale_color_brewer(palette = "Dark2")+
  labs(x = "Year", y = "Mean predicted kelp abundance (log)", color = "ESM", title = paste(species, region))+
  theme_classic()
ggsave(paste0(figure_folder, "_ts.png"), width = 6, height = 4, dpi = 300)

# 
# # MACSTIPES SE ====
# species <- "giantstipes"
# region <- "se"
# response <- "macstipes_log"
# figure_folder <- paste0("figures/brt/kelp_projections/", species, "_", region)
# 
# bt_deploy_model <- readRDS(paste0("rds/", species, "_", region, "_brt_deploy_poisson.rds"))
# 
# historical <- urchin_historical |> 
#   mutate(site = as.character(site)) |> 
#   filter(longitude >= -119.95)
# future <- urchin_future |> 
#   mutate(site = as.character(site)) |> 
#   filter(longitude >= -119.95)
# 
# project_future <- predict(bt_deploy_model, future) 
# project_future_df <- future |> 
#   mutate(macstipes_log = project_future$.pred) |> 
#   relocate(urchin_log, .after = site) |> 
#   relocate(macstipes_log, .before = year)
# head(project_future_df)
# 
# project_historical <- predict(bt_deploy_model, historical) 
# project_historical_df <- historical |> 
#   mutate(macstipes_log = project_historical$.pred) |> 
#   relocate(urchin_log, .after = site) |> 
#   relocate(macstipes_log, .before = year)
# head(project_historical_df)      
# 
# # Save outputs
# write_csv(project_future_df, paste0("csv/brt_predictions/", species, "_", region, "_future_predictions_300res.csv"))
# write_csv(project_historical_df, paste0("csv/brt_predictions/", species, "_", region, "_historical_predictions_300res.csv"))
# 
# kelp_future_summary <- project_future_df |> 
#   group_by(year, model) |> 
#   reframe(macstipes_log_mean = mean(get(response)),
#           macstipes_log_min = min(get(response)),
#           macstipes_log_max = max(get(response)),
#           macstipes_log_se = sd(get(response))/sqrt(n()),
#           macstipes_log_sd = sd(get(response))) |> 
#   na.omit() 
# 
# kelp_historical_summary <- project_historical_df |> 
#   mutate(model = "historical") |> 
#   group_by(year, model) |> 
#   reframe(macstipes_log_mean = mean(get(response)),
#           macstipes_log_min = min(get(response)),
#           macstipes_log_max = max(get(response)),
#           macstipes_log_se = sd(get(response))/sqrt(n()),
#           macstipes_log_sd = sd(get(response))) |> 
#   na.omit()
# 
# kelp_summary <- full_join(kelp_historical_summary, kelp_future_summary) |> 
#   mutate(model = factor(model, levels = c("historical", "gfdl", "hadl", "ipsl")))
# 
# write_csv(kelp_summary, paste0("csv/brt_predictions/", species, "_", region, "_future_predictions_summary_300res.csv"))
# 
# ggplot()+
#   geom_ribbon(data = kelp_summary, aes(x = year, ymin = macstipes_log_min, ymax = macstipes_log_max, fill = model), alpha = 0.3)+
#   geom_line(data = kelp_summary, aes(x = year, y = macstipes_log_mean, color = model)) +
#   scale_color_brewer(palette = "Dark2")+
#   scale_fill_brewer(palette = "Dark2")+
#   labs(x = "Year", y = "Mean predicted kelp abundance (log)", color = "ESM", fill = "ESM", title = paste(species, region))+
#   theme_classic()
# ggsave(paste0(figure_folder, "_ts_maxmin.png"), width = 6, height = 4, dpi = 300)
# 
# ggplot()+
#   geom_line(data = kelp_summary, aes(x = year, y = macstipes_log_mean, color = model)) +
#   scale_color_brewer(palette = "Dark2")+
#   labs(x = "Year", y = "Mean predicted kelp abundance (log)", color = "ESM", title = paste(species, region))+
#   theme_classic()
# ggsave(paste0(figure_folder, "_ts.png"), width = 6, height = 4, dpi = 300)
# 
