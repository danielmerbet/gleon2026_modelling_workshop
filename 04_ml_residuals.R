# =============================================================================
# 04_ml_residuals.R
# PART 4 — ML to correct process model residuals (20 min)
#
# Goals:
#   • Build a feature matrix from physics + ERA5 + depth/season encoding
#   • Train random forests on temperature and DO residuals
#   • Cross-validate with leave-one-campaign-out (LOCO) — honest for
#     the temporal structure of the data
#   • Validate surface predictions against ACA 2025–2026 test campaigns
#   • Generate full depth profiles for each test campaign date
#     (the risk assessment map in Part 4)
# =============================================================================

source("00_packages.R")
source("utils.R")
source("model.R")          # shared process model (used by process_predict)
source("01_data.R")

library(randomForest)
library(caret)

aca_modelled <- readRDS("data/aca_modelled_train.rds")
params       <- readRDS("data/model_params.rds")
era5         <- read_csv("data/era5_banyoles.csv", show_col_types = FALSE) |>
  arrange(date) |>
  mutate(air_temp_14d = zoo::rollmeanr(air_temp_C, k = 14, fill = air_temp_C))

# ─────────────────────────────────────────────────────────────────────────────
# 3.1  Feature matrix
#
# Physical features  — depth, season, ERA5 forcing
# Process output     — T_pred (model temperature at that depth)
# Wind               — drives mixing depth (new vs ILEC version)
# ─────────────────────────────────────────────────────────────────────────────

FEATURES <- c("sin_doy", "cos_doy", "stratified",
              "rel_depth", "depth_x_summer",
              "T_pred", "air_temp_14d", "wind_ms", "radiation_MJ_m2")

ml_data <- aca_modelled |>
  select(date, depth_m,
         all_of(FEATURES),
         temp_residual, DO_residual, temp_C, DO_mgl, T_pred, DO_pred)

# ─────────────────────────────────────────────────────────────────────────────
# 3.2  Temperature RF — leave-one-campaign-out CV
# ─────────────────────────────────────────────────────────────────────────────

df_temp <- ml_data |>
  select(date, depth_m, all_of(FEATURES), temp_residual) |>
  drop_na()

n_campaigns <- n_distinct(df_temp$date)
message("Training temperature RF on ", nrow(df_temp), " observations | ",
        n_campaigns, " campaigns | LOCO CV")

set.seed(42)
cv_folds_temp <- groupKFold(df_temp$date, k = n_campaigns)

ctrl_temp <- trainControl(
  method = "cv", index = cv_folds_temp, savePredictions = TRUE
)

rf_temp <- train(
  x         = df_temp[, FEATURES],
  y         = df_temp$temp_residual,
  method    = "rf",
  trControl = ctrl_temp,
  ntree     = 300,
  importance = TRUE,
  tuneGrid  = data.frame(mtry = c(2, 3, 4))
)
print(rf_temp)

# Variable importance
p_imp_temp <- varImp(rf_temp)$importance |>
  rownames_to_column("feature") |>
  arrange(desc(Overall)) |>
  mutate(feature = fct_reorder(feature, Overall)) |>
  ggplot(aes(x = Overall, y = feature, fill = Overall)) +
  geom_col() +
  scale_fill_viridis_c(guide = "none") +
  labs(title = "Variable importance — temperature residual RF",
       x = "Importance (%IncMSE)", y = NULL) +
  theme_workshop()

print(p_imp_temp)
png("outputs/13_varimp_temp.png", width = 20, height = 10, 
    units="cm", res=300)
p_imp_temp
dev.off()

# ─────────────────────────────────────────────────────────────────────────────
# 3.3  DO RF — leave-one-campaign-out CV
# ─────────────────────────────────────────────────────────────────────────────

df_do <- ml_data |>
  select(date, depth_m, all_of(FEATURES), DO_residual) |>
  drop_na()

n_do_campaigns <- n_distinct(df_do$date)
message("Training DO RF on ", nrow(df_do), " observations | ",
        n_do_campaigns, " campaigns | LOCO CV")

set.seed(42)
cv_folds_do <- groupKFold(df_do$date, k = n_do_campaigns)

ctrl_do <- trainControl(
  method = "cv", index = cv_folds_do, savePredictions = TRUE
)

rf_do <- train(
  x         = df_do[, FEATURES],
  y         = df_do$DO_residual,
  method    = "rf",
  trControl = ctrl_do,
  ntree     = 300,
  importance = TRUE,
  tuneGrid  = data.frame(mtry = c(2, 3, 4))
)
print(rf_do)

p_imp_do <- varImp(rf_do)$importance |>
  rownames_to_column("feature") |>
  arrange(desc(Overall)) |>
  mutate(feature = fct_reorder(feature, Overall)) |>
  ggplot(aes(x = Overall, y = feature, fill = Overall)) +
  geom_col() +
  scale_fill_viridis_c(guide = "none") +
  labs(title = "Variable importance — DO residual RF",
       x = "Importance (%IncMSE)", y = NULL) +
  theme_workshop()

print(p_imp_do)
png("outputs/14_varimp_do.png", width = 20, height = 10, 
    units="cm", res=300)
p_imp_do
dev.off()

# ─────────────────────────────────────────────────────────────────────────────
# 3.4  Hybrid assembly on training data
# ─────────────────────────────────────────────────────────────────────────────

hybrid_train <- ml_data |>
  drop_na(all_of(FEATURES)) |>
  mutate(
    temp_rf_correction = predict(rf_temp, newdata = pick(all_of(FEATURES))),
    do_rf_correction   = predict(rf_do,   newdata = pick(all_of(FEATURES))),
    T_hybrid           = T_pred  + temp_rf_correction,
    DO_hybrid          = pmax(0, DO_pred + do_rf_correction)
  )

# Observed vs hybrid predicted — mirror of plot 07 (process-only) after ML
metrics_temp_hybrid <- eval_metrics(hybrid_train$temp_C, hybrid_train$T_hybrid,
                                    "Hybrid (temperature)")
print(metrics_temp_hybrid)

p_temp_hybrid_vs <- hybrid_train |>
  filter(!is.na(temp_C)) |>
  ggplot(aes(x = temp_C, y = T_hybrid, colour = depth_m)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey30") +
  scale_colour_viridis_c("Depth (m)", option = "magma", direction = -1) +
  labs(title = "Temperature: observed vs hybrid model (ACA training campaigns)",
       subtitle = paste0("RMSE = ", metrics_temp_hybrid$RMSE,
                         " °C  |  R² = ", metrics_temp_hybrid$R2,
                         "  [compare with plot 07: process-only]"),
       x = "Observed (°C)", y = "Predicted (°C)") +
  theme_workshop()

print(p_temp_hybrid_vs)
png("outputs/15_temp_hybrid_vs_obs.png", width = 20, height = 10, 
    units="cm", res=300)
p_temp_hybrid_vs
dev.off()


# DISCUSSION: compare RMSE and scatter pattern with plot 07 (process-only).
# The ML correction tightens the fit, especially at intermediate depths where
# the thermocline shape was poorly captured by the simple physics.

# ─────────────────────────────────────────────────────────────────────────────
# 3.5  Process model helper — apply to any date × depth grid
#      Wraps the shared model (model.R) and adds the ML feature encoding.
# ─────────────────────────────────────────────────────────────────────────────

process_predict <- function(dates, depths, era5_df, params) {
  crossing(date = dates, depth_m = depths) |>
    left_join(era5_df |> select(date, air_temp_14d, wind_ms, radiation_MJ_m2),
              by = "date") |>
    mutate(
      doy            = yday(date),
      month          = month(date),
      stratified     = as.integer(month %in% 5:10),
      rel_depth      = depth_m / 50,
      depth_x_summer = rel_depth * stratified,
      sin_doy        = sin(2 * pi * doy / 365),
      cos_doy        = cos(2 * pi * doy / 365)
    ) |>
    run_process_model(params)
}

# ─────────────────────────────────────────────────────────────────────────────
# 3.6  Validate on ACA 2025–2026 test campaigns (surface, depth = 0)
# ─────────────────────────────────────────────────────────────────────────────

test_surface <- process_predict(
  dates  = unique(aca_test$date),
  depths = 0,
  era5_df = era5,
  params  = params
) |>
  mutate(
    temp_rf_correction = predict(rf_temp, newdata = pick(all_of(FEATURES))),
    do_rf_correction   = predict(rf_do,   newdata = pick(all_of(FEATURES))),
    T_hybrid           = T_pred  + temp_rf_correction,
    DO_hybrid          = pmax(0, DO_pred + do_rf_correction)
  ) |>
  left_join(aca_test |> filter(depth_m == 0) |>
              select(date, temp_C_obs = temp_C, DO_obs = DO_mgl),
            by = "date")

cat("\n--- Surface validation: ACA test campaigns (2021–2024) ---\n")
metrics_surface_temp <- eval_metrics(test_surface$temp_C_obs, test_surface$T_hybrid,
                                     "Hybrid (surface temp)")
metrics_surface_do   <- eval_metrics(test_surface$DO_obs, test_surface$DO_hybrid,
                                     "Hybrid (surface DO)")
print(metrics_surface_temp)
print(metrics_surface_do)

p_surface_val <- test_surface |>
  mutate(campaign = format(date, "%b %Y")) |>
  pivot_longer(c(T_pred, T_hybrid), names_to = "model", values_to = "pred_temp") |>
  mutate(model = recode(model, T_pred = "Process only", T_hybrid = "Hybrid")) |>
  ggplot(aes(x = reorder(campaign, date))) +
  geom_point(aes(y = temp_C_obs), colour = "black", size = 3, shape = 16) +
  geom_point(aes(y = pred_temp, colour = model, shape = model),
             size = 3, alpha = 0.85) +
  scale_colour_manual(values = c("Process only" = "steelblue",
                                  "Hybrid"       = "darkgreen")) +
  scale_x_discrete(NULL) +
  scale_y_continuous("Surface temperature (°C)") +
  labs(title = "Surface temperature validation — ACA test campaigns (2021–2024)",
       subtitle = "Black = observed | Blue = process model | Green = hybrid",
       colour = NULL, shape = NULL) +
  theme_workshop() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

print(p_surface_val)
png("outputs/16_surface_validation_temp.png", width = 20, height = 10,
    units="cm", res=300)
p_surface_val
dev.off()


# ─────────────────────────────────────────────────────────────────────────────
# 3.7  Generate full depth profiles for every test campaign date
#      (0–50 m at 1 m resolution) — input for the risk map in Part 4
# ─────────────────────────────────────────────────────────────────────────────

message("Generating hybrid depth profiles for test campaigns (0–50 m)...")

risk_profiles <- process_predict(
  dates   = unique(aca_test$date),
  depths  = 0:50,
  era5_df = era5,
  params  = params
) |>
  mutate(
    temp_rf_correction = predict(rf_temp, newdata = pick(all_of(FEATURES))),
    do_rf_correction   = predict(rf_do,   newdata = pick(all_of(FEATURES))),
    T_hybrid           = T_pred  + temp_rf_correction,
    DO_hybrid          = pmax(0, DO_pred + do_rf_correction)
  )

# Spot check: DO profiles for all test campaigns
p_do_profiles <- risk_profiles |>
  mutate(campaign = format(date, "%b %Y")) |>
  ggplot(aes(x = DO_hybrid, y = depth_m, colour = campaign, group = campaign)) +
  geom_path(linewidth = 1) +
  geom_vline(xintercept = 4, linetype = "dotted", colour = "firebrick",
             linewidth = 0.8) +
  annotate("text", x = 4.2, y = 48, label = "Hypoxia threshold\n(4 mg/L)",
           hjust = 0, colour = "firebrick", size = 3) +
  scale_y_reverse("Depth (m)") +
  scale_x_continuous("Hybrid DO prediction (mg/L)", limits = c(0, 14)) +
  scale_colour_viridis_d(option = "turbo", name = "Campaign") +
  labs(title = "Hybrid DO profiles — ACA test campaigns (2021–2024)",
       subtitle = "Summer stratification and hypolimnetic anoxia below ~20 m") +
  theme_workshop()

print(p_do_profiles)
png("outputs/17_hybrid_DO_profiles.png", width = 20, height = 10, 
    units="cm", res=300)
p_do_profiles
dev.off()

# ─────────────────────────────────────────────────────────────────────────────
# 3.8  Save everything for Part 4
# ─────────────────────────────────────────────────────────────────────────────

saveRDS(
  list(
    rf_temp        = rf_temp,
    rf_do          = rf_do,
    features       = FEATURES,
    hybrid_train   = hybrid_train,
    test_surface   = test_surface,
    risk_profiles  = risk_profiles
  ),
  "data/ml_models.rds"
)

message("\n--- Part 3 complete ---")
message("Temperature RF  LOCO CV RMSE : ",
        round(min(rf_temp$results$RMSE), 2), " °C")
message("DO RF           LOCO CV RMSE : ",
        round(min(rf_do$results$RMSE),   2), " mg/L")
message("Surface temp validation RMSE : ", metrics_surface_temp$RMSE, " °C")
message("Surface DO   validation RMSE : ", metrics_surface_do$RMSE,   " mg/L")
message("Risk profiles generated: ", n_distinct(risk_profiles$date),
        " test campaigns × 51 depths")
