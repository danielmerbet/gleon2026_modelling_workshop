# =============================================================================
# 07_validate_independent.R
# Part 7 — independent validation against independent temperature soundings
#
# data/validate_temperature.csv (see 01c_prepare_data_validate.R) is citizen-science
# monitoring shared by Josep Pascual (meteolestartit.cat) — many thanks to him
# for making it available to the workshop.
#
# This gives us 60+ genuinely out-of-sample campaigns (2023-2026) to check
# whether the hybrid model — fit only on ACA 2007-2018 and lightly tested on
# ACA 2021-2024 — generalises to new years it has never seen.
# =============================================================================

source("00_packages.R")
source("utils.R")
source("model.R")

ml     <- readRDS("data/ml_models.rds")
params <- readRDS("data/model_params.rds")

rf_temp  <- ml$rf_temp
FEATURES <- ml$features

era5 <- read_csv("data/era5_banyoles.csv", show_col_types = FALSE) |>
  arrange(date) |>
  mutate(air_temp_14d = zoo::rollmeanr(air_temp_C, k = 14, fill = air_temp_C))

# ─────────────────────────────────────────────────────────────────────────────
# 7.1  Build the validation set — site A, within the model's fitted depth
#      range (0-50 m) and within ERA5 forcing coverage
# ─────────────────────────────────────────────────────────────────────────────

validate <- read_csv("data/validate_temperature.csv", show_col_types = FALSE)

validate_A <- validate |>
  filter(site == "A", depth_m <= 50) |>
  left_join(era5 |> select(date, air_temp_14d, wind_ms, radiation_MJ_m2),
            by = "date") |>
  filter(!is.na(air_temp_14d)) |>
  mutate(
    doy            = yday(date),
    month          = month(date),
    stratified     = as.integer(month %in% 5:10),
    rel_depth      = depth_m / 50,
    depth_x_summer = rel_depth * stratified,
    sin_doy        = sin(2 * pi * doy / 365),
    cos_doy        = cos(2 * pi * doy / 365)
  ) |>
  run_process_model(params) |>
  mutate(
    temp_rf_correction = predict(rf_temp, newdata = pick(all_of(FEATURES))),
    T_hybrid            = T_pred + temp_rf_correction
  )

message("Validation set: ", n_distinct(validate_A$campaign_id), " validate campaigns (site A) | ",
        nrow(validate_A), " depth observations | ",
        min(validate_A$date), " to ", max(validate_A$date))

# ─────────────────────────────────────────────────────────────────────────────
# 7.2  Metrics — process-only vs. hybrid, on data the model has never seen
# ─────────────────────────────────────────────────────────────────────────────

metrics_process_validate <- eval_metrics(validate_A$temp_C, validate_A$T_pred,   "Process only (validate A)")
metrics_hybrid_validate  <- eval_metrics(validate_A$temp_C, validate_A$T_hybrid, "Hybrid (validate A)")

cat("\n--- Independent validation: validate site A, 2023-2026 (never seen in training) ---\n")
print(metrics_process_validate)
print(metrics_hybrid_validate)

# ─────────────────────────────────────────────────────────────────────────────
# 7.3  Observed vs predicted, full depth profiles
# ─────────────────────────────────────────────────────────────────────────────

p_validate_vs <- validate_A |>
  ggplot(aes(x = temp_C, y = T_hybrid, colour = depth_m)) +
  geom_point(size = 2, alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey30") +
  scale_colour_viridis_c("Depth (m)", option = "magma", direction = -1) +
  labs(title = "Temperature: observed vs hybrid model (validate site A, independent validation)",
       subtitle = paste0("RMSE = ", metrics_hybrid_validate$RMSE,
                         " °C  |  R² = ", metrics_hybrid_validate$R2,
                         "  |  n = ", nrow(validate_A), " obs across ",
                         n_distinct(validate_A$campaign_id), " campaigns"),
       x = "Observed (°C)", y = "Predicted (°C)") +
  theme_workshop()

print(p_validate_vs)
png("outputs/24_validate_validation_scatter.png", width = 20, height = 10,
    units="cm", res=300)
p_validate_vs
dev.off()

# ─────────────────────────────────────────────────────────────────────────────
# 7.4  Surface temperature time series — every campaign, observed vs both models
# ─────────────────────────────────────────────────────────────────────────────

p_validate_surface <- validate_A |>
  filter(depth_m == min(depth_m)) |>
  pivot_longer(c(T_pred, T_hybrid), names_to = "model", values_to = "pred_temp") |>
  mutate(model = recode(model, T_pred = "Process only", T_hybrid = "Hybrid")) |>
  ggplot(aes(x = date)) +
  geom_point(aes(y = temp_C), colour = "black", size = 2, shape = 16) +
  geom_point(aes(y = pred_temp, colour = model, shape = model), size = 2, alpha = 0.85) +
  scale_colour_manual(values = c("Process only" = "steelblue", "Hybrid" = "darkgreen")) +
  labs(title = "Surface temperature — validate site A, 2023-2026 (independent validation)",
       subtitle = "Black = observed | Blue = process model | Green = hybrid",
       x = NULL, y = "Surface temperature (°C)", colour = NULL, shape = NULL) +
  theme_workshop()

print(p_validate_surface)
png("outputs/25_validate_validation_timeseries.png", width = 20, height = 10,
    units="cm", res=300)
p_validate_surface
dev.off()

message("\n--- Part 7 complete ---")
message("Process-only RMSE on validate A : ", metrics_process_validate$RMSE, " °C")
message("Hybrid       RMSE on validate A : ", metrics_hybrid_validate$RMSE,  " °C")
message("Compare with ACA test RMSE from 04_ml_residuals.R to check generalisation.")
