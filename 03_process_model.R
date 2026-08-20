# =============================================================================
# 03_process_model.R
# PART 3 — Process-based prediction on ACA training campaigns (20 min)
#
# Goals:
#   • Apply a 1D physics model to each ACA training campaign date
#   • Forcing: ERA5 14-day rolling mean air temperature (from 01b / 01c)
#   • Predict temperature and DO profiles down to 50 m
#   • Compute residuals at each observed depth — the ML target in Part 3
#   • Diagnose systematic biases: groundwater anomaly, summer anoxia
#
# The model is INTENTIONALLY simple. Its job is to produce physically
# structured predictions that ML can improve — not to be state-of-the-art.
# you can use this pipeline an run your own lake model (e.g. GLM, Simstrat, etc.)
# =============================================================================

source("00_packages.R")
source("utils.R")
source("model.R")          # the shared process model (physics lives here)
source("01_data.R")        # loads: aca, aca_train, aca_test

# ─────────────────────────────────────────────────────────────────────────────
# 2.1  The process model — one shared definition (see model.R)
#
# run_process_model() applies a 1D physics model to any (depth × date) grid:
#   Temperature: mixed layer → exponential thermocline → hypolimnion, plus a
#                weak karstic groundwater warming below gw_depth.
#   Oxygen:      saturation at the predicted temperature, scaled by a sigmoid
#                O2 fraction that collapses in the stratified hypolimnion.
#
# The parameters (default_params()) are the same ones the Shiny playground
# exposes as sliders — try app/app.R to tune them interactively.
# ─────────────────────────────────────────────────────────────────────────────

params <- default_params()

# Sanity check — O2 saturation curve
tibble(temp_C = c(0, 10, 20, 25, 30), do_sat = do_saturation(temp_C)) |> print()
# Expected: ~14.6, 11.3, 9.1, 8.2, 7.5 mg/L

# ─────────────────────────────────────────────────────────────────────────────
# 2.2  Apply the model to the depth-resolved training profiles
# ─────────────────────────────────────────────────────────────────────────────

train_profiles <- aca_train |> filter(!surface_only)

modelled <- run_process_model(train_profiles, params)

# ─────────────────────────────────────────────────────────────────────────────
# 2.3  Temperature residuals — the ML target in Part 3
# ─────────────────────────────────────────────────────────────────────────────

temp_model <- modelled |>
  filter(!is.na(temp_C)) |>
  mutate(temp_residual = temp_C - T_pred)

# ─────────────────────────────────────────────────────────────────────────────
# 2.4  Evaluate temperature model
# ─────────────────────────────────────────────────────────────────────────────

metrics_temp <- eval_metrics(temp_model$temp_C, temp_model$T_pred,
                             "Process-based (temperature)")
print(metrics_temp)

# Observed vs predicted
p_temp_vs <- temp_model |>
  ggplot(aes(x = temp_C, y = T_pred, colour = depth_m)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey30") +
  scale_colour_viridis_c("Depth (m)", option = "magma", direction = -1) +
  labs(title = "Temperature: observed vs process model (ACA training campaigns)",
       subtitle = paste0("RMSE = ", metrics_temp$RMSE, " °C  |  R² = ", metrics_temp$R2),
       x = "Observed (°C)", y = "Predicted (°C)") +
  theme_workshop()

print(p_temp_vs)
png("outputs/09_temp_obs_vs_pred.png", width = 20, height = 10, 
    units="cm", res=300)
p_temp_vs
dev.off()

# Residuals by depth and campaign date
p_temp_resid <- temp_model |>
  mutate(campaign = format(date, "%b %Y")) |>
  ggplot(aes(x = temp_residual, y = depth_m, colour = campaign)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_path(aes(group = campaign), alpha = 0.4) +
  geom_point(size = 2, alpha = 0.8) +
  scale_y_reverse("Depth (m)") +
  scale_x_continuous("Temperature residual (obs − pred, °C)") +
  scale_colour_viridis_d(option = "turbo", name = "Campaign") +
  labs(title = "Temperature residuals by depth — process model",
       subtitle = "Warm colours = underprediction; cool = overprediction") +
  theme_workshop()

print(p_temp_resid)
png("outputs/10_temp_residuals.png", width = 20, height = 10, 
    units="cm", res=300)
p_temp_resid
dev.off()

# DISCUSSION: the deep layer (25–28 m) is warmer than the model predicts in
# some campaigns, it could related to the intermittent karstic groundwater 
# intrusion (~18°C). The process model adds only a weak, constant 
# groundwater term (T_gw), so this campaign-to-campaign warm anomaly survives
# as a structured residual for ML.

# ─────────────────────────────────────────────────────────────────────────────
# 2.5  DO model  (already computed by run_process_model — see model.R)
#
# DO_pred = DO_sat(T_pred) × o2_frac(depth, season)
#
# o2_frac is a sigmoid O2-demand proxy: ~1 at the surface, collapsing in the
# stratified hypolimnion. It deliberately cannot reproduce the abrupt anoxia
# or surface biological supersaturation — that is ML's job.
# ─────────────────────────────────────────────────────────────────────────────

do_model <- modelled |>
  filter(!is.na(DO_mgl), !is.na(DO_pred)) |>
  mutate(DO_residual = DO_mgl - DO_pred)

metrics_do <- eval_metrics(do_model$DO_mgl, do_model$DO_pred,
                           "Process-based (DO)")
print(metrics_do)

# DO residuals
p_do_resid <- do_model |>
  mutate(campaign = format(date, "%b %Y")) |>
  ggplot(aes(x = DO_residual, y = depth_m, colour = campaign)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_path(aes(group = campaign), alpha = 0.4) +
  geom_point(size = 2, alpha = 0.8) +
  scale_y_reverse("Depth (m)") +
  scale_x_continuous("DO residual (obs − pred, mg/L)") +
  scale_colour_viridis_d(option = "turbo", name = "Campaign") +
  labs(title = "DO residuals by depth — process model",
       subtitle = "Large negative residuals = model overestimates O2 in anoxic deep water") +
  theme_workshop()

print(p_do_resid)
png("outputs/11_DO_residuals.png", width = 20, height = 10, 
    units="cm", res=300)
p_do_resid
dev.off()

# DISCUSSION: the model overestimates DO at depth (positive residuals in deep
# summer layers). This is the anoxia signal the process model can't reproduce —
# we deliberately left it for ML to learn.

# ─────────────────────────────────────────────────────────────────────────────
# 2.6  Safe swimming zone — process model estimate
#
# Define the safe zone as the depth range where:
#   DO_pred  ≥ 4 mg/L  (above hypoxia threshold)
#   temp_C   ≥ 15°C    (comfortable for swimming)
#
# This gives participants a direct link between the model output and the
# swimmer safety question.
# ─────────────────────────────────────────────────────────────────────────────

safe_zone <- do_model |>
  mutate(
    safe_DO   = DO_pred >= 4,
    safe_temp = T_pred  >= 15,
    safe      = safe_DO & safe_temp
  ) |>
  group_by(date) |>
  summarise(
    safe_depth_m   = if (any(safe)) max(depth_m[safe], na.rm = TRUE) else 0,
    air_temp_14d   = first(air_temp_14d),
    surface_temp_C = T_pred[which.min(depth_m)],
    .groups = "drop"
  ) |>
  mutate(campaign = format(date, "%b\n%Y"))

cat("\n--- Process model: safe swimming depth (DO ≥ 4 mg/L & T ≥ 15°C) ---\n")
print(safe_zone |> select(campaign, surface_temp_C, safe_depth_m))

p_safe <- safe_zone |>
  ggplot(aes(x = reorder(campaign, date), y = safe_depth_m)) +
  geom_col(fill = "steelblue", alpha = 0.8, width = 0.6) +
  scale_y_reverse("Safe swimming depth (m)", limits = c(55, 0)) +
  scale_x_discrete(NULL) +
  labs(title = "Process model estimate: safe swimming depth per campaign",
       subtitle = "Below this depth: hypoxic (DO < 4 mg/L) or cold (T < 15°C)",
       caption = "ACA training campaigns 2007–2018 | Station 0450401") +
  theme_workshop()

print(p_safe)
png("outputs/12_safe_depth_process.png", width = 20, height = 10, 
    units="cm", res=300)
p_safe
dev.off()

# ─────────────────────────────────────────────────────────────────────────────
# 2.7  Save intermediate objects for Part 3
# ─────────────────────────────────────────────────────────────────────────────

aca_modelled_train <- modelled |>
  left_join(temp_model |> select(date, depth_m, temp_residual),
            by = c("date", "depth_m")) |>
  left_join(do_model |> select(date, depth_m, DO_residual),
            by = c("date", "depth_m"))

saveRDS(aca_modelled_train, "data/aca_modelled_train.rds")
saveRDS(params,             "data/model_params.rds")

message("\n--- Part 2 complete ---")
message("Process model performance:")
print(metrics_temp)
print(metrics_do)
message("\nSaved: data/aca_modelled_train.rds")
message("Key insight: DO model fails in the anoxic hypolimnion (below ~15–20 m).")
message("Temperature model misses karstic groundwater warming at depth.")
message("Both residual patterns are structured → learnable by ML in Part 3.")
