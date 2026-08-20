# =============================================================================
# model.R
# The Banyoles process-based model — ONE definition, shared everywhere.
#
# Sourced by:
#   03_process_model.R      — fit residuals on training campaigns
#   04_ml_residuals.R       — generate physics baseline for ML
#   06_forecast_tomorrow.R  — next-day swimmer forecast
#   app/process_playground  — interactive Shiny tuning
#
# Design principle (workshop narrative):
#   The physics is calibrated to be CREDIBLE (surface + bulk structure right),
#   but deliberately leaves structured residuals for ML to learn:
#     • intermittent karstic groundwater warming at 25–28 m (some campaigns)
#     • the metalimnion temperature plateau
#     • sharp hypolimnetic anoxia and surface biological O2 signals
# =============================================================================

# ─────────────────────────────────────────────────────────────────────────────
# Default parameters — calibrated on ACA training campaigns (2007–2018)
# Each is exposed as a slider in the Shiny playground.
# ─────────────────────────────────────────────────────────────────────────────

default_params <- function() {
  list(
    # --- surface heat ---
    k_heat       = 0.90,   # air–water thermal coupling (0–1)
    T_mean_air   = 14.7,   # long-term mean air temperature [°C]
    solar_offset = 3.0,    # summer solar warming above air-coupled temp [°C]

    # --- mixed layer / thermocline ---
    mld_max      = 8,      # winter mixed-layer depth [m]
    mld_summer   = 5,      # summer mixed-layer depth [m]
    therm_scale  = 6.0,    # e-folding depth of the thermocline decay [m]
    T_deepwater  = 11.5,   # background hypolimnetic temperature [°C]

    # --- karstic groundwater (deliberately weak — ML learns the rest) ---
    T_gw         = 13.0,   # groundwater end-member temperature [°C]
    gw_depth     = 25,     # depth below which groundwater is felt [m]
    gw_scale     = 8,      # e-folding scale of groundwater influence [m]

    # --- dissolved oxygen ---
    do_halfdepth = 20,     # depth of 50% O2 saturation fraction (summer) [m]
    do_slope     = 7.0     # sigmoid steepness of the O2 decline [m]
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# O2 saturation — Benson & Krause (1984) freshwater formula (0–40°C)
# ─────────────────────────────────────────────────────────────────────────────

do_saturation <- function(temp_C, salinity_ppt = 0) {
  T_K <- temp_C + 273.15
  ln_do_sat <- (
    -139.34411 +
    (1.575701e5  / T_K) -
    (6.642308e7  / T_K^2) +
    (1.2438e10   / T_K^3) -
    (8.621949e11 / T_K^4)
  ) - salinity_ppt * (0.017674 - 10.754 / T_K + 2140.7 / T_K^2)
  exp(ln_do_sat)
}

# ─────────────────────────────────────────────────────────────────────────────
# The process model.
#
# Input:  a data frame/tibble with columns
#           depth_m, doy, month, air_temp_14d, radiation_MJ_m2  (wind optional)
#         and a params list (see default_params()).
# Output: the same data frame with columns added:
#           mld, T_surface_pred, T_pred, DO_sat, o2_frac, DO_pred
#
# Fully vectorised — works on one row, one profile, or a whole grid.
# ─────────────────────────────────────────────────────────────────────────────

run_process_model <- function(df, params = default_params()) {
  df$mld <- params$mld_max -
    (params$mld_max - params$mld_summer) * sin(pi * df$doy / 365)

  # Surface temperature: air coupling + summer solar warming.
  # solar_offset scales with day-of-year so winter surfaces are not over-warmed.
  season_solar <- pmax(0, sin(pi * df$doy / 365))
  df$T_surface_pred <- params$k_heat * df$air_temp_14d +
    (1 - params$k_heat) * params$T_mean_air +
    params$solar_offset * season_solar

  # Depth-resolved temperature: uniform mixed layer, then an exponential
  # thermocline decay toward the hypolimnion, plus a weak exponential
  # groundwater warming below gw_depth. Exponential (not linear) decay
  # captures the sharp metalimnetic drop seen in the profiles.
  gw_bump <- (params$T_gw - params$T_deepwater) *
    (1 - exp(-pmax(0, df$depth_m - params$gw_depth) / params$gw_scale))

  df$T_pred <- with(df, ifelse(
    depth_m <= mld,
    T_surface_pred,
    params$T_deepwater +
      (T_surface_pred - params$T_deepwater) *
      exp(-(depth_m - mld) / params$therm_scale) +
      gw_bump
  ))

  # Dissolved oxygen: saturation at the predicted temperature, scaled by a
  # sigmoid O2 fraction that collapses in the stratified hypolimnion.
  # Stratified season (May–Oct) pulls the half-depth shallower.
  strat <- ifelse(df$month %in% 5:10, 1, 0.35)
  half  <- params$do_halfdepth / strat
  df$DO_sat  <- do_saturation(df$T_pred)
  df$o2_frac <- 1 / (1 + exp((df$depth_m - half) / params$do_slope))
  df$DO_pred <- df$DO_sat * df$o2_frac

  df
}
