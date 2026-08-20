# =============================================================================
# 06_forecast_tomorrow.R
# Part 6 — Live next-day swimmer safety forecast for Lake Banyoles
#         driven by an ENSEMBLE weather forecast
#
# Workflow:
#   1. Download the last 14 days of observed/deterministic weather (for the
#      air_temp_14d rolling mean) from the Open-Meteo forecast API
#   2. Download an ENSEMBLE forecast for tomorrow (Open-Meteo Ensemble API,
#      Google DeepMind WeatherNext 2 — a ML weather model, 64 members) and
#      aggregate each member to daily
#   3. Load trained hybrid models (from Part 4)
#   4. Run process model + RF corrections for tomorrow, once per ensemble
#      member (0–50 m depth grid) — this propagates weather forecast
#      uncertainty through to the swimmer-risk forecast
#   5. Summarise the ensemble (median + spread) and the probability of a
#      "safe" classification at each depth
#   6. Print a forecast card and save ensemble spread + probability plots
#
# No model training here — models are loaded from data/ml_models.rds.
# Requires an internet connection.
# =============================================================================

source("00_packages.R")
source("utils.R")
source("model.R")          # shared process model

library(httr2)
library(jsonlite)
library(randomForest)
library(caret)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# ─────────────────────────────────────────────────────────────────────────────
# 6.1  Setup
# ─────────────────────────────────────────────────────────────────────────────

LAT             <- 42.1209
LON             <- 2.7582
TARGET_DATE     <- Sys.Date() + 1
DEPTHS          <- 0:50
ENSEMBLE_MODEL  <- "google_weathernext2_ensemble"   # Open-Meteo API model id (free, no key)
ENSEMBLE_LABEL  <- "Google WeatherNext 2"           # human-readable name for messages/plots

message("Ensemble swimmer safety forecast for: ", TARGET_DATE)
message("Location: Lake Banyoles (", LAT, "°N, ", LON, "°E)")

ml     <- readRDS("data/ml_models.rds")
params <- readRDS("data/model_params.rds")

rf_temp  <- ml$rf_temp
rf_do    <- ml$rf_do
FEATURES <- ml$features

# ─────────────────────────────────────────────────────────────────────────────
# 6.2  Deterministic weather — last 14 days, for the rolling-mean air temp
#      (the recent past is essentially observed, no need for an ensemble here)
# ─────────────────────────────────────────────────────────────────────────────

DAILY_VARS <- paste(c(
  "temperature_2m_mean",
  "temperature_2m_max",
  "precipitation_sum",
  "windspeed_10m_mean",
  "shortwave_radiation_sum"
), collapse = ",")

message("Requesting recent weather history from Open-Meteo...")

# forecast_days = 2 so this same deterministic call also covers tomorrow —
# needed below as a radiation fallback (see 6.3).
resp_hist <- request("https://api.open-meteo.com/v1/forecast") |>
  req_url_query(
    latitude      = LAT,
    longitude     = LON,
    daily         = DAILY_VARS,
    past_days     = 14,
    forecast_days = 2,
    timezone      = "Europe/Madrid"
  ) |>
  req_timeout(30) |>
  req_perform()

if (resp_status(resp_hist) != 200) {
  stop("Open-Meteo history request failed — status: ", resp_status(resp_hist))
}

hist_parsed <- fromJSON(resp_body_string(resp_hist))

deterministic <- tibble(
  date            = as.Date(hist_parsed$daily$time),
  air_temp_C      = hist_parsed$daily$temperature_2m_mean,
  radiation_MJ_m2 = hist_parsed$daily$shortwave_radiation_sum
) |>
  arrange(date)

history <- deterministic |>
  filter(date < TARGET_DATE) |>
  slice_tail(n = 13)   # the 13 days immediately before tomorrow

if (nrow(history) < 13) {
  stop("Not enough recent history to build the 14-day rolling mean.")
}

message("History window: ", min(history$date), " → ", max(history$date),
        " (", nrow(history), " days)")

# WeatherNext (and some other ML weather models) don't forecast solar
# radiation — fall back to the deterministic model's radiation forecast for
# tomorrow, shared across all ensemble members. Everything else (temp, wind,
# precip) still comes from the full per-member ensemble below.
radiation_fallback <- deterministic |>
  filter(date == TARGET_DATE) |>
  pull(radiation_MJ_m2)

if (length(radiation_fallback) == 0 || is.na(radiation_fallback)) {
  stop("No deterministic radiation forecast available for ", TARGET_DATE,
       " to use as a fallback.")
}

# ─────────────────────────────────────────────────────────────────────────────
# 6.3  Ensemble weather for tomorrow — Open-Meteo Ensemble API
#      Each member is a physically plausible alternative realisation of
#      tomorrow's weather. We aggregate each member's hourly output to a
#      single day, exactly mirroring what the deterministic daily API does.
# ─────────────────────────────────────────────────────────────────────────────

message("Requesting ensemble forecast (", ENSEMBLE_MODEL, ") from Open-Meteo...")

resp_ens <- request("https://ensemble-api.open-meteo.com/v1/ensemble") |>
  req_url_query(
    latitude      = LAT,
    longitude     = LON,
    hourly        = "temperature_2m,precipitation,windspeed_10m,shortwave_radiation",
    models        = ENSEMBLE_MODEL,
    forecast_days = 2,
    timezone      = "Europe/Madrid"
  ) |>
  req_timeout(60) |>
  req_perform()

if (resp_status(resp_ens) != 200) {
  stop("Open-Meteo ensemble request failed — status: ", resp_status(resp_ens))
}

ens_hourly <- fromJSON(resp_body_string(resp_ens))$hourly

ens_tomorrow <- as_tibble(ens_hourly) |>
  mutate(across(-time, as.numeric),
         date = as.Date(substr(time, 1, 10))) |>
  filter(date == TARGET_DATE) |>
  select(-time, -date)

if (nrow(ens_tomorrow) == 0) {
  stop("Ensemble forecast does not include ", TARGET_DATE,
       ". Check your system date or increase forecast_days.")
}

# Long format: one row per (hour, member, variable), then aggregate to daily
# per member — mean for temp/wind, sum for precip, sum*0.0036 W/m2 -> MJ/m2
# for radiation (matches the deterministic daily API's own conversion).
ens_long <- ens_tomorrow |>
  pivot_longer(everything(), names_to = "col", values_to = "value") |>
  mutate(
    member   = if_else(str_detect(col, "_member"),
                       str_extract(col, "member\\d+"), "member00"),
    variable = str_remove(col, "_member\\d+$")
  )

ensemble_wx <- ens_long |>
  group_by(member, variable) |>
  summarise(
    agg = case_when(
      first(variable) == "temperature_2m"      ~ mean(value),
      first(variable) == "precipitation"        ~ sum(value),
      first(variable) == "windspeed_10m"        ~ mean(value),
      first(variable) == "shortwave_radiation"  ~ sum(value) * 0.0036
    ),
    .groups = "drop"
  ) |>
  pivot_wider(names_from = variable, values_from = agg) |>
  rename(
    air_temp_C      = temperature_2m,
    precip_mm       = precipitation,
    wind_ms         = windspeed_10m,
    radiation_MJ_m2 = shortwave_radiation
  ) |>
  mutate(
    date         = TARGET_DATE,
    air_temp_14d = (sum(history$air_temp_C) + air_temp_C) / 14
  )

if (all(is.na(ensemble_wx$radiation_MJ_m2))) {
  message("Model '", ENSEMBLE_MODEL, "' does not provide ensemble radiation ",
          "forecasts — using the deterministic radiation forecast (",
          round(radiation_fallback, 1), " MJ/m2) for all members.")
  ensemble_wx$radiation_MJ_m2 <- radiation_fallback
}

n_members <- nrow(ensemble_wx)
message("Ensemble size: ", n_members, " members (", ENSEMBLE_MODEL, ")")

# ─────────────────────────────────────────────────────────────────────────────
# 6.4  Process model + RF corrections for tomorrow, once per ensemble member
#      (0–50 m depth grid). Same shared model (model.R) as Parts 2–3, then
#      the ML residual step — exactly the pipeline used everywhere else.
# ─────────────────────────────────────────────────────────────────────────────

profiles <- crossing(member = ensemble_wx$member, depth_m = DEPTHS) |>
  left_join(ensemble_wx, by = "member") |>
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
    do_rf_correction   = predict(rf_do,   newdata = pick(all_of(FEATURES))),
    T_hybrid           = T_pred  + temp_rf_correction,
    DO_hybrid          = pmax(0, DO_pred + do_rf_correction)
  )

# ─────────────────────────────────────────────────────────────────────────────
# 6.5  Risk classification per member (same thresholds as Part 4)
# ─────────────────────────────────────────────────────────────────────────────

risk <- profiles |>
  mutate(
    risk_DO = case_when(
      DO_hybrid < 2 ~ "anoxic",
      DO_hybrid < 4 ~ "hypoxic",
      DO_hybrid < 6 ~ "caution",
      TRUE          ~ "safe"
    ),
    risk_temp = if_else(T_hybrid < 15, "cold", "ok"),
    risk_composite = case_when(
      risk_DO == "anoxic"  ~ "anoxic",
      risk_DO == "hypoxic" ~ "hypoxic",
      risk_temp == "cold"  ~ "cold",
      risk_DO == "caution" ~ "caution",
      TRUE                 ~ "safe"
    ),
    risk_composite = factor(risk_composite,
                            levels = c("safe", "caution", "cold",
                                       "hypoxic", "anoxic"))
  )

# Per-member safe depth, then the ensemble summary of that single number
safe_depth_by_member <- risk |>
  summarise(
    safe_depth_m = if (any(risk_composite == "safe"))
      max(depth_m[risk_composite == "safe"]) else 0,
    .by = member
  )

safe_depth_median <- median(safe_depth_by_member$safe_depth_m)
safe_depth_lo      <- quantile(safe_depth_by_member$safe_depth_m, 0.10)
safe_depth_hi      <- quantile(safe_depth_by_member$safe_depth_m, 0.90)

# Ensemble summary by depth: median/IQR of T and DO, and P(safe)
ensemble_summary <- risk |>
  summarise(
    T_median   = median(T_hybrid),
    T_lo       = quantile(T_hybrid, 0.10),
    T_hi       = quantile(T_hybrid, 0.90),
    DO_median  = median(DO_hybrid),
    DO_lo      = quantile(DO_hybrid, 0.10),
    DO_hi      = quantile(DO_hybrid, 0.90),
    p_safe     = mean(risk_composite == "safe"),
    .by = depth_m
  ) |>
  arrange(depth_m)

surface_summary <- filter(ensemble_summary, depth_m == 0)
overall_status  <- if (safe_depth_median >= 5) "SAFE TO SWIM" else "CAUTION — CHECK CONDITIONS"

# ─────────────────────────────────────────────────────────────────────────────
# 6.6  Forecast card
# ─────────────────────────────────────────────────────────────────────────────

div  <- strrep("─", 66)
line <- function(...) cat(sprintf("│ %-64s │\n", paste0(...)))

cat("\n┌", div, "┐\n", sep = "")
line("LAKE BANYOLES  ·  ENSEMBLE SWIMMER FORECAST  ·  ",
     format(TARGET_DATE, "%d %B %Y"))
cat("├", div, "┤\n", sep = "")
line("Status              : ", overall_status)
line("Ensemble size       : ", n_members, " members (", ENSEMBLE_MODEL, ")")
line("Air temp (median)   : ", round(median(ensemble_wx$air_temp_C), 1),
     " °C  [", round(quantile(ensemble_wx$air_temp_C, 0.1), 1), "-",
     round(quantile(ensemble_wx$air_temp_C, 0.9), 1), " °C, 10-90%]")
line("")
line("Surface temp        : ", round(surface_summary$T_median, 1),
     " °C  [", round(surface_summary$T_lo, 1), "-",
     round(surface_summary$T_hi, 1), " °C]")
line("Surface DO          : ", round(surface_summary$DO_median, 1),
     " mg/L  [", round(surface_summary$DO_lo, 1), "-",
     round(surface_summary$DO_hi, 1), " mg/L]")
line("Safe depth (median) : ", safe_depth_median,
     " m  [", safe_depth_lo, "-", safe_depth_hi, " m, 10-90%]")
line("                      DO >= 6 mg/L  &  T >= 15 degC")
cat("├", div, "┤\n", sep = "")
line("Hybrid model (process + random forest)  |  Open-Meteo ensemble API")
cat("└", div, "┘\n\n", sep = "")

# ─────────────────────────────────────────────────────────────────────────────
# 6.7  Ensemble spread plot — median line + 10-90% ribbon per depth
# ─────────────────────────────────────────────────────────────────────────────

ens_long_plot <- ensemble_summary |>
  select(depth_m, T_median, T_lo, T_hi, DO_median, DO_lo, DO_hi) |>
  pivot_longer(-depth_m,
               names_to = c("variable", ".value"),
               names_pattern = "(T|DO)_(median|lo|hi)") |>
  mutate(variable = recode(variable,
                           T  = "Temperature (°C)",
                           DO = "Dissolved oxygen (mg/L)"))

threshold_lines <- tibble(
  variable = c("Temperature (°C)", "Dissolved oxygen (mg/L)"),
  xint     = c(15, 6),
  label    = c("15 °C", "6 mg/L")
)

p_forecast <- ens_long_plot |>
  ggplot(aes(y = depth_m)) +
  geom_ribbon(aes(xmin = lo, xmax = hi, fill = variable),
              alpha = 0.25, show.legend = FALSE) +
  geom_path(aes(x = median, colour = variable), linewidth = 1.4,
            show.legend = FALSE) +
  geom_vline(data = threshold_lines,
             aes(xintercept = xint),
             linetype = "dotted", colour = "firebrick", linewidth = 0.8) +
  geom_text(data = threshold_lines,
            aes(x = xint, y = 48, label = label),
            hjust = -0.15, colour = "firebrick", size = 3.2) +
  facet_wrap(~variable, scales = "free_x") +
  scale_y_reverse("Depth (m)", breaks = seq(0, 50, 10)) +
  scale_x_continuous(NULL, expand = expansion(mult = c(0.05, 0.18))) +
  scale_colour_manual(values = c(
    "Temperature (°C)"        = "#e07b30",
    "Dissolved oxygen (mg/L)" = "#4f94cd"
  )) +
  scale_fill_manual(values = c(
    "Temperature (°C)"        = "#e07b30",
    "Dissolved oxygen (mg/L)" = "#4f94cd"
  )) +
  labs(
    title   = paste0("Ensemble swimmer safety forecast — Lake Banyoles — ",
                     format(TARGET_DATE, "%d %B %Y")),
    subtitle = paste0(
      n_members, "-member ensemble  |  median surface: ",
      round(surface_summary$T_median, 1), " °C, ",
      round(surface_summary$DO_median, 1), " mg/L DO  |  ",
      "Safe depth (median): ", safe_depth_median, " m  |  ",
      "shaded band = 10-90% ensemble range"
    ),
    caption = paste0(
      "Hybrid model (process + RF)  |  Forcing: Open-Meteo ", ENSEMBLE_LABEL,
      " ensemble API  |  Thresholds: EU Bathing Water Directive"
    )
  ) +
  theme_workshop()

print(p_forecast)

forecast_file <- paste0("outputs/22_forecast_", TARGET_DATE, ".png")
png(forecast_file, width = 20, height = 10, units = "cm", res = 300)
print(p_forecast)
dev.off()

# ─────────────────────────────────────────────────────────────────────────────
# 6.8  P(safe) by depth — the probabilistic payoff of the ensemble
# ─────────────────────────────────────────────────────────────────────────────

p_prob_safe <- ensemble_summary |>
  ggplot(aes(x = p_safe, y = depth_m)) +
  geom_ribbon(aes(xmin = 0, xmax = p_safe), fill = "#2A9D8F", alpha = 0.35) +
  geom_path(colour = "#2A9D8F", linewidth = 1.4) +
  geom_vline(xintercept = 0.5, linetype = "dotted", colour = "grey40") +
  scale_y_reverse("Depth (m)", breaks = seq(0, 50, 10)) +
  scale_x_continuous("P(safe to swim)  — fraction of ensemble members",
                     labels = scales::percent, limits = c(0, 1)) +
  labs(
    title = paste0("Probability of safe swimming conditions by depth — ",
                   format(TARGET_DATE, "%d %B %Y")),
    subtitle = paste0(n_members, "-member ensemble  |  safe = DO ≥ 6 mg/L & T ≥ 15 °C"),
    caption = "Hybrid model (process + RF)  |  Open-Meteo ensemble API"
  ) +
  theme_workshop()

print(p_prob_safe)

prob_file <- paste0("outputs/23_forecast_prob_safe_", TARGET_DATE, ".png")
png(prob_file, width = 20, height = 10, units = "cm", res = 300)
print(p_prob_safe)
dev.off()

message("Forecast complete. Outputs: ", forecast_file, ", ", prob_file)
