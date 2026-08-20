# =============================================================================
# 01_data.R
# Load, clean, and prepare the ACA monitoring data for modelling.
#
# Source: Agència Catalana de l'Aigua (ACA) — station 0450401
#         Lake Banyoles, holomictic basin
# File:   data/data.csv 
# Period: 2007–2024 | 9 summer field campaigns
# Variables: temperature (°C), dissolved oxygen (mg/L), DO saturation (%),
#            chlorophyll-a (µg/L), conductivity (µS/cm), pH, turbidity (NTU)
# Depth range: 0–50 m
#
# Joins: ERA5 daily meteorological forcing (data/era5_banyoles.csv)
#        — 14-day rolling mean air temp as the primary process-model driver
#
# Outputs (in global env):
#   aca       — full cleaned dataset with ERA5 forcing
#   aca_train — 2007–2018 (6 campaigns) → model fitting
#   aca_test  — 2021–2024 (3 campaigns) → validation
# =============================================================================

library(tidyverse)
library(lubridate)
library(zoo)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# ─────────────────────────────────────────────────────────────────────────────
# 1. Load and clean ACA profiles from data/data.csv
# ─────────────────────────────────────────────────────────────────────────────

raw <- read_csv(
  "data/data.csv",
  locale    = locale(encoding = "latin1"),
  col_types = cols(.default = "c"),
  show_col_types = FALSE
)

names(raw) <- c("station_code", "station_name", "site_code", "site_name",
                "utm_x", "utm_y", "variable", "timestamp",
                "value", "units", "depth_m", "comments")

aca_raw <- raw |>
  mutate(
    date    = as.Date(as.POSIXct(timestamp, format = "%m/%d/%Y %H:%M:%S")),
    value   = suppressWarnings(as.numeric(value)),
    depth_m = suppressWarnings(as.numeric(depth_m))
  ) |>
  filter(!is.na(date), !is.na(value))

# ─────────────────────────────────────────────────────────────────────────────
# 2. Map Catalan variable names to short English names; drop unused variables
# ─────────────────────────────────────────────────────────────────────────────

aca_vars <- aca_raw |>
  mutate(
    var_en = case_when(
      str_detect(variable, "Temperatura")   ~ "temp_C",
      str_detect(variable, "Oxigen")        ~ "DO_mgl",
      str_detect(variable, "Saturaci")      ~ "DO_sat_pct",
      str_detect(variable, "Clorofil")      ~ "chla_ugl",
      str_detect(variable, "Conductivitat") ~ "cond_uScm",
      str_detect(variable, "pH")            ~ "pH",
      str_detect(variable, "Terbolesa")     ~ "turbidity_ntu",
      TRUE                                  ~ NA_character_
    )
  ) |>
  filter(!is.na(var_en))

# ─────────────────────────────────────────────────────────────────────────────
# 3. Pivot to wide format: one row per (date, depth_m)
# ─────────────────────────────────────────────────────────────────────────────

profiles <- aca_vars |>
  select(date, depth_m, var_en, value) |>
  summarise(value = mean(value, na.rm = TRUE),
            .by   = c(date, depth_m, var_en)) |>
  pivot_wider(names_from = var_en, values_from = value) |>
  arrange(date, depth_m)

# ─────────────────────────────────────────────────────────────────────────────
# 4. Load ERA5 and compute 14-day rolling mean air temperature
# ─────────────────────────────────────────────────────────────────────────────

era5 <- read_csv("data/era5_banyoles.csv", show_col_types = FALSE) |>
  arrange(date) |>
  mutate(air_temp_14d = rollmeanr(air_temp_C, k = 14, fill = air_temp_C))

# ─────────────────────────────────────────────────────────────────────────────
# 5. Join ERA5, Julian day and summer auxiliaries
# ─────────────────────────────────────────────────────────────────────────────

aca <- profiles |>
  left_join(era5, by = "date") |>
  mutate(
    year           = year(date),
    month          = month(date),
    doy            = yday(date),
    sin_doy        = sin(2 * pi * doy / 365),
    cos_doy        = cos(2 * pi * doy / 365),
    stratified     = as.integer(month %in% 5:10),
    rel_depth      = depth_m / 50,
    depth_x_summer = rel_depth * stratified,
    surface_only   = (depth_m == 0),
    split          = if_else(year <= 2018, "train", "test")
  ) |>
  arrange(date, depth_m)

# ─────────────────────────────────────────────────────────────────────────────
# 6. Train / test subsets
# ─────────────────────────────────────────────────────────────────────────────

aca_train <- filter(aca, split == "train")
aca_test  <- filter(aca, split == "test")

# ─────────────────────────────────────────────────────────────────────────────
# 7. Summary
# ─────────────────────────────────────────────────────────────────────────────

campaign_summary <- aca |>
  group_by(split, date) |>
  summarise(
    n_depths  = n(),
    temp_surf = temp_C[which.min(depth_m)],
    DO_surf   = DO_mgl[which.min(depth_m)],
    DO_min    = min(DO_mgl, na.rm = TRUE),
    air_temp  = round(first(air_temp_14d), 1),
    .groups   = "drop"
  )

cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("  ACA Banyoles — campaign overview\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
print(campaign_summary, n = Inf)
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

cat("\nTrain: ", n_distinct(aca_train$date), "campaigns |",
    nrow(aca_train), "depth-observations |",
    sum(!is.na(aca_train$temp_C)), "temp |",
    sum(!is.na(aca_train$DO_mgl)), "DO\n")

cat("Test:  ", n_distinct(aca_test$date), "campaigns |",
    nrow(aca_test), "depth-observations |",
    sum(!is.na(aca_test$temp_C)), "temp |",
    sum(!is.na(aca_test$DO_mgl)), "DO\n")

message("\nData loaded. Objects: aca, aca_train, aca_test")
