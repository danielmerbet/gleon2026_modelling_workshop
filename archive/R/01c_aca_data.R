# =============================================================================
# 01c_aca_data.R
# Load, clean, and prepare the ACA monitoring data for modelling.
#
# Source: Agència Catalana de l'Aigua (ACA) — station 0450401
#         Lake Banyoles, holomictic basin
# Period: 2007–2026 | 22 field campaigns
# Variables: water temperature (°C), dissolved oxygen (mg/L), Secchi depth (m)
# Depth range: 1–50 m
#
# Joins: ERA5 daily meteorological forcing (data/era5_banyoles.csv)
#        — 14-day rolling mean air temp as the primary process-model driver
#
# Outputs (in global env):
#   aca       — full cleaned dataset with ERA5 forcing and features
#   aca_train — 2007–2024 (10 campaigns, mostly summer)  → model fitting
#   aca_test  — 2025–2026 (12 campaigns, all seasons)    → validation + risk
# =============================================================================

library(tidyverse)
library(readxl)
library(lubridate)

# ─────────────────────────────────────────────────────────────────────────────
# 1. Load and clean ACA profiles
# ─────────────────────────────────────────────────────────────────────────────

aca_raw <- read_excel(
  "data/ACA-Consulta_de_dades_del_medi.xlsx",
  sheet     = 1,
  skip      = 6,
  col_names = FALSE
)
names(aca_raw) <- c("date", "station_code", "station_name", "mass_name",
                    "utm_x", "utm_y", "variable", "depth_raw",
                    "value", "units", "method", "comments")

# Remove header artefacts, parse types
aca_clean <- aca_raw |>
  filter(
    !is.na(date),
    !date %in% c("Data", "Data inici", "Data fi"),
    !variable %in% c("Variable", NA)
  ) |>
  mutate(
    date  = as.Date(date),
    value = suppressWarnings(as.numeric(value))
  )

# ─────────────────────────────────────────────────────────────────────────────
# 2. Secchi depth — one value per campaign (depth recorded as "-")
# ─────────────────────────────────────────────────────────────────────────────

secchi <- aca_clean |>
  filter(variable == "Disc de Secchi") |>
  select(date, secchi_m = value)

# ─────────────────────────────────────────────────────────────────────────────
# 3. Depth profiles — temperature and DO
# ─────────────────────────────────────────────────────────────────────────────

profiles <- aca_clean |>
  filter(variable != "Disc de Secchi") |>
  mutate(
    # "-" means surface measurement (no depth probe deployed); assign 0 m.
    # "001"–"050" are depth-resolved profile observations.
    depth_m  = if_else(depth_raw == "-", 0L, suppressWarnings(as.integer(depth_raw))),
    variable = case_when(
      str_detect(variable, "Temperatura") ~ "temp_C",
      str_detect(variable, "Oxigen")      ~ "DO_mgl"
    )
  ) |>
  filter(!is.na(variable), !is.na(depth_m)) |>
  select(date, depth_m, variable, value) |>
  pivot_wider(names_from = variable, values_from = value)

# ─────────────────────────────────────────────────────────────────────────────
# 4. Load ERA5 and compute 14-day rolling mean air temperature
#    (lake temperature responds to atmospheric forcing over days, not hours)
# ─────────────────────────────────────────────────────────────────────────────

era5 <- read_csv("data/era5_banyoles.csv", show_col_types = FALSE) |>
  arrange(date) |>
  mutate(
    air_temp_14d = zoo::rollmeanr(air_temp_C, k = 14, fill = air_temp_C)
  )

# ─────────────────────────────────────────────────────────────────────────────
# 5. Join profiles + Secchi + ERA5 and engineer features
# ─────────────────────────────────────────────────────────────────────────────

max_depth_obs <- max(profiles$depth_m, na.rm = TRUE)

aca <- profiles |>
  left_join(secchi, by = "date") |>
  left_join(era5,   by = "date") |>
  mutate(
    year       = year(date),
    month      = month(date),
    doy        = yday(date),
    # cyclical encoding — day of year
    sin_doy    = sin(2 * pi * doy / 365),
    cos_doy    = cos(2 * pi * doy / 365),
    # stratification season: May–October
    stratified = as.integer(month %in% 5:10),
    # relative depth: 0 = surface, 1 = deepest observed (50 m)
    rel_depth      = depth_m / max_depth_obs,
    depth_x_summer = rel_depth * stratified,
    # surface_only: campaigns where no depth probe was deployed ("-" depth)
    # 2017 (1 row) and all 2025-2026 campaigns — surface measurements only
    # These are the most relevant for swimmer safety (bathing water quality)
    surface_only = (depth_m == 0),
    # train / test split
    split = if_else(year <= 2024, "train", "test")
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

cat("Test:  ", n_distinct(aca_test$date),  "campaigns |",
    nrow(aca_test),  "depth-observations |",
    sum(!is.na(aca_test$temp_C)),  "temp |",
    sum(!is.na(aca_test$DO_mgl)),  "DO\n")

message("\nACA data ready. Objects: aca, aca_train, aca_test")
