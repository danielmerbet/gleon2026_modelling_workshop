# =============================================================================
# 05_hybrid_compare.R
# PART 5 — Swimmer risk assessment & honest limitations (20 min)
#
# Goals:
#   • Define a risk index from hybrid DO and temperature predictions
#   • Build the seasonal risk map for Banyoles (2021–2024 test period)
#   • Estimate safe swimming depth per test campaign date
#   • Compare process-only vs ML-only vs hybrid on training data
#   • Honest limitations: surface DO failure, data sparsity, extrapolation
# =============================================================================

source("00_packages.R")
source("utils.R")
source("01_data.R")

library(randomForest)
library(caret)

aca_modelled <- readRDS("data/aca_modelled_train.rds") #required to run 04_ml_residuals
ml           <- readRDS("data/ml_models.rds") #required to run 04_ml_residuals

risk_profiles  <- ml$risk_profiles
test_surface   <- ml$test_surface
hybrid_train   <- ml$hybrid_train
rf_do          <- ml$rf_do
rf_temp        <- ml$rf_temp
FEATURES       <- ml$features

# ─────────────────────────────────────────────────────────────────────────────
# 4.1  Risk index
#
# Three-level risk score per (date, depth) cell:
#   DO threshold:   < 4 mg/L = hypoxic (physiology + EU Bathing Water Directive)
#                   < 2 mg/L = anoxic  (extreme; H₂S possible)
#   Temp threshold: < 15°C = cold-shock risk for recreational swimmers
#
# Composite: worst of DO and temperature risk at each depth.
# ─────────────────────────────────────────────────────────────────────────────

risk <- risk_profiles |>
  mutate(
    risk_DO = case_when(
      DO_hybrid < 2  ~ "anoxic",
      DO_hybrid < 4  ~ "hypoxic",
      DO_hybrid < 6  ~ "caution",
      TRUE           ~ "safe"
    ),
    risk_temp = case_when(
      T_hybrid < 15 ~ "cold",
      TRUE          ~ "ok"
    ),
    # Composite: flag if either threshold breached
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

# ─────────────────────────────────────────────────────────────────────────────
# 4.2  Seasonal risk map — the centrepiece visual
#
# Depth × campaign date heatmap coloured by risk level.
# ─────────────────────────────────────────────────────────────────────────────

risk_colours <- c(
  "safe"    = "#2d9e2d",
  "caution" = "#f0c220",
  "cold"    = "#4f94cd",
  "hypoxic" = "#e07b30",
  "anoxic"  = "#c0392b"
)

p_risk_map <- risk |>
  mutate(campaign = paste0(month.abb[month(date)], "\n", year(date))) |>
  ggplot(aes(x = reorder(campaign, date), y = depth_m, fill = risk_composite)) +
  geom_tile() +
  scale_y_reverse("Depth (m)", breaks = seq(0, 50, 10)) +
  scale_x_discrete(NULL) +
  scale_fill_manual("Risk", values = risk_colours,
                    labels = c("safe"    = "Safe (DO ≥ 6, T ≥ 15°C)",
                               "caution" = "Caution (DO 4–6 mg/L)",
                               "cold"    = "Cold (T < 15°C)",
                               "hypoxic" = "Hypoxic (DO 2–4 mg/L)",
                               "anoxic"  = "Anoxic (DO < 2 mg/L)")) +
  labs(title = "Swimmer risk map — Lake Banyoles 2021–2024",
       subtitle = "Hybrid model prediction (process + random forest) | Station 0450401",
       caption = "DO thresholds: EU Bathing Water Directive 76/160/EEC") +
  theme_workshop() +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.2, "cm"))

print(p_risk_map)
png("outputs/18_risk_map.png", width = 20, height = 10, 
    units="cm", res=300)
p_risk_map
dev.off()

# ─────────────────────────────────────────────────────────────────────────────
# 4.3  Safe swimming depth per campaign
# ─────────────────────────────────────────────────────────────────────────────

safe_depth <- risk |>
  group_by(date) |>
  summarise(
    # deepest depth still fully safe (DO ≥ 6 AND T ≥ 15°C)
    safe_depth_m   = {
      safe_rows <- depth_m[risk_composite == "safe"]
      if (length(safe_rows) > 0) max(safe_rows) else 0
    },
    surface_T_pred = T_hybrid[depth_m == 0],
    surface_DO_pred = DO_hybrid[depth_m == 0],
    month          = first(month),
    .groups        = "drop"
  ) |>
  left_join(test_surface |> select(date, temp_C_obs, DO_obs),
            by = "date") |>
  mutate(
    campaign = paste0(month.abb[month], " ", year(date)),
    season   = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% 3:5         ~ "Spring",
      month %in% 6:8         ~ "Summer",
      TRUE                   ~ "Autumn"
    ),
    season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn"))
  )

cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("  Swimmer risk summary — ACA test campaigns (2021–2024)\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
safe_depth |>
  select(campaign, season, surface_T_pred, temp_C_obs,
         surface_DO_pred, DO_obs, safe_depth_m) |>
  print(n = Inf)
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

season_colours <- c("Winter" = "#4f94cd", "Spring" = "#2d9e2d",
                    "Summer" = "#e07b30", "Autumn" = "#a0522d")

p_safe_depth <- safe_depth |>
  ggplot(aes(x = reorder(campaign, date), y = safe_depth_m, fill = season)) +
  geom_col(width = 0.7, alpha = 0.85) +
  geom_text(aes(label = paste0(safe_depth_m, " m")),
            vjust = -0.4, size = 3.5, fontface = "bold") +
  scale_y_reverse("Safe swimming depth (m)", limits = c(55, 0)) +
  scale_x_discrete(NULL) +
  scale_fill_manual("Season", values = season_colours) +
  labs(title = "Safe swimming depth — Lake Banyoles 2021–2024",
       subtitle = "Deepest depth with DO ≥ 6 mg/L and T ≥ 15°C (hybrid model)",
       caption = "Station 0450401 | Holomictic basin") +
  theme_workshop() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

print(p_safe_depth)
png("outputs/19_safe_swimming_depth.png", width = 20, height = 10, 
    units="cm", res=300)
p_safe_depth
dev.off()

# ─────────────────────────────────────────────────────────────────────────────
# 4.4  Three-way comparison on training data
#      Process-only vs ML-only (RF on raw DO) vs Hybrid
# ─────────────────────────────────────────────────────────────────────────────

# ML-only baseline: RF predicting raw DO (no process model)
features_mlonly <- c("sin_doy", "cos_doy", "stratified",
                     "rel_depth", "depth_x_summer",
                     "air_temp_14d", "wind_ms", "radiation_MJ_m2")

df_mlonly <- aca_modelled |>
  select(date, depth_m, all_of(features_mlonly), DO_mgl) |>
  drop_na()

set.seed(42)
cv_mlonly <- groupKFold(df_mlonly$date, k = n_distinct(df_mlonly$date))

message("Training ML-only DO baseline...")
rf_mlonly <- train(
  x         = df_mlonly[, features_mlonly],
  y         = df_mlonly$DO_mgl,
  method    = "rf",
  trControl = trainControl(method = "cv", index = cv_mlonly,
                           savePredictions = TRUE),
  ntree     = 300,
  tuneGrid  = data.frame(mtry = c(2, 3))
)

comparison <- hybrid_train |>
  filter(!is.na(DO_mgl)) |>
  mutate(
    DO_mlonly = predict(rf_mlonly,
                        newdata = pick(all_of(features_mlonly)))
  ) |>
  drop_na(DO_mgl, DO_pred, DO_hybrid, DO_mlonly)

metrics_all <- bind_rows(
  eval_metrics(comparison$DO_mgl, comparison$DO_pred,   "Process-only"),
  eval_metrics(comparison$DO_mgl, comparison$DO_mlonly, "ML-only (RF)"),
  eval_metrics(comparison$DO_mgl, comparison$DO_hybrid, "Hybrid")
)

cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("  DO prediction — training data comparison\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
print(metrics_all)
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("(In-sample; LOCO CV RMSE for hybrid DO is",
    round(min(rf_do$results$RMSE), 2), "mg/L)\n\n")

p_compare <- metrics_all |>
  pivot_longer(c(RMSE, R2), names_to = "metric", values_to = "value") |>
  mutate(model = factor(model,
                        levels = c("Process-only", "ML-only (RF)", "Hybrid"))) |>
  ggplot(aes(x = model, y = value, fill = model)) +
  geom_col(width = 0.6, alpha = 0.85) +
  geom_text(aes(label = round(value, 2)), vjust = -0.4,
            size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("steelblue", "darkorange", "darkgreen"),
                    guide = "none") +
  scale_x_discrete(NULL) +
  facet_wrap(~metric, scales = "free_y",
             labeller = labeller(metric = c(RMSE = "RMSE (mg/L) — lower is better",
                                             R2   = "R²  — higher is better"))) +
  labs(title = "DO prediction: three strategies (training data)",
       subtitle = "ACA 2007–2018 | in-sample; LOCO CV corrects for overfitting") +
  theme_workshop() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

print(p_compare)
png("outputs/20_model_comparison.png", width = 20, height = 10, 
    units="cm", res=300)
p_compare
dev.off()


# ─────────────────────────────────────────────────────────────────────────────
# 4.5  Honest limitations
# ─────────────────────────────────────────────────────────────────────────────

cat("\n--- LIMITATION 1: overfitting (training vs LOCO CV) ---\n")
DO_insample <- pmax(0, comparison$DO_pred +
                      predict(rf_do, newdata = comparison[, FEATURES]))
cat("In-sample RMSE:  ", round(rmse(comparison$DO_mgl, DO_insample), 2), "mg/L\n")
cat("LOCO CV RMSE:    ", round(min(rf_do$results$RMSE), 2), "mg/L\n")
cat("Gap: the model memorises campaign-specific patterns it won't see again.\n\n")

cat("--- LIMITATION 2: surface DO — biological processes missing ---\n")
cat("Surface temp validation RMSE : ", test_surface |>
      filter(!is.na(temp_C_obs)) |>
      summarise(r = round(rmse(temp_C_obs, T_hybrid), 2)) |> pull(r), "°C\n")
cat("Surface DO   validation RMSE : ", test_surface |>
      filter(!is.na(DO_obs)) |>
      summarise(r = round(rmse(DO_obs, DO_hybrid), 2)) |> pull(r), "mg/L\n")
cat("The DO model was trained on stratification-driven depth gradients.\n")
cat("It cannot predict spring blooms (supersaturation) or autumn turnover\n")
cat("(de-oxygenation during mixing). More data + a biological module needed.\n\n")

cat("--- LIMITATION 3: depth extrapolation beyond training range ---\n")
cat("Training campaigns: mostly 0–50 m, 9 summer campaigns.\n")
cat("Test campaigns: surface-only, all seasons.\n")
cat("The hybrid DO profiles at 30–50 m in winter/spring are extrapolations\n")
cat("with no validation data. Treat with caution.\n\n")

# Remaining error on training data
p_remaining <- comparison |>
  mutate(
    hybrid_residual = DO_mgl - DO_hybrid,
    campaign = format(date, "%b %Y")
  ) |>
  ggplot(aes(x = hybrid_residual, y = depth_m, colour = campaign)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_path(aes(group = campaign), alpha = 0.4) +
  geom_point(size = 2, alpha = 0.8) +
  scale_y_reverse("Depth (m)") +
  scale_x_continuous("Remaining error: observed − hybrid (mg/L)") +
  scale_colour_viridis_d(option = "turbo", name = "Campaign") +
  labs(title = "Remaining errors after hybrid correction",
       subtitle = "What the model still gets wrong — where to invest next") +
  theme_workshop()

print(p_remaining)
png("outputs/21_remaining_errors.png", width = 20, height = 10, 
    units="cm", res=300)
p_remaining
dev.off()

# ─────────────────────────────────────────────────────────────────────────────
# 4.7  Discussion prompts
# ─────────────────────────────────────────────────────────────────────────────

cat("\n--- DISCUSSION PROMPTS ---\n")
cat("1. We validated on surface measurements only. How would you design\n")
cat("   a monitoring campaign to validate the deep-water predictions?\n\n")
cat("2. The safe swimming zone in summer is the top 10–20 m. The EU\n")
cat("   Bathing Water Directive focuses on surface samples. Is that enough?\n\n")
cat("3. Would you use this model to issue a public swimming advisory?\n")
cat("   What would you need to add before you felt confident doing so?\n\n")
cat("4. What process model would better represent your own study lake?\n")
cat("   (GLM? GOTM? FLake? a 0D box model?)\n")

message("\n--- All outputs saved to outputs/ ---")
