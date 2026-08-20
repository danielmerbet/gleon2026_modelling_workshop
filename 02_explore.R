# =============================================================================
# 02_explore.R
# PART 2 — Understand the data through the lens of swimmer safety (30 min)
#
# Goals:
#   • Load ACA field campaigns and ERA5 meteorological data
#   • Visualise the seasonal patterns that create risk: stratification, anoxia
#   • Introduce the risk thresholds before any modelling
#   • Build the intuition for WHY a hybrid model is needed
#
# Structure:
#   1.1  ACA campaign overview  — data structure and split (5 min)
#   1.2  ACA profiles           — the observed record (15 min)
#   1.3  ERA5 context           — what drives stratification? (5 min)
#   1.4  Risk threshold         — where is it dangerous to swim? (5 min)
# =============================================================================

source("00_packages.R")
source("utils.R")
source("01_data.R")

# ─────────────────────────────────────────────────────────────────────────────
# 1.1  ACA campaign overview — data structure and train/test split
# ─────────────────────────────────────────────────────────────────────────────

# Campaign timeline — show data structure before diving into values
p_timeline <- aca |>
  distinct(date, split) |>
  mutate(
    campaign = paste0(month.abb[month(date)], " ", year(date)),
    season   = case_when(
      month(date) %in% c(12, 1, 2) ~ "Winter",
      month(date) %in% 3:5         ~ "Spring",
      month(date) %in% 6:8         ~ "Summer",
      TRUE                         ~ "Autumn"
    ),
    season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn"))
  ) |>
  ggplot(aes(x = date, y = split, colour = season, shape = split)) +
  geom_vline(aes(xintercept = date), alpha = 0.15, colour = "grey60") +
  geom_point(size = 4) +
  scale_colour_manual(values = c("Winter" = "#4f94cd", "Spring" = "#2d9e2d",
                                  "Summer" = "#e07b30", "Autumn" = "#a0522d")) +
  scale_shape_manual(values = c("train" = 16, "test" = 17),
                     labels = c("train" = "Train (2007–2022)",
                                "test"  = "Test  (2024)")) +
  scale_x_date("Campaign date", date_breaks = "2 years", date_labels = "%Y") +
  scale_y_discrete("Split", labels = c("train" = "Training",
                                        "test"  = "Test")) +
  labs(title = "ACA monitoring campaigns — Lake Banyoles 2007–2024",
       subtitle = "Training: 8 campaigns (2007–2022) | Test: 1 campaign (2024)",
       colour = "Season", shape = "Split",
       caption = "Station 0450401 | Agència Catalana de l'Aigua") +
  theme_workshop()

print(p_timeline)
png("outputs/01_campaign_timeline.png", width = 20, height = 10, 
    units="cm", res=300)
p_timeline
dev.off()

# DISCUSSION:
# • All campaigns are summer — high air temp, strong stratification expected.
# • The test campaign (2024) was collected AFTER training (honest split).

# ─────────────────────────────────────────────────────────────────────────────
# 1.2  ACA field campaigns — the observed record (2007–2024)
# ─────────────────────────────────────────────────────────────────────────────

train_profiles <- aca_train |> filter(!surface_only, !is.na(temp_C))

# Temperature profiles — one line per campaign
p_temp_profiles <- train_profiles |>
  mutate(campaign  = paste0(month.abb[month], " ", year),
         surface_T = temp_C[depth_m == min(depth_m)][1],
         .by       = date) |>
  ggplot(aes(x = temp_C, y = depth_m,
             group = date, colour = surface_T)) +
  geom_path(linewidth = 1, alpha = 0.85) +
  geom_point(size = 1.5, alpha = 0.7) +
  scale_y_reverse("Depth (m)", breaks = seq(0, 50, 10)) +
  scale_x_continuous("Water temperature (°C)") +
  scale_colour_viridis_c("Surface\ntemp (°C)", option = "magma") +
  labs(title = "Temperature depth profiles — ACA training campaigns",
       subtitle = "All 8 depth-resolved campaigns | coloured by surface temperature",
       caption = "Station 0450401 | ACA 2007–2022") +
  theme_workshop()

print(p_temp_profiles)
png("outputs/02_temp_profiles_ACA.png", width = 20, height = 10, 
    units="cm", res=300)
p_temp_profiles
dev.off()

# DISCUSSION:
# • Where is the thermocline? (inflection point, ~5–15 m depending on campaign)
# • How deep does the warm surface layer extend?
# • How does the profile shape change between campaigns?

# DO profiles — with swimmer risk thresholds
p_do_profiles <- aca_train |>
  filter(!surface_only, !is.na(DO_mgl)) |>
  mutate(campaign = paste0(month.abb[month], " ", year)) |>
  ggplot(aes(x = DO_mgl, y = depth_m, group = date, colour = campaign)) +
  geom_vline(xintercept = c(2, 4, 6), linetype = "dashed",
             colour = c("#c0392b", "#e07b30", "#f0c220"), linewidth = 0.8) +
  annotate("text", x = c(2.2, 4.2, 6.2), y = 49,
           label = c("Anoxic\n<2", "Hypoxic\n<4", "Caution\n<6"),
           hjust = 0, size = 2.8,
           colour = c("#c0392b", "#e07b30", "#9a7d0a")) +
  geom_path(linewidth = 1, alpha = 0.8) +
  geom_point(size = 1.5, alpha = 0.7) +
  scale_y_reverse("Depth (m)", breaks = seq(0, 50, 10)) +
  scale_x_continuous("Dissolved oxygen (mg/L)", limits = c(0, 14)) +
  scale_colour_viridis_d(option = "turbo", name = "Campaign") +
  labs(title = "DO depth profiles — ACA training campaigns",
       subtitle = "Dashed lines: EU Bathing Water Directive thresholds (mg/L)",
       caption = "Station 0450401 | ACA 2007–2022") +
  theme_workshop()

print(p_do_profiles)
png("outputs/03_DO_profiles_ACA.png", width = 20, height = 10, 
    units="cm", res=300)
p_do_profiles
dev.off()

# DISCUSSION:
# • All summer campaigns show near-zero DO below ~20–30 m.
# • The safe surface layer is typically only 10–20 m thick.
# • The May 2007 campaign is the exception — why? (spring mixing not yet complete)
#
# WHY IS ANOXIA DANGEROUS FOR SWIMMERS?
# Two mechanisms:
#
# 1. Hydrogen sulphide (H₂S)
#    When O2 is gone, sulphate-reducing bacteria take over and produce H₂S.
#    It is toxic at very low concentrations and causes rapid loss of
#    consciousness. A diver surfacing through an anoxic layer can inhale it.
#    In a stratified karstic lake this layer can sit at surprisingly shallow
#    depth during a hot, calm summer.
#
# 2. Cold shock
#    The anoxic hypolimnion is also cold (~11–13°C in Banyoles). Descending
#    below the thermocline triggers involuntary gasping, hyperventilation and
#    cardiac stress, even in fit adults. The transition from the warm surface
#    (~25°C in August) can happen over just 2–3 m.
#
# The combination is what makes it insidious: the lake looks fine from the
# surface, the anoxic/H₂S layer is invisible, and the temperature drop is
# abrupt. A freediver or spearfisherman descending without knowing the
# profile is the real risk scenario.

# Observed risk heatmap from ACA data (the "before modelling" version of Part 4)
p_obs_risk <- aca_train |>
  filter(!surface_only, !is.na(DO_mgl)) |>
  mutate(
    campaign = paste0(month.abb[month], "\n", year),
    risk_DO  = case_when(
      DO_mgl < 2 ~ "anoxic",
      DO_mgl < 4 ~ "hypoxic",
      DO_mgl < 6 ~ "caution",
      TRUE       ~ "safe"
    ),
    risk_DO = factor(risk_DO, levels = c("safe", "caution", "hypoxic", "anoxic"))
  ) |>
  ggplot(aes(x = reorder(campaign, date), y = depth_m, fill = risk_DO)) +
  geom_tile(width = 0.9) +
  scale_y_reverse("Depth (m)", breaks = seq(0, 50, 10)) +
  scale_x_discrete(NULL) +
  scale_fill_manual("Risk (DO)",
                    values = c("safe"    = "#2d9e2d", "caution" = "#f0c220",
                               "hypoxic" = "#e07b30", "anoxic"  = "#c0392b")) +
  labs(title = "Observed risk map — ACA training campaigns",
       subtitle = "Based on measured DO only | compare with the hybrid model map in Part 4",
       caption = "Station 0450401 | ACA 2007–2022") +
  theme_workshop() +
  theme(legend.position = "bottom")

print(p_obs_risk)
png("outputs/04_observed_risk_map.png", width = 20, height = 10, 
    units="cm", res=300)
p_obs_risk
dev.off()

# ─────────────────────────────────────────────────────────────────────────────
# 1.3  ERA5 meteorological context — what drives stratification?
# ─────────────────────────────────────────────────────────────────────────────

era5_full <- read_csv("data/era5_banyoles.csv", show_col_types = FALSE)

campaign_met <- era5_full |>
  mutate(
    campaign_split = case_when(
      date %in% aca_train$date ~ "train",
      date %in% aca_test$date  ~ "test",
      TRUE                     ~ "background"
    )
  )

p_era5 <- campaign_met |>
  ggplot(aes(x = date)) +
  geom_line(data = ~ filter(., campaign_split == "background"),
            aes(y = air_temp_C), colour = "grey80", linewidth = 0.4) +
  geom_point(data = ~ filter(., campaign_split != "background"),
             aes(y = air_temp_C, colour = campaign_split), size = 3) +
  geom_point(data = ~ filter(., campaign_split != "background"),
             aes(y = wind_ms * 2, colour = campaign_split),
             shape = 17, size = 2.5, alpha = 0.6) +
  scale_y_continuous(
    "Air temperature (°C)",
    sec.axis = sec_axis(~ . / 2, name = "Wind speed (m/s)")
  ) +
  scale_colour_manual(values = c("train" = "#e07b30", "test" = "#4f94cd"),
                      labels = c("train" = "Training campaigns (2007–2022)",
                                 "test"  = "Test campaign (2024)"),
                      name = NULL) +
  scale_x_date("Year", date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "ERA5 conditions on ACA campaign dates — Lake Banyoles",
       subtitle = "Circles = air temperature | Triangles = wind speed (right axis)",
       caption = "ERA5 reanalysis via Open-Meteo | 42.12°N, 2.76°E") +
  theme_workshop() +
  theme(legend.position = "top")

print(p_era5)
png("outputs/05_era5_campaign_context.png", width = 20, height = 10, 
    units="cm", res=300)
p_era5
dev.off()

# DISCUSSION:
# • All campaigns are summer: high air temp, strong stratification.
# • Wind is the driver of mixed-layer depth: low wind = deep stratification.

# ─────────────────────────────────────────────────────────────────────────────
# 1.4  The swimmer risk lens
#      Setting up the question that Parts 2–4 will answer.
# ─────────────────────────────────────────────────────────────────────────────

p_risk_scatter <- aca_train |>
  filter(!surface_only, !is.na(DO_mgl)) |>
  mutate(
    campaign = paste0(month.abb[month], " ", year),
    risk = case_when(
      DO_mgl < 2 ~ "Anoxic (<2 mg/L)",
      DO_mgl < 4 ~ "Hypoxic (2–4 mg/L)",
      DO_mgl < 6 ~ "Caution (4–6 mg/L)",
      TRUE       ~ "Safe (≥6 mg/L)"
    ),
    risk = factor(risk, levels = c("Safe (≥6 mg/L)", "Caution (4–6 mg/L)",
                                   "Hypoxic (2–4 mg/L)", "Anoxic (<2 mg/L)"))
  ) |>
  ggplot(aes(x = DO_mgl, y = depth_m, colour = risk)) +
  geom_hline(yintercept = c(5, 10, 15, 20), alpha = 0.15) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_vline(xintercept = c(2, 4, 6), linetype = "dashed", linewidth = 0.7,
             colour = c("#c0392b", "#e07b30", "#f0c220")) +
  scale_y_reverse("Depth (m)", breaks = seq(0, 50, 10)) +
  scale_x_continuous("Dissolved oxygen (mg/L)", limits = c(0, 15)) +
  scale_colour_manual(values = c("Safe (≥6 mg/L)"      = "#2d9e2d",
                                  "Caution (4–6 mg/L)"  = "#f0c220",
                                  "Hypoxic (2–4 mg/L)"  = "#e07b30",
                                  "Anoxic (<2 mg/L)"    = "#c0392b"),
                      name = "Risk level") +
  labs(title = "Swimmer risk by depth — all ACA training campaigns",
       subtitle = "The safe zone is the top 10–20 m | Below that: anoxic in summer",
       caption = "Station 0450401 | ACA 2007–2022") +
  theme_workshop()

print(p_risk_scatter)
png("outputs/06_risk_scatter.png", width = 20, height = 10, 
    units="cm", res=300)
p_risk_scatter
dev.off()


cat("\n--- Part 1 summary: risk at depth ---\n")
aca_train |>
  filter(!surface_only, !is.na(DO_mgl)) |>
  mutate(risk = case_when(
    DO_mgl < 2 ~ "anoxic",
    DO_mgl < 4 ~ "hypoxic",
    DO_mgl < 6 ~ "caution",
    TRUE       ~ "safe"
  )) |>
  group_by(risk) |>
  summarise(
    n_obs      = n(),
    pct        = round(100 * n() / nrow(filter(aca_train, !surface_only, !is.na(DO_mgl))), 1),
    depth_mean = round(mean(depth_m), 1),
    depth_min  = min(depth_m),
    depth_max  = max(depth_m),
    .groups    = "drop"
  ) |>
  arrange(desc(depth_mean)) |>
  print()

message("\n--- Part 1 complete ---")
message("Key observations:")
message("  • Training data: 8 summer depth profiles (2007–2022)")
message("  • Test data: 1 campaign (2024)")
message("  • Summer anoxia starts at ~15–20 m; spreads to >30 m by Aug")
message("  • ERA5 forcing available for all 9 campaign dates")
message("  • The risk thresholds (2/4/6 mg/L) frame everything that follows")
