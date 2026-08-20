# Is it safe to swim in Lake Banyoles during the GLEON meeting 2026?

### Hybrid Lake Modelling and forecasting

**GLEON Meeting · Banyoles · 31 August 2026**  
Facilitator: Daniel Mercado-Bettín (CEAB-CSIC)  
Duration: 2.5-3 hours | Language: R | Level: Intermediate (but anyone can follow)

```bash
git clone https://github.com/danielmerbet/gleon2026_modelling_workshop.git
```

See [SETUP.md](SETUP.md) for full installation instructions.

---

## Overview

Lake models are either physically grounded or data-driven. This workshop shows you how to combine both, and how to put the result to work. Using **Lake Banyoles** (karstic, NE Spain) as a case study, participants build a simple process-based model, train a random forest on its residuals, and assemble a hybrid prediction. The payoff: a live next-day swimmer safety forecast, driven by Google DeepMind's WeatherNext 2 ensemble (64 members, via Open-Meteo), so it reports a probability of safe conditions at each depth, that classifies temperature and dissolved oxygen conditions from surface to 50 m depth. Hands-on R throughout.

---

## Workshop structure

| Part | Script | Topic | Time |
|------|--------|-------|------|
| 0 | — | Introduction & story | 10 min |
| 1 | `01b_era5_download.R`, `01c_prepare_data_validate.R`, `01_data.R` | Data creation | Before the meeting |
| 2 | `02_explore.R` | Data exploration & swimmer risk thresholds | 30 min |
| 3 | `03_process_model.R` | 1D process-based model & residuals | 20 min |
| 4 | `04_ml_residuals.R` | Random forest on residuals (LOCO CV) | 20 min |
| 5 | `05_hybrid_compare.R` | Risk map, limitations & discussion | 20 min |
| 6 | `06_forecast_tomorrow.R` | Live next-day ensemble swimmer safety forecast | 20 min |
| 7 | `07_validate_validate.R` | Independent validation (2023–2026) | 10 min |

**25 output figures** are saved to `outputs/` as the scripts run (01–25).

---

## Repository structure

```
gleon-banyoles-workshop/
├── README.md
├── SETUP.md                    # participant pre-workshop instructions
├── 00_packages.R               # install and load all packages
├── 01_data.R                   # load ACA field data from data/data.csv
├── 01b_era5_download.R         # download ERA5 forcing from Open-Meteo
├── 01c_prepare_data_validate.R # consolidate independent temperature data to CSV
├── model.R                     # shared process model 
├── utils.R                     # shared helper functions
├── 02_explore.R                # Part 2 — data exploration & visualisation
├── 03_process_model.R          # Part 3 — process-based model & residuals
├── 04_ml_residuals.R           # Part 4 — random forest on residuals
├── 05_hybrid_compare.R         # Part 5 — risk map, comparison & limitations
├── 06_forecast_tomorrow.R      # Part 6 — live next-day ENSEMBLE swimmer safety forecast
├── 07_validate_independent.R   # Part 7 — independent validation 
├── app/
│   └── app.R     # interactive Shiny model-tuning app
├── data/
│   ├── data.csv                # ACA monitoring data (raw source)
│   ├── era5_banyoles.csv       # ERA5 daily meteorological forcing
│   ├── new_data.xlsx           # Validation temperature soundings (meteolestartit.cat, raw source)
│   └── validate_temperature.csv # tidy validation temperature soundings (generated)
├── outputs/                    # figures saved here (01–20)
├── slides/
│   ├── workshop_slides.pptx    # full 2.5-3h guided deck 
│   └── workshop_slides.pdf     # full 2.5-3h guided deck4
└── archive/                    # superseded scripts and data
```

---

## How to use

1. Read `SETUP.md` and install packages **before** the workshop.
2. Run gleon-banyoles-workshop.Rproj or open codes in RStudio
3. Run `00_packages.R` to verify your setup.


### Interactive playground
After running Parts 2–3 once (to train the ML models), launch the Shiny app:

```r
shiny::runApp("app")
```

It allows to drag sliders for the physical parameters (air-water coupling, solar warming, thermocline sharpness, groundwater temperature, oxygen decline) and watch the fit to a real campaign update live. 

---

## The story

Lake Banyoles (42.12°N, 2.76°E) looks perfectly safe from the shore. But every summer, as the surface warms to 28°C, the water column stratifies and biological decomposition strips the oxygen from the deep layer. Below ~15–20 m: near-zero dissolved oxygen and potential hydrogen sulphide. A freediver descending without knowing the profile is in serious danger.

The question this workshop answers: **can we predict the safe swimming depth from weather data alone, without sending a boat out?**

We build a hybrid model, process-based physics for the lake's thermal structure, random forest to correct what the physics gets wrong, and produce a depth-by-depth risk map for every ACA monitoring campaign.

---

## Data sources

**ACA field campaigns** (`data/data.csv`)  
Agència Catalana de l'Aigua — station 0450401, Lake Banyoles holomictic basin  
Variables: water temperature, dissolved oxygen, DO saturation, chlorophyll *a*, conductivity, pH, turbidity  
Depth range: 0–50 m | 2007–2026

**ERA5 meteorological forcing** (`data/era5_banyoles.csv`)  
Daily reanalysis  
Variables: air temperature, precipitation, wind speed, shortwave radiation | 2007–2026

**Banyoles temperature soundings** (`data/validate_temperature.csv`, generated by `01c_prepare_data_validate.R` from `data/new_data.xlsx`)  
Citizen-science monitoring shared by [meteolestartit.cat](https://meteolestartit.cat/) — many thanks to **Josep Pascual** for sharing this data with the workshop.  
Variables: water temperature at 4 sampling points (A, B, C, D)  
Depth range: 0–100 m | 62 campaigns, 2023–2026

---

## LLM use

- Claude Sonnet 5 was used to create this workshop, mainly to provide support in coding, formatting and plotting.

---

## Key references

- Willard, J., Jia, X., Xu, S., Steinbach, M., & Kumar, V. (2022). Integrating scientific knowledge with machine learning for engineering and environmental systems. ACM Computing Surveys, 55(4), 1-37.
- RRead, J. S., Jia, X., Willard, J., Appling, A. P., Zwart, J. A., Oliver, S. K., ... & Kumar, V. (2019). Process‐guided deep learning predictions of lake water temperature. Water Resources Research, 55(11), 9173-9190.
- Casamitjana, X., & Roget, E. (1993). Resuspension of sediment by focused groundwater in Lake Banyoles. Limnology and Oceanography, 38(3), 643-656.
