# Pre-workshop setup

Please complete these steps **before arriving** at the workshop.  
A working R environment saves everyone time.  
Estimated setup time: **10 minutes**.

---

## 1. Software

| Tool | Download |
|------|---------|
| **R** | https://cran.r-project.org |
| **RStudio** | https://posit.co/download/rstudio-desktop/ |

---

## 2. Get the workshop materials

```bash
git clone https://github.com/danielmerbet/gleon2026_modelling_workshop.git
```

Or download the ZIP from GitHub → **Code → Download ZIP**, then unzip.

---

## 3. Install R packages

Run gleon-banyoles-workshop.Rproj or open Rstudio and run the codes

```r
source("00_packages.R")
```

This installs anything missing and then loads all packages. You should see:

```
All packages loaded successfully. R version: 4.x.x
```

The packages installed are:

| Package | Purpose |
|---------|---------|
| `tidyverse` | data wrangling and ggplot2 |
| `lubridate` | date arithmetic |
| `caret` | unified ML training interface |
| `randomForest` | random forest engine |
| `zoo` | rolling means for ERA5 forcing |
| `reshape2` | matrix reshaping |
| `viridis` | colour scales |
| `patchwork` | multi-panel figures |
| `scales`, `broom` | axis formatting, tidy model output |
| `httr2`, `jsonlite` | Open-Meteo ERA5 API download |
| `shiny` | interactive process-model app |

> **Note:** `caret` pulls in several ML dependencies (~20 packages total).
> If you hit a compilation error on Windows, install
> [Rtools](https://cran.r-project.org/bin/windows/Rtools/) first.

---

## 4. Verify the data files

The workshop data is included in the repository. Check that these two files exist:

```
data/data.csv          # ACA monitoring data 2007–2024
data/era5_banyoles.csv # ERA5 daily meteorological forcing
```
---


