# Kelp BRT vs GAM

Comparison of boosted regression trees (BRTs) and generalized additive models (GAMs) for predicting giant kelp (*Macrocystis pyrifera*) density at the northern Channel Islands. Both models are projected through 2100 using ROMS-downscaled ocean conditions from three Earth System Models (GFDL, HadGEM, IPSL).

The repo also holds the ensemble step that prepares BRT kelp projections for the Kelp Forecasting Shiny app.

**Contact:** Benjamin Bray — bebray01@gmail.com
**Abstract:** `BRTvsGAM_Abstract.docx`

---

## Study design

- **Response:** `macpyr_log`, log giant kelp density from subtidal monitoring surveys.
- **Predictors:** depth, urchin density (`urchin_log`), cross-shore and alongshore wind stress (upwelling season), surface temperature (upwelling season), and bottom nitrate (summer).
- **Study area:** northern Channel Islands. The GAM is fit on a box of lat 33.8–34.1, lon −120.5 to −119.5. Projections for both models are made on lat 33.9–34.08, lon −120 to −119.3.
- **Train/test split:** temporal. Survey years ≤ 2015 are used for training, and years after 2015 for testing (GAM).
- **Projections:** 300 m resolution, historical (1995–2020) plus future runs under three ESMs. Urchin density in the projections comes from separate urchin BRT projections.

### Models

| | GAM | BRT |
|---|---|---|
| Package | `mgcv` | `tidymodels` / `xgboost` |
| Family | Tweedie (`tw`), REML | Poisson |
| Smooths / tuning | cubic regression splines, `k = 4` per predictor | Tuned and trained outside this repo |
| Fit in | `03262026_GAMModel.R` | Not in repo; the trained model is `rds/giant_se_brt_deploy_poisson.rds` |

---

## Pipeline

```
Training data (csv/gams_training/)
        │
        ├── 03262026_GAMModel.R ───────────► rds/giant_se_gam.rds
        │                                   rds/giant_se_brt_deploy_poisson.rds  (trained elsewhere)
        ▼
Urchin-projected environmental grids (csv/brt_predictions/*_for_kelp_projections_300res_poisson.rds)
        │
        ├── 03262026_BRTPredictions.R ──► csv/brt_predictions/giant_se_*_predictions_300res_ci.csv
        └── 03272026_GAMPredictions.R ──► csv/gam_predictions/giant_se_*_predictions_300res_ci.csv
        ▼
Evaluation, comparison, and figures (scripts below) ──► figures/
```

Run the scripts in roughly date order. Each script reads from and writes to fixed paths relative to the repo root.

---

## Scripts

### Modeling and prediction

| Script | What it does |
|---|---|
| `03262026_GAMModel.R` | Fits the giant kelp GAM, evaluates it on the 2016+ test set (RMSE, R²), and computes drop-one variable importance. Saves `rds/giant_se_gam.rds`. |
| `03262026_BRTPredictions.R` | Applies the BRT to historical and future grids. Writes point predictions, annual summaries by ESM, and time series figures. |
| `03272026_GAMPredictions.R` | Does the same for the GAM. |

### Model performance

| Script | What it does |
|---|---|
| `03272026_BRTPerformance.R` | BRT predictions vs observed density, by year (bar chart). |
| `03272026_GAMPerformance.R` | GAM predictions vs observed density, by year (bar chart). |
| `03272026_1to1.R` | BRT vs GAM historical predictions, 1:1 plot. |

### BRT vs GAM comparison

| Script | What it does |
|---|---|
| `03272026_BRTvsGAMAnnual.R` | Annual mean predictions from each model side by side. |
| `04012026_PDP.R` | Partial dependence plots, BRT vs GAM (faceted and overlay). |
| `04122026_BRT_PDP.R` | BRT-only partial dependence plots. |
| `04212026_SummaryStats.R` | Per-location means and coefficients of variation, Shapiro–Wilk, Wilcoxon, paired t-tests, and effect sizes (Cohen's d, Wilcoxon r). |
| `04232026_TrendComparison.R` | Ensemble time series for each model. |
| `04252026_SpatialDifference.R` | Map of where BRT > GAM and vice versa. Uses the `gam` and `brt` objects from `04212026_SummaryStats.R`, so run that first in the same session. |

### Maps and data figures

| Script | What it does |
|---|---|
| `03272026_MappingBRTOutput.R` / `03272026_MappingGAMOutput.R` | Maps of projected density, overall and by period (2001–2025, 2026–2050, 2051–2075, …). Also writes `spatial_extent.png`. |
| `03272026_HotspotAnalysis.R` | Compares GAM hotspots across periods. |
| `04232026_TrainingHistogram.R` | Distribution of the training data. |
| `04252026_CaliforniaMap.R` | Map of survey sites. |

### Shiny app preparation

| Script | What it does |
|---|---|
| `03282027_Ensemble_5_10.R` | Averages BRT kelp projections across the three ESMs and bins them into 5-year steps. Writes the `*_ensemble_5yr.csv` files the Kelp Forecasting app uses. Reads and writes `brt_projections_by_rock/`, which isn't in this repo. |
| `03242026_paraquetconversion.R` | Converts the projection `.rds` files in `brt_projections_by_rock/` to Parquet. |

---

## Repository structure

```
.
├── *.R                          # analysis scripts (see above)
├── BRTvsGAM_Abstract.docx
├── csv/
│   ├── gams_training/           # survey + ROMS training data (macpyr = giant kelp, nerlue = bull kelp, strpur = purple urchin)
│   ├── brt_predictions/         # BRT inputs and outputs
│   └── gam_predictions/         # GAM outputs
├── rds/                         # fitted models
├── figures/                     # output figures (brt/, gam/, and comparison plots)
├── shp/                         # California coastline and MPA shapefiles
├── renv.lock                    # package versions
└── Dockerfile                   # old Hugging Face deployment (see Known issues)
```

The main training file for both models is `csv/gams_training/macpyr_gams_training_historical_roms_0-30_5year.csv`. Most other files in `csv/gams_training/` are earlier versions or other species.

`rds/` also holds BRT models for bull kelp, purple urchin, and giant kelp stipes in other regions (`csw`, `northcentral`, `south`). This comparison doesn't use them, but the urchin models produce the urchin inputs for the projections.

---

## Data not in the repo

These files are gitignored for size and are needed to rerun predictions and comparisons:

- `csv/brt_predictions/future_for_kelp_projections_300res_poisson.rds`
- `csv/brt_predictions/giant_se_future_predictions_300res_ci.csv`
- `csv/brt_predictions/giant_se_historical_predictions_300res_ci.csv`
- `csv/brt_predictions/giant_se_future_predictions_300res.csv`
- `csv/gam_predictions/giant_se_future_predictions_300res_ci.csv`
- `brt_projections_by_rock/` (inputs and outputs of the ensemble script)

**Download:** [link to Google Drive / Box folder]

---

## Setup

Package versions are locked with `renv`. From the repo root:

```r
install.packages("renv")
renv::restore()
```

Main packages: `tidyverse`, `mgcv`, `tidymodels`, `xgboost`, `pdp`, `sf`, `patchwork`, `ggrepel`, `rstatix`, `effectsize`, `zoo`, `arrow`.

---

## Known issues

- **`.gitignore` has unresolved merge-conflict markers** (`<<<<<<< HEAD` … `>>>>>>>`). Git treats those lines as patterns, so the file only half works. Combine both sides and delete the markers.
- **The GAM variable-importance loop doesn't match the fitted model.** The main GAM uses `temp_surface_upwelling` and `urchin_log`. The drop-one loop instead uses `temp_deepest_upwelling`, includes an empty variable name (`""`), and adds `s(year_factor)` and `s(latitude, longitude)` terms the main model doesn't have. The importance values therefore don't describe the reported model. `predict(..., exclude = "s(year_factor)")` also refers to a term the model doesn't contain.
- **`04232026_TrendComparison.R` mixes files.** It reads the GAM `_ci` predictions but the BRT non-`_ci` predictions, so the two may cover different extents.
- **Empty scripts:** `03272026_PDP.R` and `04272026_RSQD.R` have no analysis. Delete them, or keep them as placeholders.
- **Stale deployment files:** the `Dockerfile` (and the Hugging Face header that was in the old README) deploys an `app.R` and `brt_projections_by_rock/`, neither of which is in this repo. The app now lives in its own project.
- **Committed clutter:** `.RDataTmp`, `.RDataTmp1` (~11 MB each), `.Rhistory`, and `.DS_Store` files. Remove them with `git rm --cached` and add them to `.gitignore`.
- **Filename typo:** `03282027_Ensemble_5_10.R` is dated 2027.

*[Delete this section once fixed.]*
