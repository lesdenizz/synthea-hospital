# Synthea Healthcare Analytics

An end-to-end clinical data analytics project on the Synthea synthetic patient dataset — combining descriptive analysis, anomaly detection, predictive mortality modelling (Logistic Regression + ROC/AUC), and drug addiction therapy outcome analysis, deployed as a 9-tab interactive Shiny dashboard.

The project demonstrates a complete data analytics pipeline: raw data → preparation → descriptive → diagnostic → predictive → actionable recommendations.


---

## Dataset

| Table | Rows | Description |
|---|---|---|
| `patients.rds` | 132,607 | Demographics, birth/death dates, gender, race |
| `encounters.rds` | 1,263,669 | All clinical visits 2010–2017 |
| `observations.rds` | 5,383,318 | Clinical measurements — BMI, BP, HbA1c, Glucose, Cholesterol, HDL |

**Source:** [Synthea — Open-source Synthetic Patient Generator](https://synthea.mitre.org/downloads)  
**Period:** 2010–2017 | 2017 is a partial year — excluded from all forecasting models
LIVE SHINY - https://019f60f8-8f8c-6215-e6c4-8fd6a8d55349.share.connect.posit.cloud/
---

## R Markdown Report — Structure (6,919 lines)

### Methodological Fixes Applied

| Fix | Problem Solved |
|---|---|
| Dynamic age per encounter | Static 2017 age distorts all age-group analyses |
| Per-encounter clinical vitals | Lifetime mean collapses disease progression into noise |
| Linear regression (not polynomial) | Polynomial overfits on 6 training data points |
| LOOCV validation | R² ≥ 0.70 filter on polynomial gave false confidence |

### Sections

**Part 1 — Descriptive Analysis** (43 sections, 115+ charts)
- Patient demographics: gender, age, race distribution
- Top 10 encounter types + bubble chart intensity by age group
- Clinical indicators (BMI, SBP, Cholesterol) at encounter date — not lifetime average
- Hospital usage intensity and patient monitoring classification

**Part 2 — Anomaly Detection** (10 types)

| Anomaly | Severity |
|---|---|
| Observation recorded 31+ days before encounter | 🔴 High |
| Negative HbA1c values | 🔴 High — physiologically impossible |
| Clinical encounters after patient death | 🔴 High |
| Zero HDL Cholesterol | 🔴 High — impossible in living patients |
| Death Certification before death date | 🟡 Medium |
| BMI inconsistency (height × weight vs recorded) | 🟡 Medium |
| Duplicate passport codes | 🟡 Medium |
| Shared passports across patients | 🟡 Medium |
| Duplicate SSN records | 🟡 Medium |
| Observation-encounter year gap | 🟢 Low |

**Part 3 — Forecasting** (LOOCV validated)
- Overall encounter volume → 2018 forecast (LOOCV R²=0.94)
- Per-type encounter volume forecast (top 10 encounter types)
- Monthly clinical measurement demand forecast

**Part 4 — Predictive Analytics (Data Analyst Sections)**

| Section | Output |
|---|---|
| DA-1: Correlation Matrix | Pearson correlations — all clinical variables vs mortality flag |
| DA-2: Ranked Correlations | Variables ranked by association strength with deceased=1 |
| DA-3: Logistic Regression | Mortality prediction — 5-fold CV, 80/20 train-test split |
| DA-4: ROC Curve + AUC | Model discrimination on held-out test set |
| DA-5: Confusion Matrix | Accuracy, Sensitivity, Specificity, Precision, Kappa |
| DA-6: Feature Importance | Log-odds coefficients — which variables drive mortality risk |
| DA-7: Patient Risk Scoring | 0–100% predicted mortality probability per patient, 4 tiers |
| DA-8: Model Summary | Business value + methodology limitations |

---

## Shiny Dashboard — 9 Tabs

| Tab | Key Features |
|---|---|
| 📊 Executive Overview | 4 KPI cards, encounter trend + 2018 forecast overlay, mortality donut chart |
| 🚶 Patient Journey | Population → Wellness → Ambulatory → High Risk → Deceased funnel; encounter transition sequences; first encounter distribution; age-group × year heatmap |
| ⚠️ Hidden Ambulatory Risk | Risk tier bars, actual mortality % per tier, filterable patient table (tier, gender, age) |
| 🌬️ Respiratory Seasonality | Monthly trend by encounter type, Year × Month heatmap, peak month analysis |
| 💊 Substance & Overdose | Overdose-after-therapy patient table; encounters after death for substance patients; substance encounter type breakdown; yearly trend |
| 📈 Forecast | LOOCV-validated linear forecast with 95% CI ribbon, model metrics panel |
| 🏥 Patient Risk Monitor | Per-encounter BMI / Systolic BP / Cholesterol by age group with clinical thresholds |
| 🔍 Anomaly Center | All 4 major anomaly types — interactive tables, negative HbA1c histogram |
| 💼 Recommendations | 5 prioritised data analyst recommendations with numbers pulled live from the data |

---

## Repository Contents

| File | Description |
|---|---|
| `Synthea_Healthcare_Analytics.Rmd` | Full R Markdown report — 6,919 lines |
| `app.R` | 9-tab Shiny dashboard — memory-efficient, pre-aggregated data |
| `data_prep.R` | Runs locally once — generates `shiny_data/` folder with 11 RDS files |

**Data files are not included** (too large for GitHub). Generate via Synthea:

```r
# 1. Download Synthea and generate data
# https://synthea.mitre.org/downloads

# 2. Load into R and save as RDS
patients     <- read.csv("patients.csv")
encounters   <- read.csv("encounters.csv")
observations <- read.csv("observations.csv")
saveRDS(patients,     "patients.rds")
saveRDS(encounters,   "encounters.rds")
saveRDS(observations, "observations.rds")

# 3. Run data_prep.R to generate Shiny summaries
source("data_prep.R")   # creates shiny_data/ folder

# 4. Deploy
rsconnect::deployApp(
  appDir   = "path/to/your/folder/",
  appFiles = c("app.R",
    paste0("shiny_data/", list.files("shiny_data/"))),
  appName  = "synthea-healthcare-analytics"
)
```

---

## Key Findings

- **19.9% mortality rate** — 26,389 of 132,607 patients have a recorded death date
- **10 data anomaly types** including physiologically impossible values (negative HbA1c, zero HDL) and temporal paradoxes (encounters after death)
- **Encounter volume grew +14.8% CAGR** (2011–2016), projecting ~254,000 encounters for 2018
- **Post-therapy overdoses** — subset of drug addiction therapy patients subsequently recorded overdose encounters
- **Clinical indicators** show consistent worsening with age — 65+ group crosses hypertension threshold for systolic BP
- **Mortality prediction model** (Logistic Regression, 5-fold CV) successfully discriminates deceased from living patients — age is the dominant predictor, HDL is the strongest protective factor

---

## Tools & Libraries

R · data.table · dplyr · ggplot2 · plotly · corrplot · caret · pROC · kableExtra · lubridate · Shiny · shinydashboard · DT · scales

---

## Author

Abdulrahman Jalilov — [LinkedIn](https://uk.linkedin.com/in/abdulrahman-jalilov-526a25257) · [GitHub](https://github.com/lesdenizz)
