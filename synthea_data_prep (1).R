# ================================================================
# SYNTHEA SHINY DASHBOARD — DATA PREPARATION
# Author: Abdulrahman Jalilov
#
# Run ONCE locally before deploying the Shiny app.
# Reads raw .rds files, creates pre-aggregated summaries.
# Shiny app loads only these small files (<80MB total).
# ================================================================

library(data.table)
library(dplyr)
library(lubridate)

# ---- Update paths to your local files ----
RAW_PATH <- "C:/Users/aksta/Desktop/Synthea/"

cat("Loading raw data (this takes ~1 min)...\n")
patients     <- readRDS(paste0(RAW_PATH, "patients.rds"))
encounters   <- readRDS(paste0(RAW_PATH, "encounters.rds"))
observations <- readRDS(paste0(RAW_PATH, "observations .rds"))

patients     <- as.data.table(patients)
encounters   <- as.data.table(encounters)
observations <- as.data.table(observations)

cat("patients:", nrow(patients), "| encounters:", nrow(encounters),
    "| observations:", nrow(observations), "\n")

# ================================================================
# BASE PREPARATION
# ================================================================
if (!"PASSPORT" %in% names(patients)) patients[, PASSPORT := NA_character_]
if (!"SSN"      %in% names(patients)) patients[, SSN      := NA_character_]

patients[, BIRTHDATE  := as.IDate(BIRTHDATE)]
patients[, DEATHDATE  := as.IDate(DEATHDATE)]
patients[, deceased   := as.integer(!is.na(DEATHDATE))]
patients[, age := as.integer(
  difftime(fifelse(is.na(DEATHDATE), as.IDate("2017-12-31"), DEATHDATE),
           BIRTHDATE, units = "days") / 365.25)]
patients[, age_group := fcase(
  age <= 21,  "0-21",
  age <= 43,  "22-43",
  age  < 65,  "44-65",
  age >= 65,  "65+",
  default = NA_character_)]

encounters[, encounter_date  := as.IDate(DATE)]
encounters[, encounter_year  := year(encounter_date)]
encounters[, encounter_month := month(encounter_date)]
encounters[patients, on = c(PATIENT = "ID"), BIRTHDATE_p := i.BIRTHDATE]
encounters[, age_at_enc := as.integer(
  difftime(encounter_date, BIRTHDATE_p, units = "days") / 365.25)]
encounters[, age_group_enc := fcase(
  age_at_enc <= 21, "0-21",
  age_at_enc <= 43, "22-43",
  age_at_enc  < 65, "44-65",
  age_at_enc >= 65, "65+",
  default = NA_character_)]
encounters[, BIRTHDATE_p := NULL]

observations[, obs_date  := as.IDate(DATE)]
observations[, obs_year  := year(obs_date)]
observations[, obs_month := month(obs_date)]
observations[, value_num := suppressWarnings(as.numeric(VALUE))]

# ================================================================
# 1. KPI
# ================================================================
cat("Building KPIs...\n")
kpi <- list(
  total_patients    = nrow(patients),
  deceased          = sum(patients$deceased),
  living            = sum(patients$deceased == 0),
  mortality_pct     = round(mean(patients$deceased) * 100, 1),
  total_encounters  = nrow(encounters),
  total_observations= nrow(observations),
  avg_age_death     = round(mean(
    patients[deceased == 1, as.numeric(
      difftime(DEATHDATE, BIRTHDATE, units = "days") / 365.25)],
    na.rm = TRUE), 1),
  date_range        = "2010-2017"
)

# ================================================================
# 2. DEMOGRAPHICS
# ================================================================
cat("Building demographics...\n")
gender_summary <- patients[!is.na(GENDER), .N, by = GENDER][order(-N)]
gender_summary[, pct := round(N/sum(N)*100, 1)]

age_summary <- patients[!is.na(age_group), .N, by = age_group][order(age_group)]
age_summary[, pct := round(N/sum(N)*100, 1)]

race_summary <- patients[!is.na(RACE), .N, by = RACE][order(-N)][1:8]
race_summary[, pct := round(N/sum(N)*100, 1)]

mortality_by_gender <- patients[!is.na(GENDER),
  .(deceased_pct = round(mean(deceased)*100,1), n = .N),
  by = GENDER]

demographics <- list(
  gender   = gender_summary,
  age      = age_summary,
  race     = race_summary,
  mort_gen = mortality_by_gender
)

# ================================================================
# 3. ENCOUNTER SUMMARY
# ================================================================
cat("Building encounter summaries...\n")
enc_yearly <- encounters[encounter_year >= 2010 & encounter_year <= 2017,
  .N, by = encounter_year][order(encounter_year)]

enc_type_top <- encounters[!is.na(DESCRIPTION),
  .N, by = DESCRIPTION][order(-N)][1:15]

enc_age_type <- encounters[
  DESCRIPTION %in% enc_type_top$DESCRIPTION & !is.na(age_group_enc),
  .N, by = .(age_group_enc, DESCRIPTION)]

enc_monthly <- encounters[encounter_year >= 2010 & encounter_year <= 2017,
  .N, by = .(encounter_year, encounter_month)]
enc_monthly[, date := as.Date(sprintf("%04d-%02d-01", encounter_year, encounter_month))]

encounter_data <- list(
  yearly    = enc_yearly,
  top_types = enc_type_top,
  age_type  = enc_age_type,
  monthly   = enc_monthly
)

# ================================================================
# 4. CLINICAL INDICATORS (per-encounter latest value)
# ================================================================
cat("Building clinical indicators...\n")
obs_key <- observations[
  DESCRIPTION %in% c("Body Mass Index","Systolic Blood Pressure",
                     "Total Cholesterol","Glucose",
                     "Hemoglobin A1c/Hemoglobin.total in Blood",
                     "High Density Lipoprotein Cholesterol") &
    !is.na(value_num),
  .(PATIENT, obs_date, DESCRIPTION, value_num)]

setkey(obs_key, PATIENT, obs_date)
enc_small <- encounters[, .(PATIENT, encounter_date, age_group_enc)]
setkey(enc_small, PATIENT, encounter_date)

get_enc_val <- function(desc, col_nm) {
  tmp <- obs_key[DESCRIPTION == desc]
  res <- tmp[enc_small, on = .(PATIENT, obs_date <= encounter_date),
             mult = "last", .(PATIENT, encounter_date, age_group_enc, v = x.value_num)]
  setnames(res, "v", col_nm)
  res
}

bmi_enc  <- get_enc_val("Body Mass Index",        "bmi")
sbp_enc  <- get_enc_val("Systolic Blood Pressure", "sbp")
chol_enc <- get_enc_val("Total Cholesterol",       "chol")
gluc_enc <- get_enc_val("Glucose",                 "gluc")

bmi_by_age  <- bmi_enc[!is.na(bmi) & !is.na(age_group_enc),
  .(avg = round(mean(bmi,na.rm=TRUE),1), n=.N), by=age_group_enc]
sbp_by_age  <- sbp_enc[!is.na(sbp) & !is.na(age_group_enc),
  .(avg = round(mean(sbp,na.rm=TRUE),1), n=.N), by=age_group_enc]
chol_by_age <- chol_enc[!is.na(chol) & !is.na(age_group_enc),
  .(avg = round(mean(chol,na.rm=TRUE),1), n=.N), by=age_group_enc]

clinical_data <- list(bmi_age=bmi_by_age, sbp_age=sbp_by_age, chol_age=chol_by_age)

# ================================================================
# 5. RESPIRATORY SEASONALITY
# ================================================================
cat("Building respiratory data...\n")
resp_keywords <- c("Acute bronchitis","Upper respiratory","Sinusitis",
                   "Viral sinusitis","Respiratory","Bronchitis","Pneumonia",
                   "Otitis media","Streptococcal sore throat")

respiratory_enc <- encounters[
  grepl(paste(resp_keywords, collapse="|"), DESCRIPTION, ignore.case=TRUE) &
    encounter_year >= 2010 & encounter_year <= 2017,
  .(count = .N),
  by = .(year = encounter_year, month = encounter_month, DESCRIPTION)]
respiratory_enc[, date := as.Date(sprintf("%04d-%02d-01", year, month))]

# ================================================================
# 6. PATIENT RISK MONITORING
# ================================================================
cat("Building risk data...\n")
last_vitals <- obs_key[order(PATIENT, DESCRIPTION, obs_date)][
  , .SD[.N], by = .(PATIENT, DESCRIPTION)
] |> dcast(PATIENT ~ DESCRIPTION, value.var = "value_num")

setnames(last_vitals,
  intersect(names(last_vitals), c(
    "Body Mass Index","Systolic Blood Pressure","Total Cholesterol",
    "Glucose","Hemoglobin A1c/Hemoglobin.total in Blood",
    "High Density Lipoprotein Cholesterol")),
  c("bmi","sbp","chol","gluc","hba1c","hdl")[
    seq_along(intersect(names(last_vitals), c(
      "Body Mass Index","Systolic Blood Pressure","Total Cholesterol",
      "Glucose","Hemoglobin A1c/Hemoglobin.total in Blood",
      "High Density Lipoprotein Cholesterol")))])

for (col in c("bmi","sbp","chol","gluc","hba1c","hdl")) {
  if (!col %in% names(last_vitals)) last_vitals[, (col) := NA_real_]
}

risk_patients <- patients[,.(PATIENT=ID, age, age_group, GENDER, deceased,
                              DEATHDATE, BIRTHDATE)][
  last_vitals, on="PATIENT"]

risk_patients[, risk_flags := 0L]
risk_patients[!is.na(bmi)  & bmi  >= 30,  risk_flags := risk_flags + 1L]
risk_patients[!is.na(sbp)  & sbp  >= 140, risk_flags := risk_flags + 1L]
risk_patients[!is.na(gluc) & gluc >= 126, risk_flags := risk_flags + 1L]
risk_patients[!is.na(hba1c)& hba1c > 0 & hba1c >= 9, risk_flags := risk_flags + 1L]
risk_patients[!is.na(chol) & chol >= 240, risk_flags := risk_flags + 1L]
risk_patients[, risk_tier := fcase(
  risk_flags == 0, "Low Risk",
  risk_flags == 1, "Moderate Risk",
  risk_flags == 2, "High Risk",
  risk_flags >= 3, "Critical Risk",
  default = "Unknown")]

risk_summary <- risk_patients[, .(
  patients = .N,
  actual_deceased = sum(deceased, na.rm=TRUE),
  mortality_pct   = round(sum(deceased,na.rm=TRUE)/.N*100,1),
  avg_age         = round(mean(age,na.rm=TRUE),1)
), by = risk_tier][order(-mortality_pct)]

# ================================================================
# 7. SUBSTANCE / DRUG ADDICTION ANALYSIS
# ================================================================
cat("Building substance addiction data...\n")
substance_keywords <- c(
  "Drug overdose","Opioid","Substance","Alcohol","Cannabis",
  "Cocaine","Heroin","Methamphetamine","Addiction","Dependency",
  "Abuse","Withdrawal","Detox","Methadone","Naloxone","Buprenorphine"
)
substance_pattern <- paste(substance_keywords, collapse="|")

# Substance-related encounters
substance_enc <- encounters[
  grepl(substance_pattern, DESCRIPTION, ignore.case=TRUE) |
    grepl(substance_pattern, REASONDESCRIPTION, ignore.case=TRUE)]

substance_patients_ids <- unique(substance_enc$PATIENT)
cat("Substance patients:", length(substance_patients_ids), "\n")

# Overdose encounters
overdose_enc <- encounters[
  grepl("overdose|poisoning|toxic", DESCRIPTION, ignore.case=TRUE) |
    grepl("overdose|poisoning|toxic", REASONDESCRIPTION, ignore.case=TRUE)]

# Patients who had substance therapy THEN overdose
therapy_keywords <- c("therapy","treatment","rehabilitation","detox",
                      "counseling","methadone","buprenorphine","naloxone")
therapy_enc <- substance_enc[
  grepl(paste(therapy_keywords, collapse="|"), DESCRIPTION, ignore.case=TRUE) |
    grepl(paste(therapy_keywords, collapse="|"), REASONDESCRIPTION, ignore.case=TRUE)]

therapy_pts <- unique(therapy_enc$PATIENT)

# Overdose AFTER therapy for same patients
overdose_after_therapy <- overdose_enc[
  PATIENT %in% therapy_pts,
  .(PATIENT, overdose_date = encounter_date, overdose_type = DESCRIPTION)]

# Join with therapy date to confirm overdose is AFTER therapy
therapy_dates <- therapy_enc[, .(
  therapy_start = min(encounter_date),
  therapy_end   = max(encounter_date)
), by = PATIENT]

ot_joined <- overdose_after_therapy[therapy_dates, on = "PATIENT", nomatch=0]
ot_joined <- ot_joined[overdose_date > therapy_start]

# Patient demographics for substance patients
substance_pt_demo <- patients[ID %in% substance_patients_ids,
  .(PATIENT=ID, age, age_group, GENDER, deceased, DEATHDATE)]

# Encounters AFTER DEATH for substance patients
substance_deceased <- patients[ID %in% substance_patients_ids & deceased==1,
  .(PATIENT=ID, DEATHDATE)]

enc_after_death_substance <- encounters[
  PATIENT %in% substance_deceased$PATIENT][
  substance_deceased, on="PATIENT", nomatch=0][
  encounter_date > DEATHDATE, .(
    PATIENT, encounter_date, DEATHDATE,
    days_after = as.integer(encounter_date - DEATHDATE),
    enc_type   = DESCRIPTION
  )][order(-days_after)]

# Yearly substance encounter trend
substance_yearly <- substance_enc[encounter_year >= 2010 & encounter_year <= 2017,
  .N, by = .(encounter_year, DESCRIPTION)][order(encounter_year,-N)]

substance_type_summary <- substance_enc[,
  .(count=.N, unique_patients=uniqueN(PATIENT)),
  by=DESCRIPTION][order(-count)][1:15]

substance_data <- list(
  patient_ids        = substance_patients_ids,
  encounters         = substance_enc[, .(PATIENT,encounter_date,
                                          encounter_year,DESCRIPTION,REASONDESCRIPTION)],
  overdose_after_therapy = ot_joined,
  therapy_dates      = therapy_dates,
  pt_demo            = substance_pt_demo,
  enc_after_death    = enc_after_death_substance,
  yearly_trend       = substance_yearly,
  type_summary       = substance_type_summary,
  overdose_enc       = overdose_enc[, .(PATIENT,encounter_date,
                                         encounter_year,DESCRIPTION,REASONDESCRIPTION)]
)

# ================================================================
# 8. PATIENT JOURNEY
# ================================================================
cat("Building patient journey data...\n")

# Top 12 encounter types for journey analysis
top12 <- enc_type_top[1:12, DESCRIPTION]

# For each patient: sequence of encounter types by year
journey_wide <- encounters[
  DESCRIPTION %in% top12 & encounter_year >= 2010 & encounter_year <= 2016,
  .(count = .N),
  by = .(PATIENT, encounter_year, DESCRIPTION)]

# Most common "first encounter type" per patient
first_enc <- encounters[order(PATIENT, encounter_date)][
  , .SD[1], by = PATIENT][
  , .(PATIENT, first_type = DESCRIPTION, first_year = encounter_year)]

# Most common encounter transitions (what comes after what)
# For each patient, order encounters by date, then compute bigrams
enc_ordered <- encounters[order(PATIENT, encounter_date),
  .(PATIENT, DESCRIPTION, encounter_date)]
enc_ordered[, next_type := shift(DESCRIPTION, type="lead"), by=PATIENT]
enc_ordered[, same_patient := PATIENT == shift(PATIENT, type="lead")]

transitions <- enc_ordered[
  !is.na(next_type) & same_patient == TRUE &
    DESCRIPTION %in% top12 & next_type %in% top12,
  .(count = .N),
  by = .(from = DESCRIPTION, to = next_type)][order(-count)][1:30]

# Patient journey funnel: how many patients progress through stages
# Stage 1: Wellness → Stage 2: Ambulatory → Stage 3: Chronic condition
wellness_pts  <- unique(encounters[grepl("wellness|well child", DESCRIPTION, ignore.case=TRUE), PATIENT])
ambulatory_pts<- unique(encounters[grepl("ambulatory|outpatient", DESCRIPTION, ignore.case=TRUE), PATIENT])
chronic_pts   <- unique(risk_patients[risk_flags >= 2, PATIENT])
deceased_pts  <- unique(patients[deceased == 1, ID])

journey_funnel <- data.table(
  Stage      = c("Total Patients","Wellness Visit","Ambulatory Care",
                 "High Risk (≥2 flags)","Deceased"),
  Count      = c(nrow(patients), length(wellness_pts), length(ambulatory_pts),
                 length(chronic_pts), sum(patients$deceased)),
  Color      = c("#2A9D8F","#457B9D","#F4A261","#E76F51","#2C3E50")
)

# Yearly encounter mix per age group
journey_age_year <- encounters[
  !is.na(age_group_enc) & encounter_year >= 2010 & encounter_year <= 2016,
  .N, by = .(age_group_enc, encounter_year)][order(age_group_enc, encounter_year)]

journey_data <- list(
  top12         = top12,
  wide          = journey_wide,
  first_enc     = first_enc,
  transitions   = transitions,
  funnel        = journey_funnel,
  age_year      = journey_age_year
)

# ================================================================
# 9. ANOMALY SUMMARY
# ================================================================
cat("Building anomaly data...\n")

# Observation-encounter gap
obs_with_enc <- observations[!is.na(ENCOUNTER) & !is.na(value_num),
  .(PATIENT, ENCOUNTER, obs_date)]
enc_dates <- encounters[, .(ENCOUNTER=ID, encounter_date)]
obs_gap <- obs_with_enc[enc_dates, on="ENCOUNTER", nomatch=0]
obs_gap[, gap_days := as.integer(encounter_date - obs_date)]
gap_anomalies <- obs_gap[gap_days > 31, .(count=.N, patients=uniqueN(PATIENT))]

# Negative HbA1c
neg_hba1c <- observations[
  DESCRIPTION == "Hemoglobin A1c/Hemoglobin.total in Blood" &
    !is.na(value_num) & value_num < 0,
  .(PATIENT, obs_date, value_num)]

# Encounters after death
enc_dead <- encounters[patients[deceased==1, .(ID, DEATHDATE)],
                       on=c(PATIENT="ID"), nomatch=0]
after_death <- enc_dead[!is.na(DEATHDATE) & encounter_date > DEATHDATE,
  .(PATIENT, encounter_date, DEATHDATE,
    days_after = as.integer(encounter_date - DEATHDATE),
    enc_type = DESCRIPTION)]

# Zero HDL
zero_hdl <- observations[
  DESCRIPTION == "High Density Lipoprotein Cholesterol" &
    !is.na(value_num) & value_num == 0]

anomaly_summary <- data.table(
  Anomaly  = c("Obs-Encounter Gap >31 days","Negative HbA1c",
               "Encounters After Death","Zero HDL Cholesterol"),
  Records  = c(nrow(obs_gap[gap_days>31]), nrow(neg_hba1c),
               nrow(after_death), nrow(zero_hdl)),
  Patients = c(uniqueN(obs_gap[gap_days>31, PATIENT]),
               uniqueN(neg_hba1c$PATIENT),
               uniqueN(after_death$PATIENT),
               uniqueN(zero_hdl$PATIENT))
)

anomaly_data <- list(
  summary    = anomaly_summary,
  gap        = obs_gap[gap_days > 31, .(PATIENT, obs_date, encounter_date, gap_days)][1:min(.N,5000)],
  neg_hba1c  = neg_hba1c,
  after_death= after_death,
  zero_hdl   = zero_hdl
)

# ================================================================
# 10. FORECAST (pre-computed)
# ================================================================
cat("Building forecasts...\n")
library(caret)

enc_train <- enc_yearly[encounter_year >= 2011 & encounter_year <= 2016]
enc_train[, year_index := encounter_year - 2010]
enc_model <- lm(N ~ year_index, data = enc_train)
enc_r2    <- round(summary(enc_model)$r.squared, 3)

ctrl_loo  <- trainControl(method="LOOCV")
cv_enc    <- suppressWarnings(train(N ~ year_index, data=as.data.frame(enc_train),
                                     method="lm", trControl=ctrl_loo))
enc_loocv <- round(cv_enc$results$Rsquared, 3)

fc_2018   <- predict(enc_model, newdata=data.frame(year_index=8),
                      interval="prediction")[1,]

forecast_data <- list(
  yearly       = enc_yearly,
  train_r2     = enc_r2,
  loocv_r2     = enc_loocv,
  forecast_2018= round(fc_2018, 0),
  model        = enc_model
)

# ================================================================
# 11. SAVE ALL
# ================================================================
cat("Saving RDS files...\n")
dir.create("shiny_data", showWarnings=FALSE)

saveRDS(kpi,             "shiny_data/kpi.rds")
saveRDS(demographics,    "shiny_data/demographics.rds")
saveRDS(encounter_data,  "shiny_data/encounter_data.rds")
saveRDS(clinical_data,   "shiny_data/clinical_data.rds")
saveRDS(respiratory_enc, "shiny_data/respiratory.rds")
saveRDS(risk_patients,   "shiny_data/risk_patients.rds")
saveRDS(risk_summary,    "shiny_data/risk_summary.rds")
saveRDS(substance_data,  "shiny_data/substance_data.rds")
saveRDS(journey_data,    "shiny_data/journey_data.rds")
saveRDS(anomaly_data,    "shiny_data/anomaly_data.rds")
saveRDS(forecast_data,   "shiny_data/forecast_data.rds")

cat("\n=== DATA PREPARATION COMPLETE ===\n")
cat("Files saved to: shiny_data/\n")
cat("Estimated total size: <80MB\n")
cat("Shiny app memory usage: <300MB (well under 1GB)\n")
