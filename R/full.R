# Package imports (handled by roxygen2)
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @import lubridate
#' @import jsonlite
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable
#' @importFrom RSQLite SQLite
#' @importFrom stats runif rbinom rnorm
#' @importFrom magrittr %>%
NULL

#' Generate synthetic hybrid EHR tables
#'
#' @param n_patients Number of unique patients.
#' @param n_sites Number of sites/hospitals to simulate.
#' @param covid_focused Logical; if TRUE, use COVID-era encounter and lab patterns.
#' @param include_ct_links Logical; if TRUE, add CT timing variables and a CT
#'   severity score in the CT research view.
#' @param seed Optional integer used to set the random seed for reproducibility.
#' @param verbose Logical; if TRUE, print progress messages to the console.
#'
#' @return A list with elements:
#' \describe{
#'   \item{tables}{Named list of core EHR tables (patients, encounters, vitals,
#'   labs, medications, procedures, allergies).}
#'   \item{research}{Named list with `ct_research_view` (if covid_focused) and
#'   `ml_flat_view` (aggregated ML-ready table).}
#'   \item{metadata}{List of high-level generation settings and table metadata.}
#' }
#' @export
generate_hybrid_ehr_dataset <- function(
    n_patients = 500,
    n_sites = 3,
    covid_focused = TRUE,
    include_ct_links = FALSE,
    seed = NULL,
    verbose = TRUE
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (verbose) {
    cat(sprintf("Generating hybrid EHR dataset with %d patients...\n", n_patients))
  }

  ## 1. CORE PATIENT DATA ------------------------------------------------------

  if (verbose) cat("1. Generating core patient data...\n")

  sites <- paste0("SITE_", LETTERS[seq_len(n_sites)])
  patient_ids <- sprintf("PAT%06d", seq_len(n_patients))

  patients_core <- tibble::tibble(
    patient_id = patient_ids,
    mrn = sprintf("MRN%08d", sample(10000000:99999999, n_patients, replace = TRUE)),

    # Demographics
    age_years = round(runif(n_patients, 18, 95)),
    sex_male = rbinom(n_patients, 1, 0.52),
    sex = ifelse(sex_male == 1, "Male", "Female"),
    bmi = round(rnorm(n_patients, mean = 28, sd = 6), 1),

    # Smoking status
    smoking_status = sample(
      c("Never", "Former", "Current"),
      n_patients,
      prob = c(0.6, 0.2, 0.2),
      replace = TRUE
    ),
    smoker_current = as.integer(smoking_status == "Current"),
    smoker_former  = as.integer(smoking_status == "Former"),
    smoker_never   = as.integer(smoking_status == "Never"),

    # Comorbidities
    hx_hypertension     = rbinom(n_patients, 1, 0.35),
    hx_diabetes         = rbinom(n_patients, 1, 0.18),
    hx_copd_asthma      = rbinom(n_patients, 1, 0.12),
    hx_ckd              = rbinom(n_patients, 1, 0.08),
    hx_cad_hf           = rbinom(n_patients, 1, 0.15),
    hx_immunosuppressed = rbinom(n_patients, 1, 0.07),
    hx_cancer           = rbinom(n_patients, 1, 0.06),
    hx_stroke           = rbinom(n_patients, 1, 0.04),
    hx_liver_disease    = rbinom(n_patients, 1, 0.03),
    hx_dementia         = rbinom(n_patients, 1, 0.02),

    # COVID vaccination
    covid_vaccinated =
      if (covid_focused) sample(
        c("Yes", "No", "Unknown"),
        n_patients,
        prob = c(0.70, 0.25, 0.05),
        replace = TRUE
      ) else NA_character_,

    vaccine_doses = ifelse(
      covid_vaccinated == "Yes",
      sample(1:4, n_patients, prob = c(0.10, 0.15, 0.60, 0.15), replace = TRUE),
      0L
    ),

    # Address
    address = replicate(
      n_patients,
      paste(
        sample(100:9999, 1, replace = TRUE),
        sample(c("Main", "Oak", "Maple", "Cedar"), 1, replace = TRUE),
        sample(c("St", "Ave", "Rd"), 1, replace = TRUE)
      )
    ),

    city = sample(
      c("Adenta", "Madina", "Legon"),
      n_patients,
      replace = TRUE
    ),

    # 🟥 FIXED HERE — replace=TRUE prevents the crash
    state = sample(state.abb, n_patients, replace = TRUE),

    zip_code = sprintf("%05d", sample(10000:99999, n_patients, replace = TRUE)),
    phone = paste0(
      "(", sample(200:999, n_patients, replace = TRUE), ") ",
      sample(100:999, n_patients, replace = TRUE), "-",
      sample(1000:9999, n_patients, replace = TRUE)
    ),
    email = tolower(paste0("patient", seq_len(n_patients), "@email.com")),

    insurance = sample(
      c("Medicare", "Medicaid", "Private", "Self-pay"),
      n_patients,
      prob = c(0.35, 0.25, 0.35, 0.05),
      replace = TRUE
    ),

    # Site assignment
    site_id = sample(sites, n_patients, replace = TRUE),
    site_id_numeric = as.numeric(factor(site_id)) - 1L,

    # Dates
    date_of_birth = Sys.Date() - lubridate::years(age_years) -
      lubridate::days(sample(0:364, n_patients, replace = TRUE)),
    created_date = Sys.Date() - lubridate::days(sample(1:365, n_patients, replace = TRUE))
  ) %>%
    dplyr::mutate(
      last_updated = created_date + lubridate::days(sample(0:30, n_patients, replace = TRUE))
    )


  ## 2. ENCOUNTERS -------------------------------------------------------------

  if (verbose) cat("2. Generating encounter data...\n")

  n_encounters <- if (covid_focused) {
    round(n_patients * 3.5)
  } else {
    round(n_patients * 2.5)
  }

  encounters <- tibble::tibble(
    encounter_id = sprintf("ENC%08d", seq_len(n_encounters)),
    patient_id = sample(patient_ids, n_encounters, replace = TRUE),

    encounter_type = sample(
      c("Inpatient", "Outpatient", "Emergency", "Virtual"),
      n_encounters,
      prob = if (covid_focused) c(0.30, 0.40, 0.25, 0.05)
      else                 c(0.20, 0.60, 0.15, 0.05),
      replace = TRUE
    ),

    admission_datetime = sample(
      seq(as.POSIXct("2020-01-01 00:00:00", tz = "UTC"),
          as.POSIXct("2023-12-31 23:00:00", tz = "UTC"),
          by = "hour"),
      n_encounters,
      replace = TRUE
    )
  ) %>%
    dplyr::mutate(
      length_of_stay_hours = dplyr::case_when(
        encounter_type == "Inpatient" ~ round(stats::rgamma(n_encounters, shape = 2, rate = 0.1)),
        encounter_type == "Emergency" ~ round(runif(n_encounters, 2, 24)),
        TRUE                          ~ 0
      ),
      discharge_datetime = admission_datetime +
        lubridate::hours(length_of_stay_hours),

      facility = sample(
        c("General Hospital", "Medical Center", "Clinic"),
        n_encounters, replace = TRUE
      ),
      department = sample(
        c("Emergency", "ICU", "Medicine", "Pulmonology",
          "Cardiology", "Surgery"),
        n_encounters, replace = TRUE
      ),

      covid_test_result = if (covid_focused) {
        sample(
          c("Positive", "Negative", "Pending", "Not Tested"),
          n_encounters,
          prob = c(0.25, 0.65, 0.05, 0.05),
          replace = TRUE
        )
      } else {
        sample(
          c("Positive", "Negative", "Not Tested"),
          n_encounters,
          prob = c(0.05, 0.15, 0.80),
          replace = TRUE
        )
      },

      covid_severity = dplyr::if_else(
        covid_test_result == "Positive",
        sample(
          c("Asymptomatic", "Mild", "Moderate", "Severe", "Critical"),
          n_encounters,
          prob = if (covid_focused) c(0.10, 0.40, 0.30, 0.15, 0.05)
          else                c(0.30, 0.40, 0.20, 0.08, 0.02),
          replace = TRUE
        ),
        NA_character_
      ),

      o2_support_level = if (covid_focused) {
        sample(0:3, n_encounters,
               prob = c(0.60, 0.25, 0.10, 0.05),
               replace = TRUE)
      } else {
        sample(0:1, n_encounters,
               prob = c(0.90, 0.10),
               replace = TRUE)
      },

      ward_type = if (covid_focused) {
        sample(0:2, n_encounters,
               prob = c(0.40, 0.45, 0.15),
               replace = TRUE)
      } else {
        sample(0:1, n_encounters,
               prob = c(0.80, 0.20),
               replace = TRUE)
      },

      on_systemic_steroids = if (covid_focused) {
        rbinom(n_encounters, 1, 0.35)
      } else {
        rbinom(n_encounters, 1, 0.10)
      },

      on_anticoagulation = if (covid_focused) {
        rbinom(n_encounters, 1, 0.45)
      } else {
        rbinom(n_encounters, 1, 0.15)
      },

      disposition = sample(
        c("Home", "SNF", "Transfer", "Hospice", "Expired"),
        n_encounters,
        prob = c(0.80, 0.08, 0.05, 0.02, 0.05),
        replace = TRUE
      ),

      symptom_to_ct_days = if (covid_focused) {
        pmax(0, round(rnorm(n_encounters, mean = 7, sd = 4)))
      } else {
        NA_real_
      },

      admission_to_ct_hours = if (covid_focused) {
        ifelse(
          ward_type > 0,
          round(runif(n_encounters, 1, 72)),
          NA_real_
        )
      } else {
        NA_real_
      },

      primary_diagnosis = sample(
        c("Hypertension", "Diabetes", "COPD", "Pneumonia",
          "COVID-19", "UTI", "CHF", "Asthma"),
        n_encounters,
        replace = TRUE
      ),
      primary_diagnosis_code = sample(
        c("I10", "E11.9", "J44.9", "J18.9",
          "U07.1", "N39.0", "I50.9", "J45.9"),
        n_encounters,
        replace = TRUE
      )
    )

  ## 3. VITAL SIGNS ------------------------------------------------------------

  if (verbose) cat("3. Generating vital signs...\n")

  n_vitals <- n_encounters * 8L

  vitals <- tibble::tibble(
    vital_id = sprintf("VIT%08d", seq_len(n_vitals)),
    encounter_id = sample(encounters$encounter_id, n_vitals, replace = TRUE),

    measurement_datetime = sample(
      seq(as.POSIXct("2020-01-01 00:00:00", tz = "UTC"),
          as.POSIXct("2023-12-31 23:45:00", tz = "UTC"),
          by = "15 min"),
      n_vitals,
      replace = TRUE
    ),

    spo2 = pmax(70, pmin(100, round(rnorm(n_vitals, mean = 96, sd = 3)))),
    resp_rate = pmax(12, pmin(40, round(rnorm(n_vitals, mean = 18, sd = 5)))),
    heart_rate = pmax(50, pmin(140, round(rnorm(n_vitals, mean = 80, sd = 15)))),
    systolic_bp = pmax(80, pmin(200, round(rnorm(n_vitals, mean = 125, sd = 20)))),
    diastolic_bp = pmax(50, pmin(120, round(rnorm(n_vitals, mean = 78, sd = 15)))),
    temp_c = round(rnorm(n_vitals, mean = 37.0, sd = 0.7), 1),

    temperature_f = round((temp_c * 9/5) + 32, 1),
    map = round((2 * diastolic_bp + systolic_bp) / 3, 0),
    pain_score = sample(0:10, n_vitals, replace = TRUE),
    glucose = pmax(50, pmin(400, round(rnorm(n_vitals, mean = 110, sd = 30)))),

    position = sample(c("Sitting", "Supine", "Standing"),
                      n_vitals, replace = TRUE),
    oxygen_device = ifelse(
      spo2 < 94,
      sample(c("Room Air", "Nasal Cannula", "Face Mask", "Ventilator"),
             n_vitals,
             prob = c(0.3, 0.4, 0.2, 0.1),
             replace = TRUE),
      "Room Air"
    ),

    manually_entered = rbinom(n_vitals, 1, 0.2),
    device_id = paste0("DEV", sprintf("%06d", sample(1:1000, n_vitals, replace = TRUE)))
  )

  ## 4. LAB RESULTS ------------------------------------------------------------

  if (verbose) cat("4. Generating lab results...\n")

  lab_tests <- tibble::tribble(
    ~test_code, ~test_name, ~unit,  ~normal_low, ~normal_high, ~covid_relevant,
    "WBC",      "White Blood Cell Count", "K/uL", 4.5,  11.0, TRUE,
    "LYM",      "Lymphocyte Count",       "K/uL", 1.0,   4.8, TRUE,
    "NEU",      "Neutrophil Count",       "K/uL", 1.8,   7.7, TRUE,
    "PLT",      "Platelet Count",         "K/uL", 150,   450, TRUE,
    "CRP",      "C-Reactive Protein",     "mg/L", 0,     10,  TRUE,
    "DDIMER",   "D-Dimer",                "ng/mL", 0,    500, TRUE,
    "LDH",      "Lactate Dehydrogenase",  "U/L",  125,   220, TRUE,
    "FER",      "Ferritin",               "ng/mL", 20,   300, TRUE,
    "CREAT",    "Creatinine",             "mg/dL", 0.6,  1.3, TRUE,
    "HGB",      "Hemoglobin",             "g/dL",  13.5, 17.5, FALSE,
    "NA",       "Sodium",                 "mmol/L", 135, 145, FALSE,
    "K",        "Potassium",              "mmol/L", 3.5, 5.1,  FALSE,
    "AST",      "AST",                    "U/L",  10,    40,  FALSE,
    "ALT",      "ALT",                    "U/L",  7,     56,  FALSE
  )

  n_labs <- n_encounters * 6L
  lab_indices <- sample(seq_len(nrow(lab_tests)), n_labs, replace = TRUE)

  labs <- tibble::tibble(
    lab_id = sprintf("LAB%08d", seq_len(n_labs)),
    encounter_id = sample(encounters$encounter_id, n_labs, replace = TRUE),

    test_code = lab_tests$test_code[lab_indices],
    test_name = lab_tests$test_name[lab_indices],

    collection_datetime = sample(
      seq(as.POSIXct("2020-01-01 00:00:00", tz = "UTC"),
          as.POSIXct("2023-12-31 23:00:00", tz = "UTC"),
          by = "hour"),
      n_labs,
      replace = TRUE
    ),
    result_datetime = collection_datetime + lubridate::hours(sample(1:24, n_labs, replace = TRUE)),

    result_value = NA_real_,
    result_unit = lab_tests$unit[lab_indices],
    reference_low = lab_tests$normal_low[lab_indices],
    reference_high = lab_tests$normal_high[lab_indices],

    lab_department = sample(
      c("Chemistry", "Hematology", "Immunology"),
      n_labs, replace = TRUE
    ),

    verified = rbinom(n_labs, 1, 0.95),
    verified_by = ifelse(
      verified == 1,
      paste("TECH", sprintf("%04d", sample(1:100, n_labs, replace = TRUE))),
      NA_character_
    ),

    is_covid_related = lab_tests$covid_relevant[lab_indices]
  )

  # Generate realistic lab values (COVID-aware)
  for (i in seq_len(n_labs)) {
    test <- lab_tests[lab_indices[i], ]

    enc <- encounters[encounters$encounter_id == labs$encounter_id[i], ]
    is_covid <- nrow(enc) > 0 && identical(enc$covid_test_result[1], "Positive")
    severity <- if (nrow(enc) > 0) enc$covid_severity[1] else NA_character_

    mean_val <- (test$normal_low + test$normal_high) / 2
    sd_val   <- (test$normal_high - test$normal_low) / 6

    if (is_covid && isTRUE(test$covid_relevant)) {
      if (test$test_code %in% c("CRP", "FER", "LDH")) {
        multiplier <- dplyr::case_when(
          severity == "Critical" ~ 10,
          severity == "Severe"   ~ 5,
          severity == "Moderate" ~ 2,
          TRUE                   ~ 1.5
        )
        mean_val <- test$normal_high * multiplier
      } else if (test$test_code == "LYM") {
        mean_val <- test$normal_low * 0.5
      } else if (test$test_code == "DDIMER") {
        multiplier <- dplyr::case_when(
          severity == "Critical" ~ 20,
          severity == "Severe"   ~ 10,
          severity == "Moderate" ~ 5,
          TRUE                   ~ 2
        )
        mean_val <- test$normal_high * multiplier
      }
    }

    labs$result_value[i] <- stats::rnorm(1, mean = mean_val, sd = sd_val)
  }

  ## 5. MEDICATIONS ------------------------------------------------------------

  if (verbose) cat("5. Generating medications...\n")

  meds_db <- tibble::tribble(
    ~medication_name, ~class,             ~covid_related,
    "Remdesivir",     "Antiviral",        TRUE,
    "Dexamethasone",  "Corticosteroid",   TRUE,
    "Enoxaparin",     "Anticoagulant",    TRUE,
    "Azithromycin",   "Antibiotic",       TRUE,
    "Metformin",      "Antidiabetic",     FALSE,
    "Lisinopril",     "Antihypertensive", FALSE,
    "Atorvastatin",   "Statin",           FALSE,
    "Albuterol",      "Bronchodilator",   FALSE,
    "Insulin",        "Antidiabetic",     FALSE,
    "Furosemide",     "Diuretic",         FALSE
  )

  n_meds <- n_encounters * 4L
  med_idx <- sample(seq_len(nrow(meds_db)), n_meds, replace = TRUE)

  medications <- tibble::tibble(
    medication_id = sprintf("MED%08d", seq_len(n_meds)),
    encounter_id  = sample(encounters$encounter_id, n_meds, replace = TRUE),

    medication_name  = meds_db$medication_name[med_idx],
    medication_class = meds_db$class[med_idx],

    dosage = paste(
      sample(c(5, 10, 20, 40, 80), n_meds, replace = TRUE), "mg"
    ),
    frequency = sample(c("QD", "BID", "TID", "QID"),
                       n_meds, replace = TRUE),
    route = sample(c("PO", "IV", "Inhalation"),
                   n_meds,
                   prob = c(0.7, 0.2, 0.1),
                   replace = TRUE),

    start_datetime = sample(
      seq(as.POSIXct("2020-01-01 00:00:00", tz = "UTC"),
          as.POSIXct("2023-12-31 23:00:00", tz = "UTC"),
          by = "hour"),
      n_meds, replace = TRUE
    ),
    end_datetime = start_datetime + lubridate::days(
      sample(1:30, n_meds, replace = TRUE)
    ),

    prescriber = paste("Dr.", sample(LETTERS, n_meds, replace = TRUE)),
    status = sample(
      c("Active", "Completed", "Discontinued"),
      n_meds,
      prob = c(0.4, 0.5, 0.1),
      replace = TRUE
    )
  )

  ## 6. PROCEDURES & ALLERGIES -------------------------------------------------

  if (verbose) cat("6. Generating additional EHR tables...\n")

  n_procedures <- round(n_encounters * 0.8)

  procedures <- tibble::tibble(
    procedure_id = sprintf("PROC%08d", seq_len(n_procedures)),
    encounter_id = sample(encounters$encounter_id, n_procedures, replace = TRUE),

    procedure_name = sample(
      c("CT Scan", "X-ray", "EKG", "Endoscopy", "Biopsy"),
      n_procedures, replace = TRUE
    ),
    procedure_date = sample(
      seq(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "day"),
      n_procedures, replace = TRUE
    ),
    performing_md = paste("Dr.", sample(LETTERS, n_procedures, replace = TRUE))
  )

  n_allergies <- round(n_patients * 0.3)

  allergies <- tibble::tibble(
    allergy_id = sprintf("ALL%08d", seq_len(n_allergies)),
    patient_id = sample(patient_ids, n_allergies, replace = TRUE),

    allergen = sample(
      c("Penicillin", "Sulfa", "Latex", "Peanuts", "Iodine"),
      n_allergies, replace = TRUE
    ),
    reaction = sample(
      c("Rash", "Hives", "Anaphylaxis", "GI upset"),
      n_allergies, replace = TRUE
    ),
    severity = sample(
      c("Mild", "Moderate", "Severe"),
      n_allergies, replace = TRUE
    )
  )

  ## 7. CT RESEARCH VIEW -------------------------------------------------------

  if (verbose) cat("7. Creating research views...\n")

  # Always create a NULL placeholder to avoid "object not found"
  research_view <- NULL
  if (covid_focused) {

    # ---- Latest encounters ----
    latest_encounters <- encounters %>%
      dplyr::group_by(patient_id) %>%
      dplyr::slice_max(order_by = admission_datetime, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup()

    # ---- Latest vitals ----
    latest_vitals <- vitals %>%
      dplyr::inner_join(latest_encounters %>% dplyr::select(patient_id, encounter_id),
                        by = "encounter_id") %>%
      dplyr::group_by(patient_id) %>%
      dplyr::slice_max(order_by = measurement_datetime, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup()

    # ---- Latest labs ----
    latest_labs <- labs %>%
      dplyr::filter(is_covid_related) %>%
      dplyr::inner_join(latest_encounters %>% dplyr::select(patient_id, encounter_id),
                        by = "encounter_id") %>%
      dplyr::group_by(patient_id, test_code) %>%
      dplyr::slice_max(order_by = collection_datetime, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::select(patient_id, test_code, result_value) %>%
      tidyr::pivot_wider(names_from = test_code, values_from = result_value)

    # ---- Build research view safely ----
    research_view <- patients_core %>%
      dplyr::left_join(
        latest_encounters %>% dplyr::select(
          patient_id, encounter_id, admission_datetime,
          symptom_to_ct_days, admission_to_ct_hours,
          o2_support_level, ward_type, on_systemic_steroids,
          on_anticoagulation, covid_test_result, covid_severity
        ),
        by = "patient_id"
      ) %>%
      dplyr::left_join(
        latest_vitals %>% dplyr::select(
          patient_id, spo2, resp_rate, heart_rate,
          systolic_bp, diastolic_bp, temp_c
        ),
        by = "patient_id"
      ) %>%
      dplyr::left_join(latest_labs, by = "patient_id")

    # ---- SAFETY CHECK ----
    if (!"admission_datetime" %in% names(research_view)) {
      research_view$admission_datetime <- as.POSIXct(NA)
    }

    # ---- Add CT fields ----
    if (include_ct_links) {
      research_view <- research_view %>%
        dplyr::mutate(
          has_ct_scan = rbinom(dplyr::n(), 1, 0.8),

          ct_scan_date = dplyr::if_else(
            has_ct_scan == 1L,
            as.Date(admission_datetime) +
              lubridate::days(sample(0:3, dplyr::n(), replace = TRUE)),
            as.Date(NA)
          ),

          ct_severity_score = dplyr::if_else(
            has_ct_scan == 1L,
            pmin(
              25,
              round(
                10 +
                  dplyr::if_else(is.na(CRP), 0, CRP / 20) +
                  (100 - spo2) / 2 +
                  resp_rate / 2 +
                  as.numeric(factor(covid_severity)) * 2
              )
            ),
            NA_real_
          )
        )
    }
  }

  ## 8. ML FLAT VIEW ----------------------------------------------------------

  encounter_summary <- encounters %>%
    dplyr::group_by(patient_id) %>%
    dplyr::summarise(
      n_encounters       = dplyr::n(),
      n_hospitalizations = sum(encounter_type == "Inpatient"),
      n_ed_visits        = sum(encounter_type == "Emergency"),
      latest_covid_result = covid_test_result[which.max(admission_datetime)],
      ever_covid_positive = any(covid_test_result == "Positive", na.rm = TRUE),
      .groups = "drop"
    )

  vitals_summary <- vitals %>%
    dplyr::inner_join(
      encounters %>% dplyr::select(encounter_id, patient_id),
      by = "encounter_id"
    ) %>%
    dplyr::group_by(patient_id) %>%
    dplyr::summarise(
      avg_spo2       = mean(spo2, na.rm = TRUE),
      max_heart_rate = max(heart_rate, na.rm = TRUE),
      min_spo2       = min(spo2, na.rm = TRUE),
      .groups = "drop"
    )

  labs_summary <- labs %>%
    dplyr::filter(test_code %in% c("CRP", "CREAT", "WBC")) %>%
    dplyr::inner_join(
      encounters %>% dplyr::select(encounter_id, patient_id),
      by = "encounter_id"
    ) %>%
    dplyr::group_by(patient_id, test_code) %>%
    dplyr::summarise(
      max_value = max(result_value, na.rm = TRUE),
      .groups = "drop_last"
    ) %>%
    tidyr::pivot_wider(
      names_from  = test_code,
      values_from = max_value,
      names_prefix = "max_"
    )

  ml_flat_view <- patients_core %>%
    dplyr::left_join(encounter_summary, by = "patient_id") %>%
    dplyr::left_join(vitals_summary,     by = "patient_id") %>%
    dplyr::left_join(labs_summary,       by = "patient_id") %>%
    dplyr::mutate(
      age_group = cut(
        age_years,
        breaks = c(0, 40, 60, 80, 120),
        labels = c("<40", "40-59", "60-79", "80+"),
        right = FALSE
      ),
      bmi_category = cut(
        bmi,
        breaks = c(0, 18.5, 25, 30, 35, 40, 100),
        labels = c("Underweight", "Normal", "Overweight",
                   "Obese I", "Obese II", "Obese III"),
        right = FALSE
      ),
      comorbidity_count = hx_hypertension + hx_diabetes + hx_copd_asthma +
        hx_ckd + hx_cad_hf + hx_immunosuppressed
    )

  ## 9. RETURN -----------------------------------------------------------------

  if (verbose) cat("Dataset generation complete!\n")

  tables_list <- list(
    patients    = patients_core,
    encounters  = encounters,
    vitals      = vitals,
    labs        = labs,
    medications = medications,
    procedures  = procedures,
    allergies   = allergies
  )

  metadata <- list(
    n_patients       = n_patients,
    n_sites          = n_sites,
    sites            = sites,
    covid_focused    = covid_focused,
    include_ct_links = include_ct_links,
    date_generated   = as.character(Sys.Date()),
    version          = "0.1.0"
  )

  list(
    tables = tables_list,
    research = list(
      ct_research_view = research_view,
      ml_flat_view     = ml_flat_view
    ),
    metadata = metadata
  )
}

#' Export a hybrid EHR dataset to disk
#'
#' @param dataset A list as returned by [generate_hybrid_ehr_dataset()].
#' @param output_dir Directory to write files into.
#' @param verbose Logical; if TRUE, print messages.
#'
#' @return The output directory (invisibly).
#' @export
export_hybrid_ehr_dataset <- function(
    dataset,
    output_dir = "hybrid_ehr_dataset",
    verbose = TRUE
) {
  if (verbose) cat("Exporting hybrid EHR dataset...\n")

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  ## 1. COMPREHENSIVE TABLES ---------------------------------------------------

  if (verbose) cat("1. Exporting comprehensive tables...\n")

  comp_dir <- file.path(output_dir, "comprehensive_tables")
  dir.create(comp_dir, showWarnings = FALSE)

  for (table_name in names(dataset$tables)) {
    file_path <- file.path(comp_dir, paste0(table_name, ".csv"))
    utils::write.csv(dataset$tables[[table_name]], file_path, row.names = FALSE)

    if (verbose) {
      cat(sprintf(
        "  - %s.csv (%d rows)\n",
        table_name,
        nrow(dataset$tables[[table_name]])
      ))
    }
  }

  ## 2. RESEARCH VIEWS ---------------------------------------------------------

  if (verbose) cat("\n2. Exporting research views...\n")

  rv_dir <- file.path(output_dir, "research_views")
  dir.create(rv_dir, showWarnings = FALSE)

  if (!is.null(dataset$research$ct_research_view)) {
    ct_file <- file.path(rv_dir, "ct_research_view.csv")
    utils::write.csv(dataset$research$ct_research_view, ct_file, row.names = FALSE)

    if (verbose) {
      cat(sprintf(
        "  - ct_research_view.csv (%d rows, %d cols)\n",
        nrow(dataset$research$ct_research_view),
        ncol(dataset$research$ct_research_view)
      ))
    }
  }

  ml_file <- file.path(rv_dir, "ml_flat_view.csv")
  utils::write.csv(dataset$research$ml_flat_view, ml_file, row.names = FALSE)

  if (verbose) {
    cat(sprintf(
      "  - ml_flat_view.csv (%d rows, %d cols)\n",
      nrow(dataset$research$ml_flat_view),
      ncol(dataset$research$ml_flat_view)
    ))
  }

  ## 3. USER-SPECIFIC FORMATS --------------------------------------------------

  if (verbose) cat("\n3. Exporting user-specific formats...\n")

  # For researchers: single CSV
  res_dir <- file.path(output_dir, "for_researchers")
  dir.create(res_dir, showWarnings = FALSE)

  if (!is.null(dataset$research$ct_research_view)) {
    utils::write.csv(
      dataset$research$ct_research_view,
      file.path(res_dir, "research_dataset.csv"),
      row.names = FALSE
    )
  }

  # For developers: SQLite database
  dev_dir <- file.path(output_dir, "for_developers")
  dir.create(dev_dir, showWarnings = FALSE)

  db_path <- file.path(dev_dir, "ehr_database.sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  on.exit(DBI::dbDisconnect(con), add = TRUE)

  for (table_name in names(dataset$tables)) {
    DBI::dbWriteTable(con, table_name, dataset$tables[[table_name]], overwrite = TRUE)
  }

  if (verbose) {
    cat("  - SQLite database: ehr_database.sqlite\n")
  }

  # For analysts: Excel workbook
  ana_dir <- file.path(output_dir, "for_analysts")
  dir.create(ana_dir, showWarnings = FALSE)

  wb <- openxlsx::createWorkbook()

  for (table_name in names(dataset$tables)) {
    openxlsx::addWorksheet(wb, table_name)
    openxlsx::writeData(wb, table_name, dataset$tables[[table_name]])
  }

  if (!is.null(dataset$research$ct_research_view)) {
    openxlsx::addWorksheet(wb, "CT_Research_View")
    openxlsx::writeData(wb, "CT_Research_View", dataset$research$ct_research_view)
  }

  openxlsx::addWorksheet(wb, "ML_Flat_View")
  openxlsx::writeData(wb, "ML_Flat_View", dataset$research$ml_flat_view)

  excel_path <- file.path(ana_dir, "ehr_dataset.xlsx")
  openxlsx::saveWorkbook(wb, excel_path, overwrite = TRUE)

  if (verbose) {
    cat("  - Excel workbook: ehr_dataset.xlsx\n")
  }

  ## 4. METADATA & DOCUMENTATION ----------------------------------------------

  if (verbose) cat("\n4. Exporting metadata...\n")

  metadata <- dataset$metadata
  metadata$tables <- lapply(dataset$tables, function(x) {
    list(
      rows         = nrow(x),
      columns      = ncol(x),
      column_names = colnames(x)
    )
  })

  metadata_json <- file.path(output_dir, "dataset_metadata.json")
  jsonlite::write_json(metadata, metadata_json, pretty = TRUE)

  if (verbose) {
    cat("  - dataset_metadata.json\n")
  }

  readme_general <- paste(
    "Hybrid EHR Dataset",
    "==================",
    "A comprehensive synthetic EHR dataset with optional COVID-19 focus.",
    "",
    "STRUCTURE:",
    "1. comprehensive_tables/ - Full EHR tables (patients, encounters, labs, etc.)",
    "2. research_views/      - Pre-processed views for research",
    "3. for_researchers/     - Single CSV for quick analysis",
    "4. for_developers/      - SQLite database for application development",
    "5. for_analysts/        - Excel workbook for business intelligence",
    "",
    "GENERATION DETAILS:",
    sprintf("- Patients: %d", metadata$n_patients),
    sprintf("- Sites: %d", metadata$n_sites),
    sprintf("- COVID focused: %s", metadata$covid_focused),
    sprintf("- Date generated: %s", metadata$date_generated),
    "",
    "USAGE:",
    "- For CT research: Use ct_research_view.csv (if available)",
    "- For general ML: Use ml_flat_view.csv",
    "- For EHR system testing: Use comprehensive tables",
    "",
    "LICENSE:",
    "Synthetic data for research and development only.",
    "Not for clinical use.",
    sep = "\n"
  )

#  utils::writeLines(readme_general, file.path(output_dir, "README.md"))

  if (!is.null(dataset$research$ct_research_view)) {
    readme_ct <- paste(
      "CT Research View",
      "================",
      "This view contains parameters useful for COVID-19 / CT scan research.",
      "",
      "PARAMETERS INCLUDED:",
      "- Demographics: age_years, sex_male, bmi, smoking status",
      "- Comorbidities: hx_hypertension, hx_diabetes, hx_copd_asthma, etc.",
      "- Acute vitals: spo2, resp_rate, heart_rate, blood pressure, temp_c",
      "- Support & severity: o2_support_level, ward_type, medications",
      "- Labs: WBC, lymphocytes, neutrophils, CRP, D-dimer, LDH, ferritin, creatinine, platelets",
      "- Timing: symptom_to_ct_days, admission_to_ct_hours",
      "- Site: site_id_numeric",
      "",
      "USAGE FOR ML:",
      "Each row represents one patient at or near the time of CT scanning.",
      "",
      "MISSING DATA:",
      "- More labs missing in outpatients",
      "- Some timing variables missing for non-admitted patients",
      "- Use appropriate imputation methods.",
      sep = "\n"
    )

 #   utils::writeLines(readme_ct, file.path(rv_dir, "README_CT.md"))
  }

  # Configuration snapshot
  config <- list(
    generation_settings = list(
      n_patients       = metadata$n_patients,
      n_sites          = metadata$n_sites,
      covid_focused    = metadata$covid_focused,
      include_ct_links = metadata$include_ct_links
    ),
    table_sizes   = vapply(dataset$tables, nrow, integer(1)),
    column_counts = vapply(dataset$tables, ncol, integer(1))
  )

  jsonlite::write_json(config, file.path(output_dir, "generation_config.json"), pretty = TRUE)

  if (verbose) {
    cat("\n Export complete!\n")
    cat(sprintf("\nOutput directory: %s\n", output_dir))
  }

  invisible(output_dir)
}

#' High-level wrapper to generate and export a hybrid EHR dataset
#'
#' @inheritParams generate_hybrid_ehr_dataset
#' @param output_dir Directory to write export files into.
#'
#' @return A list with:
#' \describe{
#'   \item{dataset}{The in-memory dataset list (as from `generate_hybrid_ehr_dataset`).}
#'   \item{output_dir}{The output directory path where files were written.}
#' }
#' @examples
#' \dontrun{
#'   # Quick COVID CT research dataset
#'   res <- generate_hybrid_ehr(
#'     n_patients = 1000,
#'     n_sites = 4,
#'     covid_focused = TRUE,
#'     include_ct_links = TRUE,
#'     output_dir = "covid_ct_research_dataset",
#'     seed = 42
#'   )
#'
#'   # General EHR dataset
#'   res2 <- generate_hybrid_ehr(
#'     n_patients = 5000,
#'     n_sites = 10,
#'     covid_focused = FALSE,
#'     include_ct_links = FALSE,
#'     output_dir = "general_ehr_dataset",
#'     seed = 123
#'   )
#' }
#'
#' @inheritParams generate_hybrid_ehr_dataset
#' @param output_dir Directory for exported files.
#'
#' @return A list containing:
#' \describe{
#'   \item{dataset}{Generated dataset object}
#'   \item{output_dir}{Path to exported files}
#' }
#' @export
generate_hybrid_ehr <- function(
    n_patients = 500,
    n_sites = 3,
    covid_focused = TRUE,
    include_ct_links = FALSE,
    output_dir = "hybrid_ehr_dataset",
    seed = NULL,
    verbose = TRUE
) {
  if (verbose) {
    cat("========================================\n")
    cat("     HYBRID EHR DATASET GENERATOR\n")
    cat("========================================\n")
  }

  dataset <- generate_hybrid_ehr_dataset(
    n_patients       = n_patients,
    n_sites          = n_sites,
    covid_focused    = covid_focused,
    include_ct_links = include_ct_links,
    seed             = seed,
    verbose          = verbose
  )

  export_path <- export_hybrid_ehr_dataset(
    dataset    = dataset,
    output_dir = output_dir,
    verbose    = verbose
  )

  if (verbose) {
    cat("\n========================================\n")
    cat("           GENERATION SUMMARY\n")
    cat("========================================\n")
    cat(sprintf("Patients: %d\n", n_patients))
    cat(sprintf("Sites: %d\n", n_sites))
    cat(sprintf("COVID focused: %s\n", covid_focused))
    cat(sprintf("CT links included: %s\n", include_ct_links))

    if (covid_focused && !is.null(dataset$research$ct_research_view)) {
      ct_view <- dataset$research$ct_research_view
      cat("\nCT Research View contains:\n")
      cat(sprintf("  - %d patients with COVID data\n",
                  sum(!is.na(ct_view$covid_test_result))))
      cat(sprintf("  - %d COVID positive cases\n",
                  sum(ct_view$covid_test_result == "Positive", na.rm = TRUE)))
      cat(sprintf("  - %d features (excluding patient_id)\n",
                  ncol(ct_view) - 1))
    }

    cat(sprintf("\nOutput: %s\n", export_path))
    cat("========================================\n")
  }

  invisible(list(
    dataset    = dataset,
    output_dir = export_path
  ))
}
