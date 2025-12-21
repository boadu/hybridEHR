hybridEHR: Synthetic Hybrid EHR Dataset Generator with COVID/CT Research Views
hybridEHR is an R package for generating realistic synthetic Electronic Health Record (EHR) datasets with optional COVID-era encounter patterns, CT-scan research variables, and ML-ready flattened tables.
It supports export to CSV, Excel, SQLite, and JSON metadata, making it useful for:
- teaching and demonstration
- machine learning prototyping
- data engineering pipelines
- simulation studies
- full EHR system testing
- research workflows requiring realistic but non-identifiable health data
The package produces a structured, multi-table EHR dataset including:
patients, encounters, vitals, labs, medications, procedures, allergies, plus two derived research tables:
- ct_research_view: aggregated patient-level CT-related clinical features
- ml_flat_view: multi-modal ML-ready flattened dataset

Key Features
1. Multi-table synthetic EHR
Includes realistic synthetic tables:
Patients (demographics, comorbidities, insurance, vaccination)
Encounters (inpatient, outpatient, emergency, virtual)
Vitals (spo2, temperature, HR, RR, BP)
Labs (COVID-relevant inflammatory markers + routine labs)
Medications (COVID + non-COVID therapeutic patterns)
Procedures (e.g., CT, X-ray, EKG)
Allergies
2. COVID-focused simulation mode
When covid_focused = TRUE, the generator adds:
COVID test results & severity
O₂ support requirements
Ward type (ICU, step-down, general)
Steroid / anticoagulation exposure patterns
COVID-linked lab trajectories
Time-to-CT and admission-to-CT intervals
3. CT-scan research view (optional)
When include_ct_links = TRUE, the package creates:
CT scan dates
CT severity score (derived clinical severity metric)
Research-focused laboratory, vital sign, and timing variables
4. ML-ready flat table
A consolidated, patient-level table for direct ML model training:
encounter summaries
aggregated vitals
aggregated labs
demographics & comorbidity burden
5. Export utilities
export_hybrid_ehr_dataset() produces:
structured folder output
complete CSV tables
Excel workbook for analysts
SQLite database for developers
metadata and JSON configuration


Installation
- install.packages("hybridEHR)
Load the package
- library(hybridEHR)
  
Generate a standard synthetic EHR dataset
data <- generate_hybrid_ehr_dataset(
  n_patients = 500,
  n_sites = 3,
  covid_focused = TRUE,
  include_ct_links = TRUE,
  seed = 123
)


Export to CSV, Excel, SQLite
export_hybrid_ehr_dataset(
  dataset = data,
  output_dir = "ehr_example_output",
  verbose = TRUE
)

Generate + export in one command:
result <- generate_hybrid_ehr(
  n_patients = 1000,
  n_sites = 5,
  covid_focused = TRUE,
  include_ct_links = TRUE,
  output_dir = "covid_ct_dataset",
  seed = 42
)
