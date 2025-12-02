# hybridEHR

`hybridEHR` is an R package for generating **synthetic hybrid EHR datasets** with:

- core EHR tables (patients, encounters, vitals, labs, medications, procedures, allergies)
- a **COVID-focused CT research view**
- an **ML-ready flat view** for general modelling
- exports to **CSV, SQLite, and Excel** for researchers, developers, and analysts.

> Synthetic data only 

## Installation

```r
# install.packages("devtools")
devtools::install_github("boadu/hybridEHR")
