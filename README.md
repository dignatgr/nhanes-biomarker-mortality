# NHANES biomarkers and mortality

Association of high-sensitivity C-reactive protein (hs-CRP) and glycated
hemoglobin (HbA1c) with all-cause and cardiovascular mortality in US adults,
using NHANES 1999-2018 linked to the NCHS public-use mortality files.

The analysis covers survey-weighted Cox models, dose-response modelling with
splines, competing-risks analysis, incremental prognostic value, multiple
imputation of missing data and sensitivity analyses (lag time, E-value).
Data preparation and a weighted Cox model are also implemented in SAS and
cross-validated against the R results.

**Stack:** R (survey, survival, mice, Quarto, renv), SAS.