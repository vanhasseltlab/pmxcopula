#' MIMIC population data
#'
#' A data.frame containing 5 covariates(age, body weight , albumin, bilirubin, and serum creatinine) of ICU patient population.
#'
#' @format A data.frame with 1000 records and 5 variables.
#' 'age', numeric, range: 18 - 97 (year)
#' 'weight', numeric, range: 29.9 - 224.30 (kg)
#' 'albumin', numeric, range: 0.933~5.098 (g/dL)
#' 'bun', numeric, range: 1.70 - 159.16 (mg/dL)
#' 'creatinine', numeric, range: 0.073 - 17.489 (mg/dL)
#' @source Virtual population generated from copula model \url{https://doi.org/10.1002/cpt.3099}
"MIMIC_5cov"
