#' MIMIC population data
#'
#' A data.frame containing 5 covariates(age, body weight , albumin, log-transformed bilirubin, and log-transformed serum creatinine) of ICU patient population.
#'
#' @format A data.frame with 1000 records and 5 variables.
#' 'age', numeric, range: 18 - 97 (year)
#' 'weight', numeric, range: 29.9 - 224.30 (kg)
#' 'albumin', numeric, range: 0.933~5.098 (g/dL)
#' 'logbun', numeric, range: 0.53 - 5.07
#' 'logcrea', numeric, range: -2.62 - 2.86
#' @source Virtual population generated from copula model \url{https://doi.org/10.1002/cpt.3099}
"MIMIC_5cov"
