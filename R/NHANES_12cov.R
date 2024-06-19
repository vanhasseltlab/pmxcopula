#' NHANES population data
#'
#' A data.frame containing 12 covariates (sex, race-ethnicity, age, body weight, height, fat mass, serum creatinine, ALT, AST, ALP, albumin and bilirubin) of the American population
#'
#' @format A numeric data.frame with 1000 records and 12 variables.
#' 'Sex', character, category: male, female
#' 'Race', character, category: Hispanic, White, Africa American, Asian, Other race
#' 'Age', numeric, range: 19 - 79 (year)
#' 'Weight', numeric, range: 39.8 - 95.7 (kg)
#' 'Height', numeric, range: 138.6 - 196.3 (cm)
#' 'Fat', numeric, range: 7.33 - 41.53 (kg)
#' 'SCR', numeric, range: 0.34 - 11.61 (mg/dL)
#' 'ALT', numeric，range: 88 - 338 (U/L)
#' 'AST', numeric, range: 12 - 266 (U/L)
#' 'ALP', numeric, range: 25 - 192 (U/L)
#' 'Albumin', numeric, range: 2.9 - 5.3 (g/dL)
#' 'BR', numeric, range: 0.1 - 2.8 (mg/dL)
#' @source Virtual population generated from NHANES copula model \url{https://cocosim.lacdr.leidenuniv.nl/}
"NHANES_12cov"
