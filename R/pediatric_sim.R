#' Simulated pediatric population data
#'
#' A data.frame containing 100 simulations of pediatric_3cov.
#'
#' @format A numeric data.frame with 44500 records and 4 variables.
#' 'AGE', numeric, range: 0 - 736 (day)
#' 'CREA', numeric, range: 0 - 158.5 ($\\mathrm{\\mu}$mol/L)
#' 'BW', numeric, range: 0 - 18795.8 (g)
#' 'simulation_nr', numeric, range: 1 - 100
#' @source Virtual population generated from pediatric_cop copula model (file path: system.file("extdata", "pediatric_cop.Rdata", package = "pmxcopula"))
"pediatric_sim"
