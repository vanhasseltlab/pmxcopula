#' Calculate Marginal Metrics
#'
#' @param data A data.frame with rows corresponding to observations and columns corresponding to covariates.
#' @param var A character vector containing the variables of interest for calculating
#' marginal metrics. If set to NULL, every covariate is included.
#'
#' @return A data.frame containing the marginal metrics calculated for the dataset.
#' @noRd
mrg_metrics <- function(data, var = NULL) {

  if(is.null(var)) {
    var <- colnames(data)
  }

  stats <- function(x) {
    x <- x[!is.na(x)]
    c(mean = mean(x),
      median = median(x),
      sd = sd(x),
      min = min(x),
      max = max(x),
      Q5 = quantile(x, 0.05),
      Q95 = quantile(x, 0.95))
  }

  univariate <- as.data.frame(apply(data[, var], 2, stats))

  long_format <- univariate |>
    tibble::rownames_to_column("statistic") |>
    tidyr::pivot_longer(-statistic, names_to = "covariate")

  return(long_format)
}


#' Compare Marginal Metrics Between Simulation Data and Observed Data
#'
#' @param sim_data A data.frame containing simulation dataset. Rows correspond
#' to observations and columns correspond to covariate variables. sim_data
#' is supposed to include a column named "simulation_nr" as an identifier
#' of each simulation run.
#' @param obs_data A data.frame containing observation dataset. Rows correspond
#' to observations and columns correspond to covariates.
#' @param sim_nr An integer indicating the number of simulations.
#' @param var A character vector containing the variables of interest for calculating
#' marginal metrics. If set to NULL, every covariate is included.
#' @param aim_statistic A character vector containing the marginal metrics of interest.
#' Available options include "mean", "median", "sd", "min", "max", "Q5.5%", and "Q95.95%".
#' If set to NULL, all marginal metrics are included.
#'
#' @return A data.frame containing the marginal metrics calculated for simulation data and observed data,
#' along with the differences between them.
#' @export
#'
#' @examples
#' mtr_margin <- calc_margin(
#'     sim_data = pediatric_sim,
#'     obs_data = pediatric_3cov,
#'     sim_nr = 100,
#'     var = NULL,
#'     aim_statistic = c("mean", "median", "sd")
#'     )
#'
calc_margin <- function(sim_data,
                            obs_data,
                            sim_nr = NULL,
                            var = NULL,
                            aim_statistic = NULL) {

  sim_data <- sim_data[, colSums(is.na(sim_data)) != nrow(sim_data)]

  if (is.null(var)) {
    var <- setdiff(colnames(sim_data), "simulation_nr")
  }

  # n_statistics <- length(var)*7  + 2*choose(length(var), 2)
  n_statistics <- length(var)*7
  full_results <- as.data.frame(matrix(nrow = sim_nr * n_statistics, ncol = 3))
  names(full_results) <- c("statistic", "covariate", "value")
  full_results$simulation_nr <- rep(1 : sim_nr, each = n_statistics)

  for(i in unique(sim_data$simulation_nr)) {
    sim_results <- mrg_metrics(sim_data[sim_data$simulation_nr == i, ], var = var)
    full_results[full_results$simulation_nr == i, c("statistic", "covariate", "value")] <- sim_results
  }

  full_results_compare <- full_results |>
    dplyr::mutate(value = ifelse(abs(value) == Inf, NA, value)) |>
    dplyr::left_join(mrg_metrics(obs_data, var = var)|> dplyr::rename(observed = value)) |>
    dplyr::mutate(rel_error = (value - observed)/observed,
           error = value - observed)

  if(!is.null(aim_statistic)){
    full_results_compare <- full_results_compare |>
      dplyr::filter(statistic %in% aim_statistic)
  }

  return(full_results_compare)

}
