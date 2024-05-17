# to simulate from the vine
library(rvinecopulib)

# simulate for evaluation

# tester
# simulate for copula model evaluation

#' Multiple simulations for copula model evaluation.
#'
#' @param copula An object of class "vine_dist" or "vine", from rvinecopulib
#' package.
#' @param sim_nr An integer indicating the number of simulations.
#' @param seed A numeric value to facilitate the generation of reproducible simulation data.
#'
#' @return A data.frame containing simulation dataset. Rows correspond
#' to observations and columns correspond to covariate variables. sim_data includes
#' a column named "simulation_nr" as an identifier.
#' @export
#'
#' @examples
#' copula_example <- vine(df_example, copula_controls = list(family_set = "parametric"), margins_controls = list(mult = 1), keep_data = FALSE)
#' sim_test <- rcopula(copula = copula_example, sim_nr = 10, seed = 12345)
rcopula <- function(copula = copula,
                    sim_nr = NULL,
                    seed = 12345) {
  if (is.null(sim_nr)) {
    stop("sim_nr needs to be determined")
  }

  amount = copula$nobs
  set.seed(seed)

  sim_data <- rvine(n = sim_nr*amount, copula)  %>%
    as.data.frame() %>%
    mutate(simulation_nr = rep(1:sim_nr, each = amount))
}
