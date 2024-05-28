#' Multiple Simulations For Copula Model Evaluation
#'
#' @param copula An object of class "vine_dist" or "vine", from rvinecopulib
#' package.
#' @param sim_nr An integer indicating the number of simulations.
#'
#' @return A data.frame containing simulation dataset. Rows correspond
#' to observations and columns correspond to covariate variables. sim_data includes
#' a column named "simulation_nr" as an identifier.
#' @export
#'
#' @examples
#' # load a copula model: pediatric_cop
#' file_path <- system.file("extdata", "pediatric_cop.Rdata", package = "pmxcopula")
#' load(file_path)
#' pediatric_sim <- rcopula(pediatric_cop, sim_nr = 100)
rcopula <- function(copula = copula,
                    sim_nr = NULL) {

  if (is.null(sim_nr)) {
    stop("sim_nr needs to be determined")
  }

  amount = copula$nobs

  sim_data <- rvinecopulib::rvine(n = sim_nr*amount, copula)  |>
    as.data.frame() |>
    dplyr::mutate(simulation_nr = rep(1:sim_nr, each = amount))
}

#' Simulate Subset Virtual Population
#'
#' @param copula Copula model, an object of class "vine_dist" or "vine", from rvinecopulib
#' package.
#' @param n_sim Number of individuals.
#' @param xmin A numeric vector of length c; Lower bound for the simulation condition (only for continuous data); NULL means no boundary.
#' @param xmax A numeric vector; Upper bound for the simulation condition (only for continuous data); NULL means no boundary.
#' @param category A list with elements to specify the category for the simulation condition (only for continuous data); NULL means no boundary.
#' @param var_types A vector of variable types, a length d vector; e.g., c("c", "c") for two continuous variables, or c("c", "d") for first variable continuous and second discrete.
#'
#' @return  A data.frame containing simulation dataset. Rows correspond
#' to observations and columns correspond to covariate variables.
#' @export
#'
#' @examples
#' # load a copula model: pediatric_cop
#' file_path <- system.file("extdata", "pediatric_cop.Rdata", package = "pmxcopula")
#' load(file_path)
#' pediatric_sub <- rscopula(
#'     pediatric_cop,
#'     n_sim = 100,
#'     xmin = c(10,100, 100),
#'     var_types = c("c", "c", "c")
#'     )
rscopula <- function(copula = copula,
                        n_sim = NULL,
                        xmin = NULL,
                        xmax = NULL,
                        category = NULL,
                        var_types = NULL){

  ## check if the input value is legal ----
  # check if the var_types(necessary part) is valid
  if (is.null(var_types)) {
    stop("'var_types' needs to be specified.")
  }

  var = copula$names
  ## estimate the proportion ----
  amount = copula$nobs
  pseudo_sim = rvinecopulib::rvine(n = amount, copula) |> as.data.frame()
  # separate discrete vars and continuous vars
  var_c = var[var_types == "c"]
  var_d = var[var_types == "d"]


  # check if xmin is valid
  if(!is.null(xmin)) {
    if (length(xmin) != length(var_c)) {
      stop("The length of 'xmin' does not match the number of continuous covariates.")
    }
  }
  # check if xmax is valid
  if(!is.null(xmax)){
    if (length(xmax) != length(var_c)) {
      stop("The length of 'xmax' does not match the number of continuous covariates.")
    }
  }

  # check if category is valid
  if(!is.null(category)) {
    if (length(category) != length(var_d)) {
      stop("The length of 'category' does not match the number of discrete covariates.")
    }
  }


  filter_subset <- function(data,
                            xmin,
                            xmax,
                            category,
                            var_types,
                            var_c,
                            var_d){
    ### filter based on continuous vars ----
    if (length(var_c) != 0) {
      # filter by minimum
      for (c in 1 : length(var_c)) {

        if (!is.null(xmin)){
          if (!is.na(xmin[c])) {
            name_c = var_c[c]
            data <- data  |>
              dplyr::filter(name_c > xmin[c])
          }
        }
      }

      # filter by maximum
      for (c in 1 : length(var_c)) {

        if (!is.null(xmax)){
          if (!is.na(xmax[c])) {
            name_c = var_c[c]
            data <- data  |>
              dplyr::filter(name_c < xmax[c])
          }
        }
      }
    }

    ### filter based on discrete vars ----
    if (length(var_d) != 0) {

      if(!is.null(category)) {
        for (d in 1 : length(var_d)) {
          if (category[[d]] == "all") {
            data <- data
          } else{
            name_d = var_d[d]
            data <- data  |>
              dplyr::filter(name_d %in% category[[d]])
          }
        }
      }
    }

    return(data)
  }


  pseudo_sim <- filter_subset(data = pseudo_sim,
                xmin = xmin,
                xmax = xmax,
                category = category,
                var_types = var_types,
                var_c = var_c,
                var_d = var_d)

  fold <- ceiling(amount/(nrow(pseudo_sim)))
  Nsim <- ceiling(fold*1.3*n_sim)

  ## simulation ---
  cat("Simulation in progress... Please wait.\n")
  sim = rvinecopulib::rvine(n = Nsim, copula) |> as.data.frame()


  sim <- filter_subset(data = sim,
                      xmin = xmin,
                      xmax = xmax,
                      category = category,
                      var_types = var_types,
                      var_c = var_c,
                      var_d = var_d)

  sim = sim |> dplyr::sample_n(n_sim)

  cat("Subset simulation completed successfully.\n")
  return(sim)
}

