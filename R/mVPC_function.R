#' Output Multivariate Visual Predictive Check Plot (mVPC)
#'
#' The multivariate visual predictive check plot (mVPC) combines donutVPCs with qqplots.
#'
#' @param sim_data A data.frame containing simulation dataset. Rows correspond
#' to observations and columns correspond to variables. sim_data
#' is supposed to include a column named "simulation_nr" as an identifier
#' of each simulation run.
#' @param obs_data A data.frame containing observation dataset. Rows correspond
#' to observations and columns correspond to variables.
#' @param var A character vector containing multiple characters. The
#' vector represents the variables of interest for generating qqplots. If
#' set to NULL, every variable is included.
#'#' @param sim_nr An integer indicating the number of simulations. Each simulation
#' generates the same number of individuals as the observation dataset.
#' @param title Title of the figure.
#' @param colors_bands A vector with two strings specifying the colors of the confidence bands.
#' @param caption A logical value to indicate define the size of caption.
#' @param percentiles (A vector of) Numeric value(s) representing the
#' percentiles of the density distribution that appear in the VPC donut plot;
#' e.g., c(10, 50, 90) represents 10th, 50th and 90th percentiles.
#' @param conf_band A numeric value indicating the empirical confidence level
#' for the width of the bands; e.g., 95 indicates 95\% confidence interval.
#' @param cores An integer of cores to use; if more than 1, calculation of mVPC is done in parallel.
#'
#' @return Multivariate visual predictive check plot which contains qqplots and donutVPCs.
#'
#' @export
#'
#' @examples
#' plot <- mVPC(
#'     sim_data = pediatric_sim,
#'     obs_data = pediatric_3cov,
#'     var = NULL,
#'     sim_nr = 100,
#'     percentiles = c(10, 50, 90),
#'     colors_bands = c("#99E0DC", "#E498B4"))
#'
mVPC <- function(sim_data,
                 obs_data,
                 var = NULL,
                 sim_nr,
                 title = NULL,
                 colors_bands = c("#99E0DC", "#E498B4"),
                 caption = NULL,
                 percentiles = c(10, 50, 90),
                 conf_band = 95,
                 return_grob = FALSE,
                 cores = 1) {

  # determine the variables of interest ----
  if (is.null(var)) {
    var <- colnames(obs_data)
  } else if (!all( var %in% colnames(obs_data))) {
    #check for names not matching between obs_data and var
    stop("Variable names in var differ from names in obs_data")
  }

  # determine the variable pairs of interest ----
  pairs_matrix <- t(combinat::combn(var, 2))

  # generate subplots for qqplots
  qqplots <- get_qqplot(sim_data = sim_data,
                        obs_data = obs_data,
                        sim_nr = sim_nr,
                        conf_band = conf_band ,
                        var = var,
                        title = FALSE)

  # generate subplots for donutplots
  donutVPCs <- get_donutVPC(sim_data = sim_data,
                            obs_data = obs_data,
                            percentiles = percentiles,
                            sim_nr = sim_nr,
                            pairs_matrix = pairs_matrix,
                            conf_band = conf_band,
                            colors_bands = colors_bands,
                            cores = cores)

  # arrange the subplots
  n = length(var)
  nr_cross_plots <- choose(length(var), 2)
  layout_mat <- matrix(NA, nrow = n, ncol = n)
  layout_mat[lower.tri(layout_mat)] <- 1 : nr_cross_plots
  diag(layout_mat) <- nr_cross_plots + (1 : n)
  list_plots <- c(donutVPCs, qqplots)


  # determine the output
  if (return_grob != FALSE) {
    gridExtra::grid.arrange(grobs = list_plots, layout_matrix = layout_mat, top = title, bottom = caption)
  } else {
    mVPC_plot <- gridExtra::grid.arrange(grobs = list_plots, layout_matrix = layout_mat, top = title, bottom = caption)
    return(mVPC_plot)
  }

}


