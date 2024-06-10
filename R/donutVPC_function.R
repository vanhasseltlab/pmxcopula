#' Create donutVPCs From Simulation Data And Observed Data
#'
#' @param sim_data A data.frame containing simulation dataset. Rows correspond
#' to observations and columns correspond to variable variables. sim_data
#' is supposed to include a column named "simulation_nr" as an identifier
#' of each simulation run.
#' @param obs_data A data.frame containing observation dataset. Rows correspond
#' to observations and columns correspond to variables.
#' @param percentiles (A vector of) Numeric value(s) representing the
#' percentiles of the density distribution that appear in the VPC donut plot;
#' e.g., c(10, 50, 90) represents 10th, 50th and 90th percentiles.
#' @param sim_nr An integer indicating the number of simulations.
#' @param pairs_matrix Matrix with 2 column and each row containing a pair combination of
#' variable names. If set to NULL, every possible variable
#' pair combination is included.
#' @param conf_band A numeric value indicating the empirical confidence level
#' for the width of the bands; e.g., 95 indicates 95\% confidence interval.
#' @param colors_bands A vector with two strings specifying the colors of the confidence bands.
#'
#' @noRd
get_donutVPC <- function(sim_data,
                         obs_data,
                         percentiles = c(10, 50, 90),
                         sim_nr,
                         pairs_matrix = NULL,
                         conf_band = 95,
                         colors_bands = c("#99E0DC", "#E498B4")) {

  # check if the pairs_matrix is valid
  if (is.null(pairs_matrix)) {
    pairs_matrix <- t(combinat::combn(colnames(obs_data), 2))
  }

  if (!all(pairs_matrix %in% colnames(obs_data))) {
    stop("variable names in pairs_matrix do not exist in obs_data.")
  }

  # generate obs_contours
  obs_contours <- NULL
  for (p in 1:nrow(pairs_matrix)) {
    kd_obs <- ks::kde(obs_data |> dplyr::select(pairs_matrix[p, ]) |> na.omit(), compute.cont = TRUE)
    contour_obs <- with(kd_obs, contourLines(x = eval.points[[1]], y = eval.points[[2]],
                                             z = estimate, levels = cont[paste0(100-percentiles,"%")]))
    obs_contours <- rbind.data.frame(obs_contours, extract_contour_df(contour_obs, kd_obs$cont, 0, pairs_matrix[p, ]))
  }

  # generate sim_contours
  sim_contours <- simulate_contours(sim_data = sim_data,
                                    percentiles = percentiles,
                                    sim_nr = sim_nr,
                                    pairs_matrix = pairs_matrix)


  sim_contours_gg <- create_geom_donutVPC(sim_contours = sim_contours,
                                          conf_band = conf_band,
                                          colors_bands = colors_bands)

  # plot dountVPC(s) for every variable pair
  plot_list <- list()
  for (i in 1:nrow(pairs_matrix)) {
    var_pair <- paste0(pairs_matrix[i, 1], "-", pairs_matrix[i, 2])
    plot_list[[var_pair]] <- obs_contours |>
      dplyr::mutate(key = paste(percentile, var1, var2, sim_nr, circle)) |>
      dplyr::filter(var1 == pairs_matrix[i, 1], var2 == pairs_matrix[i, 2]) |>
      ggplot2::ggplot() +
      sim_contours_gg[[i]] +
      ggplot2::geom_path(ggplot2::aes(x = x, y = y, color = percentile, group = key), color = "black") +
      ggplot2::labs(x = pairs_matrix[i, 1], y = pairs_matrix[i, 2]) +
      ggplot2::theme_bw() + ggplot2::theme(aspect.ratio = 1)
  }

  attr(plot_list, "obs_contours") <- obs_contours

  return(plot_list)
}


#' Output donutVPCs From Simulation Data And Observed Data
#'
#' @param sim_data A data.frame containing simulation dataset. Rows correspond
#' to observations and columns correspond to variables. sim_data
#' is supposed to include a column named "simulation_nr" as an identifier
#' of each simulation run.
#' @param obs_data A data.frame containing observation dataset. Rows correspond
#' to observations and columns correspond to variables.
#' @param percentiles  (A vector of) Numeric value(s) representing the
#' percentiles of the density distribution that appear in the VPC donut plot;
#' e.g., c(10, 50, 90) represents 10th, 50th and 90th percentiles.
#' @param sim_nr An integer indicating the number of simulations.
#' @param pairs_matrix Matrix with 2 column and each row containing a pair of
#' variable names. If set to NULL, every possible variable
#' pair is included.
#' @param conf_band A numeric value indicating the empirical confidence level
#' for the width of the bands; e.g., 95 indicates 95\% confidence interval.
#' @param colors_bands A vector with two strings specifying the colors of the confidence bands.
#'
#' @return A patchwork object of donutVPC for variable pairs.
#' @export
#'
#' @examples
#' # plot the donut VPC for the variable pairs CREA-AGE and CREA-BW
#' plot <- donutVPC(
#'     sim_data = pediatric_sim,
#'     obs_data = pediatric_3cov,
#'     percentiles = c(10, 50, 90),
#'     sim_nr = 100,
#'     pairs_matrix = matrix(c("CREA","CREA", "AGE", "BW"),2,2),
#'     conf_band = 95,
#'     colors_bands = c("#99E0DC", "#E498B4")
#'     )
#'
donutVPC <- function(sim_data,
                       obs_data,
                       percentiles = c(10, 50, 90),
                       sim_nr,
                       pairs_matrix = NULL,
                       conf_band = 95,
                       colors_bands = c("#99E0DC", "#E498B4")) {

  if (is.null(pairs_matrix)) {
    pairs_matrix <- t(combinat::combn(colnames(obs_data), 2))
  }

  # generate the geom data for dount VPC
  donutVPC_geom <- get_donutVPC(sim_data = sim_data,
                                obs_data = obs_data,
                                percentiles = percentiles,
                                sim_nr = sim_nr,
                                pairs_matrix = pairs_matrix,
                                conf_band = conf_band,
                                colors_bands = colors_bands)

  plot_donuts <- patchwork::wrap_plots(donutVPC_geom, nrow = floor(sqrt(nrow(pairs_matrix))))

  return(plot_donuts)
}
