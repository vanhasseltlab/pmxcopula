#' Plot Univariate Density Curves From Simulation Data And Observed Data
#'
#' @param sim_data A data.frame containing simulation dataset. Rows correspond
#' to observations and columns correspond to variables. sim_data
#' is supposed to include a column named "simulation_nr" as an identifier
#' of each simulation run.
#' @param obs_data A data.frame containing observation dataset. Rows correspond
#' to observations and columns correspond to variables.
#' @param sim_nr An integer indicating the number of simulations.
#' @param var A character vector containing multiple characters. The
#' vector represents the variables of interest for generating qqplots. If
#' set to NULL, every variable is included.
#' @param pick_color A vector with two strings specifying the colors to represent the
#' simulaion and observation datasets.
#'
#' @return A ggplot object of density curves for variables.
#'
#' @export
#'
#' @examples
#' plot <- plot_1dist(
#'     sim_data = pediatric_sim,
#'     obs_data = pediatric_3cov,
#'     sim_nr = 100,
#'     pick_color = c("#3ABAC1","#969696"),
#'     var = NULL
#'     )
#'
plot_1dist <- function(sim_data, obs_data, sim_nr = NULL, var = NULL, pick_color = c("#3ABAC1","#969696")) {

  # determine the variables of interest ----
  if (is.null(var)) {
    var <- colnames(obs_data)
  } else if (!all( var %in% colnames(obs_data))) {
    #check for names not matching between obs_data and var
    stop("Variable names in var differ from names in obs_data")
  }

  # filter out the non-numeric varibales in dataframe
  is_numeric <- sapply(obs_data |> dplyr::select(all_of(var)), is.numeric)
  if (any(!is_numeric)) {
    stop("Var contains non-numeric columns.")
  }

  # check if the simulation_nr exists in sim_data
  if (!("simulation_nr" %in% colnames(sim_data))) {
    stop("simulation_nr does not exist in sim_data")
  }


  long_sim <- reshape2::melt(sim_data, id.vars = "simulation_nr") |>
    dplyr::mutate(type = "Virtual population")

  long_obs <- reshape2::melt(obs_data) |>
    dplyr::mutate(simulation_nr = 101,
           type = "Observed population")

  all_line <- rbind(long_sim, long_obs)

  all_line$type <- factor(all_line$type, levels = c("Virtual population","Observed population"))

  density_curve_1D <- ggplot2::ggplot(all_line) +
    ggplot2::geom_density(aes(value, color = factor(type, levels = c("Virtual population","Observed population")) , fill = factor(simulation_nr), linewidth = factor(type)),alpha = 1/10) +
    ggplot2::scale_fill_manual(name = NULL, values=c(rep("white",100),"white"),limits = force) +
    ggplot2::scale_colour_manual(name="Data type", values=c(pick_color[1], pick_color[2]), breaks= c("Virtual population","Observed population"), limits = force) +
    ggplot2::scale_linewidth_manual(name = NULL, values = c(0.3,0.8)) +
    ggplot2::facet_wrap(variable~.,scales="free",nrow = floor(sqrt(length(var)))) +
    ggplot2::labs(x = "", y = "Density") +
    ggplot2::guides(fill = "none", linewidth = "none") +
    # guides(color = guide_legend(byrow = TRUE)) +
    ggplot2::guides(color = "none", linewidth = "none") +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "white"), panel.grid.minor.x = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_blank(),
          legend.title = ggplot2::element_blank(),
          # legend.position = c(.55, .10),
          legend.direction = "vertical",
          legend.spacing.y = ggplot2::unit(0.5, 'cm'),
          legend.box.just = "right",
          legend.margin = ggplot2::margin(6, 6, 6, 6),
          strip.text.x = ggplot2::element_text(size=6.5),
          axis.text.x = ggplot2::element_text(size=5.5),
          axis.title = ggplot2::element_text(size=10))

  return(density_curve_1D)

}
