
#' Visualize And Compare Dependency Structures Of Simulation Data And Observed Data
#'
#' @param sim_data A data.frame containing simulation dataset.
#' @param obs_data A data.frame containing observation dataset.
#' @param variables A character vector containing multiple characters. The
#' vector represents the variables of interest for plotting.
#' If set to NULL, every variable is included.
#' @param sim_nr An integer indicating the simulation data to use. One simulation
#' generates the same number of individuals as the observation dataset. If there are
#' multiple simulations or a column named "simulation_nr" in the simulation
#' dataset, select one specific simulation to visualize the distribution; if such column does not exist,
#' the default value of 1 will be used to visualize the entire silumation dataset.
#' @param title Title of the figure.
#' @param plot_type A character vector of plotting types. Either "density",
#' "points", or "both".
#' @param pick_color A vector with two strings specifying the colors to represent the
#' simulaion and observation datasets.
#' @param full_plot A logical value to indicate whether to plot multiple figures
#' on one page.
#' @param caption A logical value to indicate define the size of caption.
#'
#' @return A graphical object.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_density2d theme_bw theme geom_boxplot scale_y_continuous scale_x_continuous scale_fill_manual
#' @examples
#' plot <- plot_mvdist(
#'     sim_data = pediatric_sim,
#'     obs_data = pediatric_3cov,
#'     sim_nr = 1,
#'     pick_color = c("#3ABAC1","#969696"),
#'     plot_type = "density"
#'     )
#'
plot_mvdist <- function(sim_data, obs_data, variables = NULL,
                        sim_nr = 1, title = NULL, plot_type = "points",
                        pick_color = c("#3ABAC1","#969696"), full_plot = TRUE,
                        caption = NULL,
                        return_grob = FALSE) {

  if ("simulation_nr" %in% colnames(sim_data)) {
    # Use only 1 simulation for plotting
    sim_data <- sim_data |>
      dplyr::filter(simulation_nr %in% sim_nr) |>
      dplyr::select(-simulation_nr)
  }

  #Combine truth and simulation results
  total_data <- obs_data |> dplyr::mutate(type = "observed") |>
    dplyr::bind_rows(sim_data |> dplyr::mutate(type = "simulated"))

  total_data$type <- factor(total_data$type, levels = c("simulated", "observed"))
  names(pick_color) <- NULL


  if (is.null(variables)) {
    variables <- setdiff(colnames(sim_data), "simulation_nr")
  }

  point_plots <- density_plots <- list()
  combination_variables <- t(combinat::combn(variables, 2))
  for (i in 1:nrow(combination_variables)) {

    combination <- paste(combination_variables[i, ], collapse = "_")
    part_data <- total_data[, c(combination_variables[i, ], "type")] |>
      dplyr::filter(rowSums(is.na(total_data[, combination_variables[i, ]])) == 0)

    if (plot_type != "density") {

      data_s <- part_data[part_data$type == "simulated", ]
      data_o <- part_data[part_data$type == "observed", ]
      point_plots[[combination]] <- ggplot2::ggplot(mapping = ggplot2::aes(.data[[combination_variables[i, 1]]], .data[[combination_variables[i, 2]]])) +
        ggplot2::geom_point(data = data_s, alpha = 0.4, shape = 16, color = pick_color[1]) +
        ggplot2::geom_point(data = data_o, alpha = 0.8, shape = 16, color = pick_color[2]) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "none")
    }
    if (plot_type != "points") {
      data_s <- part_data[part_data$type == "simulated", ]
      data_o <- part_data[part_data$type == "observed", ]

      density_plots[[combination]] <- ggplot2::ggplot(mapping = ggplot2::aes(.data[[combination_variables[i, 1]]], .data[[combination_variables[i, 2]]])) +
        ggplot2::geom_density2d(data = data_s, color = pick_color[1], bins = 20) +
        ggplot2::geom_density2d(data = data_o,  alpha = 1, color = pick_color[2], linetype = 2, bins = 20) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0))) +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0))) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "none")
    }
  }

  if(!full_plot) {
    if (plot_type == "density") {
      return(density_plots)
    }
    if (plot_type == "points") {
      return(point_plots)
    }
    else if (plot_type == "both") {
      names(density_plots) <- paste0("density_",names(density_plots))
      names(point_plots) <- paste0("point_", names(point_plots))
      return(c(density_plots, point_plots))
    }
  }


  univariate_plots <- list()
  for (i in variables) {
    univariate_plots[[i]] <- total_data |>
      ggplot2::ggplot(ggplot2::aes(y = .data[[i]], x = type, fill = type)) +
      ggplot2::geom_boxplot() +
      ggplot2::scale_fill_manual(values = c(pick_color[1], pick_color[2])) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none")
  }
  nr_cross_plots <- choose(length(variables), 2)
  layout_mat <- matrix(NA, nrow = length(variables), ncol = length(variables))
  layout_mat[lower.tri(layout_mat)] <- 1:nr_cross_plots
  diag(layout_mat) <- nr_cross_plots + (1:length(variables))



  if (plot_type == "points") {
    list_plots <- c(point_plots, univariate_plots)
  } else if (plot_type == "density") {
    list_plots <- c(density_plots, univariate_plots)
  } else if (plot_type == "both") {
    t_layout_mat <- t(layout_mat)
    t_layout_mat[lower.tri(t_layout_mat)] <- nr_cross_plots + length(variables) + (1:nr_cross_plots)
    layout_mat <- t(t_layout_mat)
    list_plots <- c(density_plots, univariate_plots, point_plots)
  }

  # determine the output
  if (return_grob != FALSE) {
    gridExtra::grid.arrange(grobs = list_plots, layout_matrix = layout_mat, top = title, bottom = caption)
  } else {
    mvdist <- gridExtra::grid.arrange(grobs = list_plots, layout_matrix = layout_mat, top = title, bottom = caption)
    return(mvdist)
  }


}
