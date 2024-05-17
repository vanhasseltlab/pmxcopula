# library(tibble)
# library(patchwork)
# library(dplyr)
# library(ggplot2)


#' Create qqplots from simulation data and observed data.
#'
#' @param sim_data A data.frame containing simulation dataset. Rows correspond
#' to observations and columns correspond to covariate variables. sim_data
#' is supposed to include a column named "simulation_nr" as an identifier
#' of each simulation run.
#' @param obs_data A data.frame containing observation dataset. Rows correspond
#' to observations and columns correspond to variables.
#' @param sim_nr An integer indicating the number of simulations.
#' @param conf_band A numeric value indicating the empirical confidence level
#' for the width of the bands; e.g., 95 indicates 95% confidence interval.
#' @param var A character vector containing multiple characters. The
#' vector represents the variables of interest for generating qqplots. If
#' set to NULL, every covariate is included.
#' @param type A character of plotting types. Either "ribbon" or"point".
#' "ribbon" corresponds to visualizing the confidence level defined in "conf_band" as shaded area,
#' and "point" corresponds to visualizing all the simulation quantiles as individual points.
#'
#' @return A list containing ggplot objects of qqplots for covariates.
#' @export
#'
#' @examples
get_qqplot <- function(sim_data, obs_data, sim_nr = NULL, conf_band = 95, var = NULL, type = "ribbon") {

  # determine the variables of interest ----
  if (is.null(var)) {
    var <- colnames(obs_data)
  } else if (!all( var %in% colnames(obs_data))) {
    #check for names not matching between obs_data and var
    stop("Covariate names in var differ from names in obs_data")
  }

  # check if the simulation_nr exists in sim_data
  if (!("simulation_nr" %in% colnames(sim_data))) {
    stop("simulation_nr does not exist in sim_data")
  }

  # preparation of constants ----
  quant = seq(from = 0.01, to = 0.99, by = 0.01) # for qqplot, all the quantiles
  a  =  0.5* (100-conf_band)*0.01 # lower quantile for the summary statistics
  b = 1- a # upper quantile for the summary statistics

  quant_sims_sum <- list()
  qqplots <- list()

  # calculate the quantiles for each varibale in var
  for (c in 1 : length(var)) {

      ## calculation for observation quantiles ----
      obs_subset <- obs_data[[var[c]]]
      observed_quantiles <- quantile(as.vector(obs_subset), probs = quant)
      quant_obs <- cbind(as.data.frame(observed_quantiles), quant = quant)

      ## calculation for simulation ----
      sim_subset <- sim_data %>%
        select(var[c], simulation_nr)
      quant_sims <- list()

          ### calculate the quantiles statistics for each simulation run ----
          for (i in 1 : sim_nr){
            #### extract the ith simulation run ----
            marginal_data <- sim_subset %>%
              filter(simulation_nr == i)
            simulation_quantiles <- quantile(marginal_data[, var[c]], probs = quant)
            quant_sim <- cbind(as.data.frame(simulation_quantiles), quant = quant, simulation_nr = i)
            quant_sims[[i]] <- quant_sim
          }

          ## summary calculation of multiple simulations ----
          combi_quant <- do.call(rbind,quant_sims)
          sum_quant <- combi_quant %>%
            group_by(quant) %>%
            summarize(median_q = median(simulation_quantiles),
                      lower_q = quantile(simulation_quantiles, probs = a),
                      upper_q = quantile(simulation_quantiles, probs = b))

      ## plotting ----
      plot_dat <- merge(sum_quant, quant_obs, by = "quant")
      qqplot <- ggplot(plot_dat, aes(x = observed_quantiles))+
        geom_line(aes(y = median_q), color = "blue") +  # Line plot
        geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
        labs(x = "observed quantiles", y = "simulated quantiles", title = var[c]) +  # Labels and title
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5),
              aspect.ratio = 1)

      if (type == "ribbon") {
        qqplot <- qqplot +
          geom_ribbon(data = plot_dat, aes(x = observed_quantiles, ymin = lower_q, ymax = upper_q), fill = "lightblue", alpha = 0.5)   # Shaded area
      } else if (type == "point") {

        all_quant <- merge(quant_obs,combi_quant,by = "quant")
        plot_dat <- merge(all_quant, sum_quant, by = "quant")

        qqplot <- qqplot +
          geom_point(data = plot_dat, aes(x = observed_quantiles, y = simulation_quantiles, color = "lightblue", size = 1), alpha = 0.01) +# 1/sim_nr
          theme(legend.position = "none")
      }

      quant_sims_sum[[c]] <- sum_quant # not sure if it is necessary to keep
      qqplots[[var[c]]] <- qqplot

  }

  return(qqplots)
}


#' Output qqplots from simulation data and observed data.
#'
#' @param sim_data A data.frame containing simulation dataset. Rows correspond
#' to observations and columns correspond to covariate variables. sim_data
#' is supposed to include a column named "simulation_nr" as an identifier
#' of each simulation run.
#' @param obs_data A data.frame containing observation dataset. Rows correspond
#' to observations and columns correspond to variables.
#' @param sim_nr An integer indicating the number of simulations.
#' @param conf_band A numeric value indicating the empirical confidence level
#' for the width of the bands; e.g., 95 indicates 95% confidence interval.
#' @param var A character vector containing multiple characters. The
#' vector represents the variables of interest for generating qqplots. If
#' set to NULL, every covariate is included.
#' @param type A character of plotting types. Either "ribbon" or"point".
#' "ribbon" corresponds to visualizing the confidence level defined in "conf_band" as shaded area,
#' and "point" corresponds to visualizing all the simulation quantiles as individual points.
#'
#' @return A patchwork object of qqplots for covariates.
#' @export
#'
#' @examples
plot_qq <- function(sim_data, obs_data, sim_nr = NULL, conf_band = 95, var = NULL, type = "ribbon") {

  # determine the variables of interest ----
  if (is.null(var)) {
    var <- colnames(obs_data)
  } else if (!all( var %in% colnames(obs_data))) {
    #check for names not matching between obs_data and var
    stop("Covariate names in var differ from names in obs_data")
  }

  # check if the simulation_nr exists in sim_data
  if (!("simulation_nr" %in% colnames(sim_data))) {
    stop("simulation_nr does not exist in sim_data")
  }

  ## generate plotting data ----
  plot_dat <- get_qqplot(sim_data = sim_data,
                         obs_data = obs_data,
                         sim_nr = sim_nr,
                         conf_band = conf_band,
                         var = var,
                         type = type)

  plot_qq <- wrap_plots(plot_dat, nrow = floor(sqrt(length(var))))

  return(plot_qq)

}


#### boxes version ####
# test with box-version qqplot
# test_qq <- get_qqplot(sim_data = sim_example,
#                       obs_data = df_example,
#                       sim_nr = 100,
#                       conf_band = 95,
#                       var = NULL,
#                       percentiles = c(10, 50, 90),
#                       colors_bands = c("#99E0DC", "#E498B4"))
#
# get_qqplot <- function(sim_data,
#                        obs_data,
#                        sim_nr = NULL,
#                        conf_band = 95,
#                        var = NULL,
#                        percentiles = c(10, 50, 90),
#                        colors_bands = c("#99E0DC", "#E498B4")) {
#
#   # determine the variables of interest ----
#   if (is.null(var)) {
#     var <- colnames(obs_data)
#   } else if (!all( var %in% colnames(obs_data))) {
#     #check for names not matching between obs_data and var
#     stop("Covariate names in var differ from names in obs_data")
#   }
#
#   # check if the simulation_nr exists in sim_data
#   if (!("simulation_nr" %in% colnames(sim_data))) {
#     stop("simulation_nr does not exist in sim_data")
#   }
#
#   # preparation of constants ----
#   quant = seq(from = 0.01, to = 0.99, by = 0.01) # for qqplot, all the quantiles
#   a  =  0.5* (100-conf_band)*0.01 # lower quantile for the summary statistics
#   b = 1- a # upper quantile for the summary statistics
#
#   quant_sims_sum <- list()
#   qqplots <- list()
#
#   box_width <- 0
#   # calculate the quantiles for each varibale in var
#   for (c in 1 : length(var)) {
#
#     ## calculation for observation quantiles ----
#     obs_subset <- obs_data[[var[c]]]
#     observed_quantiles <- quantile(as.vector(obs_subset), probs = quant)
#     quant_obs <- cbind(as.data.frame(observed_quantiles), quant = quant)
#
#     # Define the width of the boxes
#     box_width <- 4 * diff(range(observed_quantiles)) / 99   # Adjust the scaling factor as needed
#
#     ## calculation for simulation ----
#     sim_subset <- sim_data %>%
#       select(var[c], simulation_nr)
#     quant_sims <- list()
#
#     ### calculate the quantiles statistics for each simulation run ----
#     for (i in 1 : sim_nr){
#       #### extract the ith simulation run ----
#       marginal_data <- sim_subset %>%
#         filter(simulation_nr == i)
#       simulation_quantiles <- quantile(marginal_data[, var[c]], probs = quant)
#       quant_sim <- cbind(as.data.frame(simulation_quantiles), quant = quant, simulation_nr = i)
#       quant_sims[[i]] <- quant_sim
#     }
#
#     ## summary calculation of multiple simulations ----
#     combi_quant <- do.call(rbind,quant_sims)
#     sum_quant <- combi_quant %>%
#       group_by(quant) %>%
#       summarize(median_q = median(simulation_quantiles),
#                 lower_q = quantile(simulation_quantiles, probs = a),
#                 upper_q = quantile(simulation_quantiles, probs = b))
#
#
#     plot_dat <- merge(quant_obs,sum_quant,by = "quant")
#
#     percent_to_match <- 0.01 * percentiles
#
#     plot_dat_sub <- plot_dat %>%
#       filter(as.character(quant) %in% percent_to_match)
#
#     # add a color to each quant
#     color_table <- cbind(plot_dat_sub,
#                          color = c(colors_bands[2],colors_bands[1],colors_bands[2]),
#                          box_width = box_width) %>%
#       as.data.frame()
#
#     ## plotting ----
#     qqplot <- ggplot(plot_dat, aes(x = observed_quantiles))+
#       geom_line(aes(y = median_q), color = "blue") +  # Line plot
#       geom_ribbon(aes(ymin = lower_q, ymax = upper_q), fill = "lightblue", alpha = 0.5) +  # Shaded area
#       geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
#       geom_rect(data = color_table,
#                  aes(xmin = observed_quantiles - box_width,
#                      xmax = observed_quantiles + box_width,
#                      ymin = lower_q,
#                      ymax = upper_q,
#                      fill = color),
#                 alpha = 0.8) +  # boxes
#       labs(x = "observed quantiles", y = "simulated quantiles", title = var[c]) +  # Labels and title
#       guides(fill = "none") +
#       theme_bw() +
#       theme(plot.title = element_text(hjust = 0.5),
#             aspect.ratio = 1)
#
#     qqplot
#
#     qqplots[[var[c]]] <- qqplot
#
#   }
#
#   return(qqplots)
# }
#
#
# # test
# test_qqplot <- plot_qq(sim_data = sim_example,
#                        obs_data = df_example,
#                        sim_nr = 100,
#                        conf_band = 95,
#                        var = NULL,
#                        percentiles = c(10, 50, 90),
#                        colors_bands = c("#99E0DC", "#E498B4"))
# # A warpper function to output the qqplots only ----
# plot_qq <- function(sim_data, obs_data, sim_nr = NULL, conf_band = 95, var = NULL, percentiles = c(10, 50, 90), colors_bands = c("#99E0DC", "#E498B4")) {
#
#   # determine the variables of interest ----
#   if (is.null(var)) {
#     var <- colnames(obs_data)
#   } else if (!all( var %in% colnames(obs_data))) {
#     #check for names not matching between obs_data and var
#     stop("Covariate names in var differ from names in obs_data")
#   }
#
#   # check if the simulation_nr exists in sim_data
#   if (!("simulation_nr" %in% colnames(sim_data))) {
#     stop("simulation_nr does not exist in sim_data")
#   }
#
#   ## generate plotting data ----
#   plot_dat <- get_qqplot(sim_data = sim_data,
#                          obs_data = obs_data,
#                          sim_nr = sim_nr,
#                          conf_band = conf_band,
#                          var = var,
#                          percentiles = c(10, 50, 90),
#                          colors_bands = c("#99E0DC", "#E498B4"))
#
#   plot_qq <- wrap_plots(plot_dat, nrow = floor(sqrt(length(var))))
#
#   return(plot_qq)
#
# }
