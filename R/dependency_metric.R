#' Calculate Overlap Percentage Of Simulation Data And Observed Data
#'
#' @param sim_data A data.frame containing simulation dataset. Rows correspond to observations and columns correspond to variables.
#' @param obs_data A data.frame containing observation dataset. Rows correspond to observations and columns correspond to variables.
#' @param pairs_matrix Matrix with 2 column and each row containing a pair of
#' variable names. If set to NULL, every possible variable
#' pair is included.
#' @param percentile A numeric value representing the
#' percentile of the density contour for each pair combination of variables;
#' e.g., 90 represents that the overlap percentage is calculated for 90th percentile contours.
#' @param sim_nr An integer indicating the number of simulations. If not set to 1, sim_data
#' is supposed to include a column named "simulation_nr" as an identifier of each simulation run.
#' @param summarize A logical value to indicate whether to summarize the overlap
#' percentage across different simulations using mean and standard deviation.
#'
#' @return A data.frame containing the overlap percentage of variable pair(s).
#'
#' @export
#'
#' @examples
#' overlap <- calc_overlap(
#'     sim_data = pediatric_sim,
#'     obs_data = pediatric_3cov,
#'     pairs_matrix = matrix(c("AGE" ,"BW" ,"BW" ,"CREA" ), nrow = 2, ncol = 2),
#'     percentile = 95,
#'     sim_nr = 100,
#'     summarize = FALSE)
#'
calc_overlap <- function(sim_data, obs_data, pairs_matrix = NULL, percentile, sim_nr, summarize = FALSE) {

  # V1 <- V2 <- No <- statistic <- var_pair <- ovlp <- Var1 <- Var2 <- Sim_N <- sd <- NULL

  if (is.null(pairs_matrix)) {
    pairs_matrix <- t(combinat::combn(colnames(obs_data), 2))
  }


  pair_data <- pairs_matrix |> as.data.frame() |>
    dplyr::mutate(No = c(1:nrow(pairs_matrix)), # No serves as the indicator of variable pair
                  var_pair = paste0(V1, "-", V2))
  ovlp_data <- pair_data |>
    dplyr::slice(rep(dplyr::row_number(), sim_nr)) |>
    dplyr::mutate(Sim_N = rep(1:sim_nr, each = nrow(pair_data)),
                  ovlp = NA)


  # calculate the contours of observation contours
  contour_obs_list <- list()
  for (i in 1 : nrow(pair_data)){
    odata <- obs_data |> dplyr::select(pair_data[i,1],pair_data[i,2]) |> na.omit() # extract data
    kd_obs <- ks::kde(odata, compute.cont=TRUE) # estimate kernel density
    contour_obs <- with(kd_obs, grDevices::contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                             z=estimate, levels=cont[paste0(100-percentile,"%")]))
    # to make sure it's closed polygon
    for(b in 1: length(contour_obs)){
      contour_obs[[b]] <- as.data.frame(contour_obs[[b]])
      contour_obs[[b]][nrow(contour_obs[[b]])+1,] <- c(contour_obs[[b]][1,1], contour_obs[[b]][1,2], contour_obs[[b]][1,3])
    }

    contour_obs_long <- dplyr::bind_rows(contour_obs,.id="circle")
    contour_obs_list[[i]] <- create_polygon(contour_obs_long)
  }

  # calculate the contours of simulation contours
  contour_sim_list <- list()

  for (b in 1 : sim_nr){
    cat("\r", "Calculate overlap of simulation:", b, "/", sim_nr)
    for (i in 1 : nrow(pair_data))
      {
      sdata <- sim_data[sim_data$simulation_nr == b,] |> dplyr::select(pair_data[i,1],pair_data[i,2]) |> na.omit()
      kd_sim <- ks::kde(sdata, compute.cont=TRUE)
      contour_sim <- with(kd_sim, grDevices::contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                               z=estimate, levels=cont[paste0(100-percentile,"%")]))

      for(m in 1: length(contour_sim)){
        contour_sim[[m]] <- as.data.frame(contour_sim[[m]])
        contour_sim[[m]][nrow(contour_sim[[m]])+1,] <- c(contour_sim[[m]][1,1], contour_sim[[m]][1,2], contour_sim[[m]][1,3])
      }
      contour_sim_long <- dplyr::bind_rows(contour_sim,.id="circle")
      contour_sim_list[[paste0("Sim",b)]][[paste0(pair_data$var_pair[pair_data$No == i])]] <- create_polygon(contour_sim_long)

      union_area <- sf::st_union(contour_obs_list[[i]],contour_sim_list[[b]][[i]])
      inter_area <- sf::st_intersection(contour_obs_list[[i]],contour_sim_list[[b]][[i]])

      Areaunion = sf::st_area(union_area)
      AreaIntersec = sf::st_area(inter_area)

      ovlp_data$ovlp[ovlp_data$Sim_N == b & ovlp_data$No == i] <- 100 * AreaIntersec/Areaunion

    }

  }

  overlap <- ovlp_data |>
    dplyr::mutate(statistic = "overlap") |>
    dplyr::select(-No) |>
    dplyr::mutate(Var1 = as.character(V1), Var2 = as.character(V2)) |>
    dplyr::select(statistic, var_pair, ovlp, Var1, Var2, Sim_N)

  if (sim_nr == 1) {
    return(overlap)
  } else {

    if (summarize == FALSE){
      return(overlap)
    } else {

     overlap |> dplyr::group_by(var_pair) |> dplyr::summarise(mean = mean(ovlp), SD = sd(ovlp))
      return(overlap)
    }


  }

}



#' Compare Dependency Metric Between Simulation Data And Observed Data
#'
#' @param sim_data A data.frame containing simulation dataset. Rows correspond
#' to observations and columns correspond to variables. sim_data
#' is supposed to include a column named "simulation_nr" as an identifier
#' of each simulation run.
#' @param obs_data A data.frame containing observation dataset. Rows correspond
#' to observations and columns correspond to variables.
#' @param pairs_matrix Matrix with 2 column and each row containing a pair of
#' variable names. If set to NULL, every possible variable
#' pair is included.
#' @param percentile A numeric value representing the
#' percentile of the density contour for each pair combination of variables;
#' e.g., 90 represents that the overlap percentage is calculated for 90th percentile contours.
#' @param sim_nr An integer indicating the number of simulations.
#'
#' @return A data.frame containing the dependency metrics calculated for simulation data and observed data.
#'
#' @export
#'
#' @examples
#' mtr_dependency <- calc_dependency(
#'     sim_data = pediatric_sim,
#'     obs_data = pediatric_3cov,
#'     pairs_matrix = NULL,
#'     percentile = 95,
#'     sim_nr = 100
#'     )
#'
calc_dependency <- function(sim_data, obs_data,
                              pairs_matrix = NULL,
                              percentile,
                              sim_nr) {
  ovlp <- NULL
  if (is.null(pairs_matrix)) {
    pairs_matrix <- t(combinat::combn(colnames(obs_data), 2))
  }


  overlap <- calc_overlap(sim_data = sim_data,
                         obs_data = obs_data,
                         percentile = percentile,
                         sim_nr = sim_nr,
                         pairs_matrix = pairs_matrix,
                         summarize = FALSE) |>
    dplyr::rename(value = ovlp) |>
    dplyr::mutate(observed = 1)

  cat("\r", "Calculation of dependency metric completed.","\n")

  cor <- as.data.frame(matrix(nrow = sim_nr * nrow(pairs_matrix), ncol = 5))
  colnames(cor) <- c("statistic", "Var1", "Var2","var_pair", "cor")
  cor$Sim_N <- rep(1:sim_nr, each = nrow(pairs_matrix))

  for(i in unique(sim_data$simulation_nr)) {
    correlation <- calc_correlation(data = sim_data[sim_data$simulation_nr == i, ],
                           pairs_matrix = pairs_matrix)

    cor[cor$Sim_N == i, c("statistic", "Var1", "Var2", "var_pair", "cor")] <-  correlation
  }


  cor_compare <- cor |>
    dplyr::mutate(cor = ifelse(abs(cor) == Inf, NA, cor)) |>
    # left_join(calc_correlation(obs_data, pairs_matrix = pairs_matrix) |> rename(observed = cor)) |>
    dplyr::rename(value = cor)

  cor_compare <- suppressMessages(dplyr::left_join(cor_compare,calc_correlation(obs_data, pairs_matrix = pairs_matrix) |> dplyr::rename(observed = cor)))

  cat("\r", "Calculation of correlation completed.","\n")

  dependency <- rbind(cor_compare, overlap)

  return(dependency)
}
