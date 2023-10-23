#VPC donut simulation function, geom_creation function and plot method

#depends on:
# dplyr
# ggplot
# ks
# sf


#' Restructure calculated contours in data.frame.
#'
#' Output from contourLines is converted from a list to a data.frame for
#' further use in other functions.
#'
#' @param contour_list Output of the contourLines function, a list with contour(s)
#' of different density levels of interest.
#' @param contour_levels A numeric vector with density levels at which the
#' percentiles are calculated.
#' @param sim_nr An integer identifying the simulation for which contours are calculated.
#' @param pair A vector with two strings indicating the names of the covariates.
#'
#' @return A data.frame containing the coordinates of contours, percentiles, density levels,
#' covariate pairs and simulation identifiers.
#'

extract_contour_df <- function(contour_list, contour_levels, sim_nr = NULL, pair = NULL) {
  cont_df <- NULL
  for (i in 1:length(contour_list)) {
    cont_df <- rbind.data.frame(cont_df,
                                contour_list[[i]] %>% as.data.frame() %>%
                                  dplyr::mutate(circle = i, percentile = names(contour_levels[contour_levels == level[1]])))
  }
  if (!is.null(sim_nr)) {
    cont_df <- cont_df %>% dplyr::mutate(sim_nr = sim_nr)
  }
  if (!is.null(pair)) {
    cont_df <- cont_df %>% dplyr::mutate(var1 = pair[1], var2 = pair[2])
  }

  return(cont_df)
}

#dependency function: create_polygon
#' Extract the polygons from contour calculations.
#'
#' Create a sf polygon object from (multiple) contour(s)
#'
#'
#' @param ci_data Output from extract_contour_df function, containing the
#' coordinates of contours, percentiles, density levels, covariate pairs and
#' simulation identifiers.
#'
#' @return A sf polygon object with the polygon(s) of a contour.
#'

create_polygon <- function(ci_data) {
  polygon_list <- list()
  for (cr in unique(ci_data$circle)) {
    dat_matrix <- list(ci_data %>% dplyr::filter(circle == cr) %>% select(x, y) %>% as.matrix)
    polygon_list[[as.character(cr)]] <- sf::st_polygon(dat_matrix)
  }

  if (length(polygon_list) >= 2) {
    p_union <- polygon_list[[1]]
    for (cr in 2:length(polygon_list)) {
      p_union <- sf::st_union(p_union, polygon_list[[cr]])
    }

    polygon_pairs <- t(combn(1:length(polygon_list), 2))
    p_intersect <- list()
    for (cr in 1:nrow(polygon_pairs)) {
      p_intersect[[cr]] <- sf::st_intersection(polygon_list[[polygon_pairs[cr, 1]]], polygon_list[[polygon_pairs[cr, 2]]])
    }

    if (length(p_intersect) > 1){
      p_intersect_full <- p_intersect[[1]]
      for (cr in 2:length(p_intersect)) {
        p_intersect_full <- sf::st_union(p_intersect_full, p_intersect[[cr]])
      }
    } else {
      p_intersect_full <- p_intersect[[1]]
    }

    p_full <- sf::st_difference(p_union, p_intersect_full)

  } else {
    p_full <- polygon_list[[1]]
  }

  return(p_full)
}


#' Get contours from simulated data.
#'
#' Simulate or add simulated values from multiple simulations to compute
#' the contours for a donut VPC
#'
#' @param vine An object of class "vine_dist" or "vine", from rvinecopulib
#' package.
#' @param percentiles (A vector of) Numeric value(s) representing the
#' percentiles of the density distribution that appear in the VPC donut plot;
#' e.g., c(10, 50, 90) represents 10th, 50th and 90th percentiles.
#' @param B An integer indicating the number of simulations.
#' @param pairs_matrix Matrix with 2 column and each row containing a pair of
#' covariate names from the copula. If set to NULL, every possible covariate
#' pair is included.
#' @param nobs Number of observations for each simulation, if set to NULL, the
#' number of observations from the original data will be picked.
#'
#' @return A data.frame containing contours over all percentiles from the
#' different simulations .
#' @export
#'
#' @examples
#' # specify pair-copulas
#' bicop <- bicop_dist("bb1", 90, c(3, 2))
#' pcs <- list(
#'   list(bicop, bicop), # pair-copulas in first tree
#'   list(bicop) # pair-copulas in second tree
#' )
#' # specify R-vine matrix
#' mat <- matrix(c(1, 2, 3, 1, 2, 0, 1, 0, 0), 3, 3)
#' # set up vine copula model
#' vc <- vine_dist(list(distr = "norm"), pcs, mat)
#'
#' sim_contour_data <- simulate_contours(vc, c(10, 50, 90), B = 10, nobs = 10)
#'
simulate_contours <- function(vine, percentiles, B, pairs_matrix = NULL, nobs = NULL) {
  sim_contours_list <- list()

  if (is.null(vine$names)) {
    vine$names <- paste0("V", 1:vine$copula$structure$d) #  rename covariate names into e.g. "V1", "V2" and "V3"
  }

  if (is.numeric(nobs)) {
    vine$nobs <- nobs
  } else if (is.null(vine$nobs)) {
    stop("nobs should be specified if the vine is not estimated from data",
         " and does not contain nobs in the vine object (vine$nobs)")
  }

  if (is.null(pairs_matrix)) {
    pairs_matrix <- t(combn(vine$names, 2))
  } else if (!all(c(pairs_matrix) %in% c(vine$names))) {
    #check for names not matching between vine and pairs_matrix
    stop("Covariate names in pairs_matrix differ from names in vine")
  }

  #simulate from vine
  full_sim_data <- as.data.frame(rvine(vine$nobs*B, vine))
  full_sim_data$b <- rep(1:B, each = vine$nobs) # add label for each run of simulation

  #calculate contours for every percentile and every covariate combination
  i <- 1
  for (b in 1:B) {
    cat("\r", "Calculate contours simulation:", b, "/", B)
    sim_data_b <- full_sim_data[full_sim_data$b == b, ]
    for (p in 1:nrow(pairs_matrix)) {
      #use ks for density computation
      kd_sim <- ks::kde(sim_data_b[, pairs_matrix[p, ]], compute.cont = TRUE, approx.cont = FALSE)
      contour_sim <- with(kd_sim, grDevices::contourLines(x = eval.points[[1]], y = eval.points[[2]],
                                               z = estimate, levels = cont[paste0(100-percentiles, "%")]))
      #extract information
      sim_contours_list[[i]] <- extract_contour_df(contour_sim, kd_sim$cont, b, pairs_matrix[p, ])
      i <- i + 1

    }

  }
  cat("\n")
  sim_contours <- dplyr::bind_rows(sim_contours_list)

  return(sim_contours)
}




#' Create ggplot layers for contours.
#'
#' Create the ggplot contour layers for a donut VPC
#'
#' @param sim_contours Output data.frame from the simulate_contours function.
#' @param conf_band A numeric value indicating the empirical confidence level for the width of the bands; e.g., 95 indicates 95\% confidence interval.
#' @param colors_bands A vector with two strings specifying the colors of the confidence bands.
#' @param return_polygons A logical value to indicate whether return to a (list of) sf polygon object or a list containing the ggplot layers for a donut VPC.
#'
#' @return A list containing the ggplot layers for a donut VPC or a (list of) sf polygon object.
#' @export
#'

create_geom_donutVPC <- function(sim_contours, conf_band = 95, colors_bands = c("#99E0DC", "#E498B4"), return_polygons = FALSE) {

  # percentiles <- as.numeric(gsub("%", "", unique(sim_contours$percentile), fixed = T))
  percentiles <- as.numeric(gsub("%", "", sort(unique(sim_contours$percentile)), fixed = T))
  pairs_matrix <- unique(sim_contours[, c("var1", "var2")])

  colors_bands <- colors_bands[((1:length(percentiles))/2 == round((1:length(percentiles))/2)) + 1] # colors were assigned to the bands depending on the odd or even order of percentiles
  names(colors_bands) <- percentiles

  contour_geoms <- list()
  conf_geom_data <- list()
  for (p in 1:nrow(pairs_matrix)) {
    var_pair <- paste0(pairs_matrix[p, 1], "-", pairs_matrix[p, 2])

    contour_geoms[[var_pair]] <- list()
    for (pr in percentiles) {
      sim_full_df <- sim_contours %>%
        dplyr::filter(var1 == pairs_matrix[p, 1], var2 == pairs_matrix[p, 2]) %>%
        dplyr::filter(percentile == paste0(pr, "%"))
      kd_sim_full <- ks::kde(sim_full_df[, c("x", "y")], compute.cont = TRUE, approx.cont = FALSE)
      # new contour was calculated based the coordinates of the previously calculated contours
      # different countours at the same density level will be merged
      contour_sim_full <- with(kd_sim_full, grDevices::contourLines(x = eval.points[[1]], y = eval.points[[2]],
                                                         z = estimate, levels = cont[paste0(100-conf_band, "%")]))
      contour_data <- extract_contour_df(contour_sim_full, kd_sim_full$cont, pr, pairs_matrix[p, ])

      conf_geom_data[[paste0(var_pair, ": ", pr, "%")]] <- contour_data %>%
        create_polygon()

      contour_geoms[[var_pair]][[paste0(pr, "%")]] <- ggplot2::geom_sf(data = conf_geom_data[[paste0(var_pair, ": ", pr, "%")]],
                                                              color = colors_bands[as.character(pr)], fill = colors_bands[as.character(pr)])
    }

  }

  if (return_polygons) {
    return(conf_geom_data)
  } else {
    return(contour_geoms)
  }
}


#' Create ggplot(s) with geom_vpc.
#'
#' @param geom_vpc Output list from the simulate_contours function, containing
#' the ggplot layers for a donut VPC.
#' @param obs_data A data.frame containing observation dataset.
#' @param save_path A string with path to save the resulting simulations as an R
#' object; if set to NULL, the object will be saved in the current directory.
#'
#' @return ggplot objects of donut VPC.
#' @export
#'
#work in progress
ggVPC_donut <- function(geom_vpc, obs_data, pairs_data = NULL) {

  if (is.null(pairs_data)) {
    pairs_data <- matrix(c(gsub("-([A-Z,a-z])\\w+", "", names(geom_vpc)),
                           gsub("([A-Z,a-z])\\w+-", "", names(geom_vpc))), ncol = 2)
  }

  percentiles <- names(geom_vpc[[1]])
  percentiles_num <- as.numeric(sub("\\%.*", "", percentiles))

  if (all(pairs_data %in% colnames(obs_data))) {
    obs_contours <- NULL
    for (p in 1:nrow(pairs_data)) {
      kd_obs <- ks::kde(obs_data %>% select(pairs_data[p, ]), compute.cont = TRUE)
      contour_obs <- with(kd_obs, contourLines(x = eval.points[[1]], y = eval.points[[2]],
                                               z = estimate, levels = cont[paste0(100-percentiles_num,"%")]))
      obs_contours <- rbind.data.frame(obs_contours, extract_contour_df(contour_obs, kd_obs$cont, 0, pairs_data[p, ]))
    }
  } else if (all(colnames(obs_data) %in%
                 c("level", "x", "y", "circle", "percentile",
                   "sim_nr", "var1", "var2"))) {
    obs_contours <- obs_data
  } else {
    stop("The obs_data does not seem to be a observed contour, or does not ",
         "contain the same names as the covariate names in pairs data or ",
         "the geom_vpc object.")
  }

  plot_list <- list()
  for (i in 1:nrow(pairs_data)) {
    var_pair <- paste0(pairs_data[i, 1], "-", pairs_data[i, 2])
    plot_list[[var_pair]] <- obs_contours %>%
      dplyr::mutate(key = paste(percentile, var1, var2, sim_nr, circle)) %>%
      dplyr::filter(var1 == pairs_data[i, 1], var2 == pairs_data[i, 2]) %>%
      ggplot2::ggplot() +
      geom_vpc[[i]] +
      ggplot2::geom_path(ggplot2::aes(x = x, y = y, color = percentile, group = key), color = "black") +
      ggplot2::labs(x = pairs_data[i, 1], y = pairs_data[i, 2]) +
      ggplot2::theme_bw() + ggplot2::theme(aspect.ratio = 1)
  }

  attr(plot_list, "obs_contours") <- obs_contours

  return(plot_list)
}

# - add possibility of combining plots (in matrix?)
# - add possibility to plot without observed data


#' Create ggplot layers for simulation data.
#'
#' @param sim_data A data.frame containing simulation dataset. A column called "simulation_nr" is reuqired to
#' indicate the number of simulations.
#' @param percentiles (A vector of) Numeric value(s) representing the
#' percentiles of the density distribution that appear in the VPC donut plot;
#' e.g., c(10, 50, 90) represents 10th, 50th and 90th percentiles.
#' @param B An integer indicating the number of simulations.
#' @param pairs_matrix Matrix with 2 column and each row containing a pair of
#' covariate names from the copula. If set to NULL, every possible covariate
#' pair is included.
#' @param conf_band A numeric value indicating the empirical confidence level for the width of the bands; e.g., 95 indicates 95\% confidence interval.
#' @param colors_bands A vector with two strings specifying the colors of the confidence bands.
#' @param return_polygons A logical value to indicate whether return to a (list of) sf polygon object or a list containing the ggplot layers for a donut VPC.
#'
#' @return
#' @export

extract_geom_donutVPC <- function(sim_data,
                                  percentiles, B, pairs_matrix = NULL,
                                  conf_band = 95, colors_bands = c("#99E0DC", "#E498B4"), return_polygons = FALSE) {

  sim_contours_list <- list()

  # if (is.null(vine$names)) {
  #   vine$names <- paste0("V", 1:vine$copula$structure$d) #  rename covariate names into e.g. "V1", "V2" and "V3"
  # }

  # if (!"simulation_nr") {
  #   vine$nobs <- nobs
  # } else if (is.null(vine$nobs)) {
  #   stop("nobs should be specified if the vine is not estimated from data",
  #        " and does not contain nobs in the vine object (vine$nobs)")
  # }
  var <- setdiff(colnames(sim_data), "simulation_nr")

  if (!"simulation_nr" %in% colnames(sim_data)) {
    #check for simulation_nr not in sim_data
    stop("Column 'simulation_nr' is missing in sim_data")
  }

  if (is.null(pairs_matrix)) {
    pairs_matrix <- t(combn(var, 2))
  } else if (!all(c(pairs_matrix) %in% var)) {
    #check for names not matching between vine and pairs_matrix
    stop("Covariate names in pairs_matrix differ from names in vine")
  }

  #simulate from vine
  # full_sim_data <- as.data.frame(rvine(vine$nobs*B, vine))
  # full_sim_data$b <- rep(1:B, each = vine$nobs) # add label for each run of simulation

  #calculate contours for every percentile and every covariate combination
  i <- 1
  for (b in 1:B) {
    cat("\r", "Calculate contours of simulation:", b, "/", B)
    sim_data_b <- sim_data[sim_data$simulation_nr == b,]
    for (p in 1:nrow(pairs_matrix)) {
      #use ks for density computation
      kd_sim <- ks::kde(sim_data_b[, pairs_matrix[p, ]], compute.cont = TRUE, approx.cont = FALSE)
      contour_sim <- with(kd_sim, grDevices::contourLines(x = eval.points[[1]], y = eval.points[[2]],
                                                          z = estimate, levels = cont[paste0(100-percentiles, "%")]))
      #extract information
      sim_contours_list[[i]] <- extract_contour_df(contour_sim, kd_sim$cont, b, pairs_matrix[p, ])
      i <- i + 1

    }

  }
  cat("\n")
  sim_contours <- dplyr::bind_rows(sim_contours_list)


  # percentiles <- as.numeric(gsub("%", "", unique(sim_contours$percentile), fixed = T))
  # percentiles <- as.numeric(gsub("%", "", sort(unique(sim_contours$percentile)), fixed = T))
  # pairs_matrix <- unique(sim_contours[, c("var1", "var2")])

  colors_bands <- colors_bands[((1:length(percentiles))/2 == round((1:length(percentiles))/2)) + 1] # colors were assigned to the bands depending on the odd or even order of percentiles
  names(colors_bands) <- percentiles

  contour_geoms <- list()
  conf_geom_data <- list()
  for (p in 1:nrow(pairs_matrix)) {
    var_pair <- paste0(pairs_matrix[p, 1], "-", pairs_matrix[p, 2])

    contour_geoms[[var_pair]] <- list()
    for (pr in percentiles) {
      sim_full_df <- sim_contours %>%
        dplyr::filter(var1 == pairs_matrix[p, 1], var2 == pairs_matrix[p, 2]) %>%
        dplyr::filter(percentile == paste0(pr, "%"))
      kd_sim_full <- ks::kde(sim_full_df[, c("x", "y")], compute.cont = TRUE, approx.cont = FALSE)
      # new contour was calculated based the coordinates of the previously calculated contours
      # different countours at the same density level will be merged
      contour_sim_full <- with(kd_sim_full, grDevices::contourLines(x = eval.points[[1]], y = eval.points[[2]],
                                                                    z = estimate, levels = cont[paste0(100-conf_band, "%")]))
      contour_data <- extract_contour_df(contour_sim_full, kd_sim_full$cont, pr, pairs_matrix[p, ])

      conf_geom_data[[paste0(var_pair, ": ", pr, "%")]] <- contour_data %>%
        create_polygon()

      contour_geoms[[var_pair]][[paste0(pr, "%")]] <- ggplot2::geom_sf(data = conf_geom_data[[paste0(var_pair, ": ", pr, "%")]],
                                                                       color = colors_bands[as.character(pr)], fill = colors_bands[as.character(pr)])
    }

  }

  if (return_polygons) {
    return(conf_geom_data)
  } else {
    return(contour_geoms)
  }
}
