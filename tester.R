#VPC function tester
source("scripts/VPC_donut_functions.R")

#dependencies (libraries)
library(kde1d)
library(rvinecopulib)
library(sf)
library(ks)
library(tidyverse) #dplyr + ggplot
library(ggplot2)
#make some data
set.seed(123)
df_example <- read.csv("data/pediatrics_example_data.csv", row.names = 1)
df_example$biomarker <- exp(rnorm(nrow(df_example), 2, 1.35))

#fit copula
copula_example <- vine(df_example, copula_controls = list(family_set = "parametric"), margins_controls = list(mult = 1))


#functions VPC
set.seed(04285195)

sim_contour_data <- simulate_contours(copula_example, percentiles = c(10, 50, 90), B = 10) #5 sec/sim!
sim_contour_data <- simulate_contours(copula_example, percentiles = c(10, 90, 50), B = 10) #5 sec/sim!
sim_contour_data <- simulate_contours(copula_example, percentiles = c(10, 50, 90), pairs_matrix = matrix(c("CREA",  "CREA", "age","age", "BW", "BW"), ncol = 2), B = 10) #5 sec/sim!

geom_vpc <- create_geom_donutVPC(sim_contour_data, colors_bands = c("#99E0DC", "#E498B4", "black"))
# return_polygons = FALSE
geom_vpc <- create_geom_donutVPC(sim_contour_data, colors_bands = c("#99E0DC", "#E498B4", "black"), return_polygons = FALSE) # return to a list
geom_vpc <- create_geom_donutVPC(sim_contour_data, colors_bands = c("#99E0DC", "#E498B4"), return_polygons = FALSE)
plots <- ggVPC_donut(geom_vpc, df_example)

# check outcomes
names(plots)
plots[["age-BW"]]
plots[["CREA-age"]]

sim_data <- as.data.frame(rvine(copula_example$nobs*B, copula_example))
sim_data$simulation_nr <- rep(1:B, each = copula_example$nobs)



### messy for overlap

source("scripts/VPC_donut_functions.R")

percentiles <- 5
kd_obs <- ks::kde(df_example[, c("age", "BW")], compute.cont = TRUE, approx.cont = FALSE)
contour_obs <- with(kd_obs, contourLines(x = eval.points[[1]], y = eval.points[[2]],
                                         z = estimate, levels = cont[paste0(percentiles, "%")]))
#extract information
obs_contours_list <- extract_contour_df(contour_obs, kd_obs$cont, 0, c("age", "BW"))

poly_output_obs <- create_polygon(obs_contours_list)

#simulate from vine
set.seed(123)
sim_data <- rvine(copula_example$nobs, copula_example)

kd_sim <- ks::kde(sim_data[, c("age", "BW")], compute.cont = TRUE, approx.cont = FALSE)
contour_sim <- with(kd_sim, contourLines(x = eval.points[[1]], y = eval.points[[2]],
                                         z = estimate, levels = cont[paste0(percentiles, "%")]))
#extract information
sim_contours_list <- extract_contour_df(contour_sim, kd_sim$cont, 0, c("age", "BW"))

poly_output_sim <- create_polygon(sim_contours_list)


ggplot() +
  geom_sf(data = poly_output_obs, color = "#99E0DC", fill = "#99E0DC") +
  geom_sf(data = poly_output_sim, color = "orange", fill = "orange",  alpha = 0.3) +
  theme_bw() + theme(aspect.ratio = 1)

#' Create ggplot layer for contours
#'
#' Create the ggplot contour layers for a donut VPC
#'
#' @param sim_contours Output data.frame from the simulate_contours function.
#' @param conf_band A numeric value indicating the empirical confidence level for the width of the bands; e.g., 95 indicates 95% confidence interval.
#' @param colors_bands A vector with two strings specifying the colors of the confidence bands.
#' @param return_polygons A logical value to indicate whether return to a (list of) sf polygon object or a list containing the ggplot layers for a donut VPC.
#'
#' @return A list containing the ggplot layers for a donut VPC or a (list
#' of) sf polygon object.
#' @export
#'
#' @examples
#'
