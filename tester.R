#VPC function tester
source("scripts/VPC_donut_functions.R")

#dependencies (libraries)
library(kde1d)
library(rvinecopulib)
library(sf)
library(ks)
library(tidyverse) #dplyr + ggplot
library(ggplot2)
library(pmxcopula)
library(tibble)
library(patchwork)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ks)
library(sf)
library(dplyr)



#make some data
set.seed(123)
df_example <- read.csv("data/pediatrics_example_data.csv", row.names = 1)
df_example$biomarker <- exp(rnorm(nrow(df_example), 2, 1.35))

#fit copula
copula_example <- vine(df_example, copula_controls = list(family_set = "parametric"), margins_controls = list(mult = 1))


#functions VPC
set.seed(04285195)

sim_contour_data <- simulate_contours(copula_example, percentiles = c(10, 50, 90), B = 10) #5 sec/sim!
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

#########################
##### function test #####
#########################

## test the get_donutVPC() function ---
# test
# t(combn(var, 2))
# obs_data <- df_example
donutVPC_test <- get_donutVPC(sim_data = sim_example,
                              obs_data = df_example,
                              percentiles = c(10, 50, 90),
                              sim_nr = 100,
                              pairs_matrix = NULL,
                              conf_band = 95,
                              colors_bands = c("#99E0DC", "#E498B4"))
donutVPC_mvn <- get_donutVPC(sim_data = sim_mvn,
                             obs_data = Liver_expression_ins_log,
                             percentiles = c(10, 50, 90),
                             sim_nr = 100,
                             pairs_matrix = NULL,
                             conf_band = 95,
                             colors_bands = c("#99E0DC", "#E498B4")) # run on server

donutVPC_transmvn <- get_donutVPC(sim_data = sim_mvn_un_cmp,
                                  obs_data = Liver_expression_ins_log,
                                  percentiles = c(10, 50, 90),
                                  sim_nr = 100,
                                  pairs_matrix = NULL,
                                  conf_band = 95,
                                  colors_bands = c("#99E0DC", "#E498B4"))

donutVPC_transmvn <- get_donutVPC(sim_data = sim_copula_cmp,
                                  obs_data = Liver_expression_ins_log,
                                  percentiles = c(10, 50, 90),
                                  sim_nr = 100,
                                  pairs_matrix = NULL,
                                  conf_band = 95,
                                  colors_bands = c("#99E0DC", "#E498B4"))

donutVPC_mvn_plot <- wrap_plots(donutVPC_mvn, nrow = 9)
ggsave(donutVPC_mvn_plot, file = "tests/dunutVPC_MVN.tiff",width = 13, height = 13, units = "in", dpi = 300)

donutVPC_transmvn_plot <- wrap_plots(donutVPC_transmvn, nrow = 9)
ggsave(donutVPC_transmvn_plot, file = "tests/trans.tiff",width = 13, height = 13, units = "in", dpi = 300)


donutVPC_cop_plot <- wrap_plots(donutVPC_transmvn, nrow = 9)
ggsave(donutVPC_cop_plot, file = "tests/dunutVPC_cop.tiff",width = 13, height = 13, units = "in", dpi = 300)

## test the simulate_contours() function ---
sim_example_10 <- sim_example[sim_example$simulation_nr == c(1:10),]
sim_contour_test <- simulate_contours(sim_data = sim_example_10,
                                      percentiles = c(10, 50, 90),
                                      sim_nr = 10,
                                      pairs_matrix = NULL)

## test mVPC function ----
# test
mVPC_test <- mVPC(sim_data = sim_example,
                  obs_data = df_example,
                  var = NULL,
                  sim_nr = 100,
                  title = NULL,
                  colors_bands = c("#99E0DC", "#E498B4"),
                  full_plot = TRUE,
                  caption = NULL,
                  percentiles = c(10, 50, 90),
                  conf_band = 95)
# grid.draw(mVPC)
#
ggsave("mVPC_test.pdf", mVPC_test, width = 6, height = 6)


## test the box for the categorical variables ----
library(tidyverse)
possi <- paste(c(rep("m", 3), rep("f", 3)), rep(c("A", "B", "C"), 2), sep = "_")
set.seed(123)
test_data <- sample(possi, 500, prob = c(0.2, 0.3, 0.02, 0.35, 0.1, 0.03), replace = TRUE) %>%
  str_split_fixed(pattern = "_", n=2) %>% as.data.frame()
sim_data <- sample(possi, 500*100, prob = c(0.3, 0.2, 0.02, 0.35, 0.1, 0.03), replace = TRUE) %>%
  str_split_fixed(pattern = "_", n = 2) %>% as.data.frame() %>%
  mutate(sim_nr = rep(1:100, each = 500))

barboxplot_comparison(c("V1", "V2"), sim_data, test_data)
barboxplot_comparison(c("V2", "V1"), sim_data, test_data)


## test qqplot ----
# obs_data
load("data/df_example.rda")
load("data/sim_example.rda")

# sim_data
n_sim <- nrow(df_example)
m <- 100
sim_raw <- rvine(copula_example, n = n_sim * m)
sim_example <- cbind(
  sim_raw,
  simulation_nr = rep(1:m, each = n_sim)) %>%
  as.data.frame()
usethis::use_data(sim_example)

# test
test_qq <- get_qqplot(sim_data = sim_example,
                      obs_data = df_example,
                      sim_nr = 100,
                      conf_band = 95,
                      var = NULL,
                      type = "point")
# test
test_qqplot <- plot_qq(sim_data = sim_example,
                       obs_data = df_example,
                       sim_nr = 100,
                       conf_band = 95,
                       var = NULL,
                       type = "point")

# test with points-version qqplot
test_qq <- get_qqplot(sim_data = sim_example,
                      obs_data = df_example,
                      sim_nr = 100,
                      conf_band = 95,
                      var = NULL,
                      percentiles = c(10, 50, 90))

# test the selective simulation
xmin = c(10, NaN, 200)
xmax = c(100, NaN, 1000)
copula = copula_example
# validate: PASS
pseudo_sim_validate <- pseudo_sim %>%
  filter(CREA > 10 & CREA < 100) %>%
  filter(BW > 200 & BW < 1000)
category  = list(c("male", "female"),"all", "all") # should be the form of list
pseudo_sim <- data.frame(age = c(1:10),
                         gender = c(rep("male",3), rep("female",3),rep("unknown",4)),
                         height = runif(10, min = 100, max = 200))
rcopula_sub_test <- rscopula(copula = copula,
                                 n_sim = 200,
                                 xmin = c(10, NaN, 200),
                                 xmax = NULL,
                                 category = NULL,
                                 var_types = c("c", "c","c"))

# test marginal metric calculation
mrg_test <- mrg_metrics(df_example)
# test marginal metric comparison
test_compare <- marginal_metric(sim_data = sim_example,
                                obs_data = df_example,
                                sim_nr = 100)

# test correlation
test_cor <- get_correlation(obs_data, pairs_matrix = NULL)

# fit a copula for MIMIC data
MIMIC_cop <- vine(MIMIC_data,
                  copula_controls = list(family_set = "parametric"),
                  margins_controls = list(mult = 1),
                  cores = 1)
filepath <- "inst/extdata/MIMIC_cop.Rdata"
save(MIMIC_cop, file = filepath)
# to load the MIMIC_cop
system.file("extdata", "MIMIC_cop.Rdata", package = "pmxcopula")

# simulate from MIMIC_cop
MIMIC_sim <- rvine(MIMIC_cop$nobs*100, MIMIC_cop) %>%
  as.data.frame() %>%
  cbind(simulation_nr = rep(1:100, each = MIMIC_cop$nobs))
usethis::use_data(MIMIC_sim)
NHANES_12cov <- NHANES_data
usethis::use_data(NHANES_12cov,overwrite = TRUE)

############################
############################
############################

library(rvinecopulib)
# fit a copula for pediatric data
pediatric_cop <- vine(pediatric_3cov,
                  copula_controls = list(family_set = "parametric"),
                  margins_controls = list(mult = 1),
                  cores = 1)
filepath <- "inst/extdata/pediatric_cop.Rdata"
save(pediatric_cop, file = filepath)
# to load the pediatric_cop
system.file("extdata", "pediatric_cop.Rdata", package = "pmxcopula")

# simulate from pediatric_cop
pediatric_sim <- rvine(pediatric_cop$nobs*100, pediatric_cop) %>%
  as.data.frame() %>%
  cbind(simulation_nr = rep(1:100, each = pediatric_cop$nobs))
usethis::use_data(pediatric_sim)

NHANES_12cov <- NHANES_data
usethis::use_data(NHANES_12cov,overwrite = TRUE)

# test overlap
pediatric_sim_10 <- pediatric_sim %>%
  filter(simulation_nr < 11)
mtr_dependency <- pmxcopula::calc_dependency(sim_data = pediatric_sim_10, obs_data = pediatric_3cov, pairs_matrix = NULL, percentile = 95, sim_nr = 10)

# test marginal
mtr_margin <- calc_margin(sim_data = pediatric_sim, obs_data = pediatric_3cov, sim_nr = 100, var = NULL, aim_statistic = c("mean", "median", "sd"))
#' mtr_margin

# test 1d density curve
plot_1dist(sim_data = pediatric_sim, obs_data = pediatric_3cov, sim_nr = 100, pick_color = c("#3ABAC1","#969696"), var = NULL)

# donutVPC
donutVPC(sim_data = pediatric_sim, obs_data = pediatric_3cov, percentiles = c(10, 50, 90), sim_nr = 100, pairs_matrix = NULL, conf_band = 95, colors_bands = c("#99E0DC", "#E498B4"))

# rscopula
# test the selective simulation
xmin = c(10, NaN, 200)
xmax = c(100, NaN, 1000)
copula = copula_example
# validate: PASS
pseudo_sim_validate <- pseudo_sim %>%
  filter(CREA > 10 & CREA < 100) %>%
  filter(BW > 200 & BW < 1000)
category  = list(c("male", "female"),"all", "all") # should be the form of list
pseudo_sim <- data.frame(age = c(1:10),
                         gender = c(rep("male",3), rep("female",3),rep("unknown",4)),
                         height = runif(10, min = 100, max = 200))
rcopula_sub_test <- rscopula(copula = pediatric_cop,
                             n_sim = 200,
                             xmin = c(10, NaN, 200),
                             xmax = NULL,
                             category = NULL,
                             var_types = c("c", "c","c"))
mtr_olp <- pmxcopula::calc_overlap(sim_data = pediatric_sim_10, obs_data = pediatric_3cov, pairs_matrix = NULL, percentile = 95, sim_nr = 10)
