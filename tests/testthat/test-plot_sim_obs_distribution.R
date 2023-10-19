# test_that("plot works", {
#
#   copula_example <- rvinecopulib::vine(df_example, copula_controls = list(family_set = "parametric"), margins_controls = list(mult = 1))
#   set.seed(123)
#   sim_data <- rvinecopulib::rvine(copula_example$nobs, copula_example) %>% as.data.frame() # we should indicate the data type for the function!!!!!!!!
#   data_type <- plot_sim_obs_distribution(sim_data = sim_data, obs_data = df_example, plot_type = "points", pick_color = c("#F46E32", "#5063B9"))
#
#   expect_equal(class(data_type), "data.frame")
# })

# test_that("plot works", {
#
#   copula_example <- rvinecopulib::vine(df_example, copula_controls = list(family_set = "parametric"), margins_controls = list(mult = 1))
#   set.seed(123)
#   sim_data <- rvinecopulib::rvine(copula_example$nobs, copula_example) %>% as.data.frame() # we should indicate the data type for the function!!!!!!!!
#   plot_test <- plot_sim_obs_distribution(sim_data = sim_data, obs_data = df_example, plot_type = "points", pick_color = c("#F46E32", "#5063B9"))
#
#   expect_named(names(plot_test), c("CREA-age","CREA-BW","age-BW"))
# })
