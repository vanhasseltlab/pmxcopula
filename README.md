
# pmxcopula  <img src="inst/img/logo.png" width="120" align="right" />

<!-- badges: start -->
<!-- badges: end -->

Virtual population is an important element in pharmacometric (PMX) simulations.
The goal of *pmxcopula* is to facilitate the evaluation of copulas by comparing 
the virtual population with the real-world population. The package provides 
functionality for visualizing distribution plots, calculating the marginal and dependency metrics, 
generating qqplots and donut visual predictive check plots (donutVPCs), and more.

An overview of the evaluation tools are summarized in our recent [*poster*](https://www.page-meeting.org/?abstract=10989).


## Installation

This is an *R* package. [*R*](https://www.r-project.org/) is required,
[*RStudio*](https://www.rstudio.com/) is recommended.

As *pmxcopula* is still hosted on Github, we need to install it via 
the [`devtools`](https://devtools.r-lib.org/) official CRAN package.

```r
devtools::install_github("vanhasseltlab/pmxcopula")
```
After installing, we have to attach the package as usual:

```r
library(pmxcopula)
```

The package is now ready for use!

## Basic usage

As the goal of developing copulas is mainly to facilitate the generation of realistic 
virtual population, copula evaluation is conducted by comparing the distribution
properties of the virtual population simulated from the copula with those of the 
original (real-world) population. If the copula model could well capture the real-world 
population, the distribution properties of the virtual population simulated 
should resemble the real-world population. 


### Perform simulations of virtual populations for copula evaluation
To avoid arbitrariness induced by sampling variability, extensive (e.g. 100) 
simulations of the original dataset is needed. You can perform the simulation with the copula
_pediatric_cop_ in the package, or you can also estimate a copula from your own data. 
Several datasets are available for exploration and analysis.

``` r
# load a copula model: pediatric_cop
file_path <- system.file("extdata", "pediatric_cop.Rdata", package = "pmxcopula")
load(file_path)
pediatric_sim <- rcopula(pediatric_cop, sim_nr = 100)

# available datasets in the package
data("MIMIC_5cov")
data("NHANES_12cov")
data("pediatric_3cov")
data("pediatric_sim")
```
### Visualize the marginal distribution
Marginal distribution can be visualized with _plot_1dist()_ functions.

``` r
plot1d <- plot_1dist(
  sim_data = pediatric_sim,
  obs_data = pediatric_3cov,
  sim_nr = 100,
  pick_color = c("#3ABAC1","#969696")
)
```
### Visualize the dependency structure
Dependency structure can be visualized with _plot_mvdist()_ functions.

``` r
plot1d <- plot_1dist(
  sim_data = pediatric_sim,
  obs_data = pediatric_3cov,
  sim_nr = 100,
  pick_color = c("#3ABAC1","#969696")
)
```

### Calculate marginal metrics
Marginal metrics can be calculated with _calc_margin()_ functions.
Compare the marginal metrics [M] between observed and simulated data in terms of relative error (RE):
RE=(M_sim-M_obs)/M_obs 
M_sim and M_obs represent the metrics for simulated population and observed population, respectively.
``` r
mtr_margin <- calc_margin(
    sim_data = pediatric_sim,
    obs_data = pediatric_3cov,
    sim_nr = 100,
    var = NULL,
    aim_statistic = c("mean", "median")
    )

```

### Calculate dependency metrics
Pearson correlations and overlap metrics can be calculated with _calc_dependency()_ functions.
Pearson correlation quantifies linear association. Overlap metrics are calculated based on specific density contours (e.g. 95th percentile density contours) of the real-world and virtual populations: for each pair combination of covariates, 95th percentile density contours were calculated for observed and simulated populations. The overlap metric was computed as the Jaccard index: the ratio between the intersection area and union area. Higher overlap indicated a better description of dependence relations.
Since data sharing the same linear or non-linear correlation could exhibit different dependency structures, the overlap metric takes the shape or pattern of the dependency into account. Pearson correlation and overlap metric collectively depict the joint behavior at a pairwise level and address different perspectives. Multiple cores are recommended to accelerate the calculation.

``` r
mtr_dependency <- calc_dependency(
    sim_data = pediatric_sim,
    obs_data = pediatric_3cov,
    pairs_matrix = NULL,
    percentile = 95,
    sim_nr = 100,
    cores = 4
    )
```
### Generate qqplots
qqplots can be generated with _plot_qq()_ functions.
``` r
qqplot <- plot_qq(
  sim_data = pediatric_sim,
  obs_data = pediatric_3cov,
  sim_nr = 100,
  conf_band = 95,
  var = NULL,
  type = "ribbon"
)
```

### Generate donutVPCs
Donut visual predictive check plot(donutVPC) can be generated with _donutVPC()_ function.
DonutVPC can help visualize the 90% prediction interval of 5th, 50th and 95th percentile density contour. Multiple cores are recommended to accelerate the calculation.
``` r
donut <- donutVPC(
    sim_data = pediatric_sim,
    obs_data = pediatric_3cov,
    percentiles = c(5, 50, 95),
    sim_nr = 100,
    conf_band = 99,
    colors_bands = c("#99E0DC", "#E498B4"),
    cores = 4
    )
```

## References
1. Guo Y, Guo T, et al, PAGE 32 (2024) Abstr 10989 www.page-meeting.org/?abstract=10989
2. Czado C, Nagler T (2022) Vine Copula Based Modeling. Annual Review of Statistics and Its Application 9:453–477. https://doi.org/10.1146/annurev-statistics-040220-101153
3. Zwep LB, Guo T, Nagler T, et al (2024) Virtual Patient Simulation Using Copula Modeling. Clinical Pharmacology & Therapeutics 115:795–804.  https://doi.org/10.1002/cpt.3099


## Authors
- [Yuchen Guo](https://www.universiteitleiden.nl/en/staffmembers/yuchen-guo#tab-1)
- [Laura B. Zwep](https://www.universiteitleiden.nl/en/staffmembers/laura-zwep#tab-1)
