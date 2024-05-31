
# pmxcopula  <img src="inst/img/logo.png" width="120" align="right" />

<!-- badges: start -->
<!-- badges: end -->

Virtual population is an important element in pharmacometric (PMX) analysis.
The goal of *pmxcopula* is to facilitate the evaluation of copulas and realistic 
populations by providing functionality for calculating the marginal metrics 
(mean, standard deviation and percentiles) and dependency metrics (correlation 
and overlap percentage) of the observed and simulated populations, generating 
qqplots and donut visual predictive check plots (donutVPCs), 
applying subset simulation for specific group of individuals, and more.



## Installation

This is an *R* package. [*R*](https://www.r-project.org/) is required,
[*RStudio*](https://www.rstudio.com/) is recommended.

In order to use the package, we have to install it. The easiest way to
install it is via the [`devtools`](https://devtools.r-lib.org/) official
CRAN package.

```r
devtools::install_github("vanhasseltlab/pmxcopula")
```
After installing, we have to attach the package as usual:

```r
library(pmxcopula)
```

And we are ready to go!

## Basic usage

If the copula model could well capture the real-world population, the distribution properties of virtual population simulated should resemble the real-world population.

The copula model evaluation was conducted using a simulation-based strategy: by performing extensive (e.g. 100) simulations of the original dataset of the original dataset, we compare the distribution properties between original (real-world) population and simulated (virtual) populations. 

### Perform extensive simulations of virtual populations for copula evaluation

``` r
# load a copula model: pediatric_cop
file_path <- system.file("extdata", "pediatric_cop.Rdata", package = "pmxcopula")
load(file_path)
pediatric_sim <- rcopula(pediatric_cop, sim_nr = 100)
```

### Calculate marginal metrics
Marginal metrics can be calculated with calc_margin() functions.
Compare the marginal matrics [M] between observed and simulated data in terms of relative error (RE):
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

### Calculate the dependency metrics
Pearson correlations and overlap metrics can be calculated with calc_dependency() functions.
Overlap metrics is calculated based on specific density contours (e.g. 95th percentile density contours) of the real-world and virtualp populaions:  for each pair combination of covariates, 95th percentile density contours were calculated for observed and simulated populations. The overlap metric was computed as the Jaccard index: the ratio between the intersection area and union area. Higher overlap indicated a better description of dependence relations.

Pearson correlation quantifies linear association, while data sharing the same linear correlation could exhibit different dependency structures, and the overlap metric takes the shape or pattern of the dependency into account. Pearson correlation and overlap metric collectively depicted the joint behavior at a pairwise level and addressed different perspectives.
``` r
mtr_dependency <- calc_dependency(
    sim_data = pediatric_sim,
    obs_data = pediatric_3cov,
    pairs_matrix = NULL,
    percentile = 95,
    sim_nr = 100
    )
```

### Generate donutVPC
Donut visual predictive check plot(donutVPC) can be calculated with donutVPC() functions.
DonutVPC can help visualize the 90% prediction interval of 5th, 50th and 95th percentile density contour.
``` r
donutVPC(
    sim_data = pediatric_sim,
    obs_data = pediatric_3cov,
    percentiles = c(10, 50, 90),
    sim_nr = 100,
    pairs_matrix = ,
    conf_band = 95,
    colors_bands = c("#99E0DC", "#E498B4")
    )
```

## Citation

Please use the information below to compose your citation of this software.

```

```


## Authors

- [Laura B. Zwep](https://www.universiteitleiden.nl/en/staffmembers/laura-zwep#tab-1)
- [Yuchen Guo](https://www.universiteitleiden.nl/en/staffmembers/yuchen-guo#tab-1)
