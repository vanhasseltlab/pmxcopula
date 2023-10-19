
# pmxcopula  <img src="inst/img/logo.png" width="120" align="right" />

<!-- badges: start -->
<!-- badges: end -->

Virtual population is an important element in pharmacometric (PMX) analysis.
The goal of pmxcopula is to calculate the overlap percentage and plot the VPC 
donut to evaluate the capture of the dependency structure between covariates 
in virtual population. 


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

### Simulating the density contours based on the vine copula

With simulate_contours function, we could directly generate the simulated contours 
at different percentiles based on the established vine copula model. 

``` r
# fit the vine copula model to observation data with *rvinecopulib* package.
library(rvinecopulib)
copula_example <- vine(df_example, copula_controls = list(family_set = "parametric"), margins_controls = list(mult = 1))

# extract the density contours of simulation data
set.seed(123456)
sim_contour_data <- simulate_contours(copula_example, percentiles = c(10, 50, 90), B = 10) 
```

### Generating the VPC donut

VPC donut can be generated with create_geom_donutVPC and ggVPC_donut functions.

``` r
# create the ggplot layers of the simulation contours
geom_vpc <- create_geom_donutVPC(sim_contour_data, colors_bands = c("#99E0DC", "#E498B4"))
# make the VPC_donut
plots <- ggVPC_donut(geom_vpc, df_example)

# visualize the plots
names(plots)
plots[["age-BW"]]
plots[["CREA-age"]]
plots[["CREA-BW"]]
```

### Calculate the overlap percentage of density contours

Overlap percentage can be calculated with get_overlap functions.

``` r
# fit the vine copula model to observation data with *rvinecopulib* package.
library(rvinecopulib)
copula_example <- vine(df_example, copula_controls = list(family_set = "parametric"), margins_controls = list(mult = 1))

# perform simulations
B = 10
sim_data <- as.data.frame(rvine(copula_example$nobs*B, copula_example))
sim_data$simulation_nr <- rep(1:B, each = copula_example$nobs)

# calculate the overlap of 95th percentile contour
overlap <- get_overlap(sim_data = sim_data, obs_data = df_example, variables = NULL, percentile = 95, B = 10, summarize = FALSE)
```

## Citation

Please use the information below to compose your citation of this software.

```

```


## Authors

- [Laura B. Zwep](https://www.universiteitleiden.nl/en/staffmembers/laura-zwep#tab-1)
- [Yuchen Guo](https://www.universiteitleiden.nl/en/staffmembers/yuchen-guo#tab-1)
