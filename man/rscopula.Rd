% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulation_function.R
\name{rscopula}
\alias{rscopula}
\title{Simulate Subset Virtual Population}
\usage{
rscopula(
  copula = copula,
  n_sim = NULL,
  xmin = NULL,
  xmax = NULL,
  category = NULL,
  var_types = NULL
)
}
\arguments{
\item{copula}{Copula model, an object of class "vine_dist" or "vine", from rvinecopulib
package.}

\item{n_sim}{Number of individuals.}

\item{xmin}{A numeric vector of length c; Lower bound for the simulation condition (only for continuous data); NULL means no boundary.}

\item{xmax}{A numeric vector; Upper bound for the simulation condition (only for continuous data); NULL means no boundary.}

\item{category}{A list with elements to specify the category for the simulation condition (only for continuous data); NULL means no boundary.}

\item{var_types}{A vector of variable types, a length d vector; e.g., c("c", "c") for two continuous variables, or c("c", "d") for first variable continuous and second discrete.}
}
\value{
A data.frame containing simulation dataset. Rows correspond
to observations and columns correspond to variables.
}
\description{
Simulate Subset Virtual Population
}
\examples{
# load a copula model: pediatric_cop
file_path <- system.file("extdata", "pediatric_cop.Rdata", package = "pmxcopula")
load(file_path)
pediatric_sub <- rscopula(
    pediatric_cop,
    n_sim = 100,
    xmin = c(10,100, 100),
    var_types = c("c", "c", "c")
    )
}
