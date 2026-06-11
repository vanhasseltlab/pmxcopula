#' Monte Carlo sampling from vine object
#'
#' Samples from a vine copula object and calculates the MC density. The result
#' is a `repscore` object including the sample and the densities
#'
#' @param vine_obj an object of class `vine_dist` or `vinecop_dist`.
#' @param n the number of MC samples.
#' @param cores the number of cores to use for parallel computations in vine
#' functions.
#'
#' @return an object of class `repscore` including the MC sample and their
#' corresponding densities
#'
#' @import rvinecopulib
#' @export
repscore_MC <- function(vine_obj, n, cores) {
  MC_sample <- rvinecopulib::rvine(n, vine_obj, cores = cores)
  MC_dens <- rvinecopulib::dvine(MC_sample, vine_obj, cores = cores)

  MC_results <- list(sample = MC_sample, density = MC_dens, vine_obj = vine_obj)
  class(MC_results) <- "repscore"
  return(MC_results)
}

#' Plot histogram from repscore MC sample
#'
#' Plots a histogram of the densities of the log density from the MC sample. Uses ggplot2.
#' @param obj an object of class `repscore` including the MC sample and their
#' corresponding densities
#' @param vline probability value
#'
#' @export
plot.repscore <- function(obj, vline = NULL) {
  N <- length(obj$density)

  p_density <- data.frame(d_score = log(obj$density), id = 1:N) |>
    ggplot2::ggplot() +
    ggplot2::geom_histogram(ggplot2::aes(x = d_score, y = ggplot2::after_stat(count)/sum(ggplot2::after_stat(count))),
                            bins = 100, fill = "#E0E0E0", color = "#E0E0E0") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(y = "Frequency", x = "log(copula density)") +
    ggplot2::theme_bw() + ggplot2::theme(panel.grid = ggplot2::element_blank())

  if (!is.null(vline)) {
    if (!is.numeric(vline)) {
      warning("Value of vline must be numeric, vertical line not added")
    } else {
      quantile_vl <- quantile(log(obj$density), probs = vline)
      p_density <- p_density +
        ggplot2::geom_vline(xintercept = quantile_vl, color = "#1a1c67ff")
    }
  }
  return(p_density)
}


#' Print method for repscore objects
#'
#' @inheritParams plot.repscore
#'
#'@export
print.repscore <- function(obj) {
  N <- length(obj$density)
  if (N > 10^4) N <- formatC(N, format = "g")
  p <- length(obj$vine_obj$names)
  cat("MC samples (n = ", N, ") of vine copula based on ", p, " variables\n",
      "",
      sep = ""
  )
}

#' Calculate the representation probability for a new (set of) patient(s)
#'
#' @inheritParams plot.repscore
#' @param dat a data.frame containing the new patient data
#' @param type type of output, either the density, logdensity or the probability
#'
#' @return a numeric vector, with the same length as rows in dat. Wil contain either the density, log(density) or representation probability, depending on specified type.
#'
#' @export
get_repscore <- function(obj, dat, type = "prob") {
  #checking inputs
  if (suppressWarnings(!all(names(dat) == obj$vine_obj$names))) {
    if (all(obj$vine_obj$names %in% names(dat))) {
      message(
        "Using only columns ", paste(obj$vine_obj$names, collapse = ", "),
        " in the vine object"
      )
      dat <- dat[obj$vine_obj$names]
    } else if (length(names(dat)) >= length(obj$vine_obj$names)) {
      warning(
        "First ", length(obj$vine_obj$names), " columns are used corresponding",
        " to the names: ", paste(obj$vine_obj$names, collapse = ", ")
      )
      names(dat)[1:length(obj$vine_obj$names)] <- obj$vine_obj$names
    } else {
      stop(
        "Number of columns of dat is smaller than in the copula ",
        "object. Check if data corresponds to vinecopula in the repprop object."
      )
    }
  }

  #core of the function
  l_mc_dens <- log(obj$density)
  dens <- rvinecopulib::dvine(dat, obj$vine_obj)
  if (type == "dens") {
    output <- dens
  }
  if (type == "logdens") {
    dens_p <- log(dens)
    output <- dens_p
  }
  if (type == "prob") {
    dens_p <- log(dens)
    pvalue <- numeric(length(dens_p))
    for (i in 1:length(dens_p)) {
      pvalue[i] <- mean(l_mc_dens < dens_p[i])
    }
    output <- pvalue
  }
  return(output)
}
