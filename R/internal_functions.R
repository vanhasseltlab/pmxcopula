#' Calculate Correlations
#'
#' @param data A data.frame with rows corresponding to observations and columns corresponding to covariates.
#' @param pairs_matrix Matrix with 2 column and each row containing a pair of
#' covariate names. If set to NULL, every possible covariate
#' pair is included.
#'
#' @return A data.frame containing the correlation calculated for the dataset.
#' @noRd
calc_correlation <- function(data, pairs_matrix = NULL){

  if (is.null(pairs_matrix)) {
    pairs_matrix <- t(combinat::combn(colnames(data), 2))
  }

  cor <- matrix(data = NA, nrow = nrow(pairs_matrix), ncol = 5)
  colnames(cor) <- c("statistic", "Var1", "Var2","var_pair", "cor")
  for (i in 1:nrow(pairs_matrix)) {
    cor_pearson <- cor(x = data[, pairs_matrix[i,1]],
                       y = data[, pairs_matrix[i,2]],
                       use = "pairwise.complete.obs",
                       method = "pearson")

    cor[i,] <- c("correlation", pairs_matrix[i,1], pairs_matrix[i,2],
                 paste(pairs_matrix[i,1], pairs_matrix[i,2], sep = "_"),
                 cor_pearson)
  }

  cor <- cor |> as.data.frame() |> dplyr::mutate(cor = as.numeric(cor))
  return(cor)

}
