#' create a data matrix from a set of pairwise comparisons
#'
#' data matrix returned for estimation.
#'
#' @param x A data frame containing the raw results from pairwise comparisons
pairs_format <- function(x) {
	options(stringsAsFactors = FALSE)
	x <- pref_codes(x)
	x <- left_preferred(x)
	x <- add_comparisons(x)
	x <- complete_comparisons(x)
	data_matrix <- data_matrix(x)
	x <- data_matrix
	x <- remove_xtrms(x)
	xtrm <- xtrms(x)

  attr(x, "data_matrix") <- list(data_matrix)
  attr(x, "extremes") <- list(xtrm)
  x
}

