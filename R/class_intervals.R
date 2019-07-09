#' Form class intervals
#'
#' @param x data matrix of proportions, ordered on location
#' @param b Numeric vector. Locations returned from \code{estimate}
#' @param nci Integer.  Number of class intervals
#'
#' @return A list of numeric matrices, one matrix for each performance, showing
#'   the performances it was involved with, and for each of these, the
#'   proportion, location and class interval used for Person Characteristic
#'   Curve.
#'
#' @export
class_intervals <- function(x, b, nci) {
#numbers of comparisons per script
	number_comparisons <- apply(x, 2, function(x) length(na.omit(x)))
	message(paste0(" Min comparisons per script is ", min(number_comparisons)))

	ci_numbers <- vector("list", ncol(x)-1)      # class interval numbers
	comparisons_i <- vector("list", ncol(x)-1)                    # comparisons_i is a list of which ordered scripts are involved in the comparison
	comparisons_i <- apply(x, 2, function(x) which(!is.na(x)))
	comparisons_i <- lapply(comparisons_i, function(x) unname(x, force = TRUE))   # do I need this?

	for (c_i in 1:length(comparisons_i)) {
    div <- length(comparisons_i[[c_i]]) %/% nci
    rem <- length(comparisons_i[[c_i]]) %% nci
    etc <- c(rep(0, nci-rem), rep(1, rem))
	  ci_numbers[[c_i]] <- rep(1:nci, div+etc)
	}

	# Create a matrix of comparisons for each script (a list of matrices)
	comparison <- vector("list", nrow(x))
	for (i in 1:nrow(x)) {
	  comparison[[i]] <- cbind(unname(x[comparisons_i[[i]], i]), b[(comparisons_i[[i]])], ci_numbers[[i]])
	  colnames(comparison[[i]]) <- c("Proportions", "Locations", "class interval")
	}
	names(comparison) <- colnames(x)
	comparison
}
