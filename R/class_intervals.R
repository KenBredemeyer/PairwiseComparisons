#' Form class intervals
#'
#' @param data_matrix data matrix of proportions, ordered on location
#' @param locations Numeric vector. Locations returned from \code{estimate}
#' @param n_class_intervals Integer.  Number of class intervals
#'
#' @return A list of numeric matrices, one matrix for each performance, showing
#'   the performances it was involved with, and for each of these, the
#'   proportion, location and class interval used for Person Characteristic
#'   Curve.
#'
#' @export
class_intervals <- function(data_matrix, locations, n_class_intervals) {
#numbers of comparisons per script
	number_comparisons <- apply(data_matrix, 2, function(x) length(na.omit(x)))
	message(paste0(" Min comparisons per script is ", min(number_comparisons)))

	ci_numbers <- vector("list", ncol(data_matrix)-1)      # class interval numbers
	comparisons_i <- vector("list", ncol(data_matrix)-1)                    # comparisons_i is a list of which ordered scripts are involved in the comparison
	comparisons_i <- lapply(split(data_matrix,seq(ncol(data_matrix))), function(x) which(!is.na(x)))
	#comparisons_i <- lapply(comparisons_i, function(x) unname(x, force = TRUE))   # do I need this?

	for (c_i in 1:length(comparisons_i)) {
    div <- length(comparisons_i[[c_i]]) %/% n_class_intervals
    rem <- length(comparisons_i[[c_i]]) %% n_class_intervals
    etc <- c(rep(0, n_class_intervals-rem), rep(1, rem))
	  ci_numbers[[c_i]] <- rep(1:n_class_intervals, div+etc)
	}

	# Create a matrix of comparisons for each script (a list of matrices)
	comparison <- vector("list", nrow(data_matrix))
	for (i in 1:nrow(data_matrix)) {
	  comparison[[i]] <- cbind(unname(data_matrix[comparisons_i[[i]], i]), locations[(comparisons_i[[i]])], ci_numbers[[i]])
	  colnames(comparison[[i]]) <- c("Proportions", "Locations", "class_interval")
	}
	names(comparison) <- colnames(data_matrix)
	comparison
}
