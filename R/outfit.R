# 'item', judge and criterion outfit

# return matrix of probabilities
probs <- function(beta) {
	P.mat <- matrix(NA, nrow = length(beta), ncol = length(beta))
	for (i in seq_along(beta)) {
		for (j in seq_along(beta)) {
			P.mat[i, j] <- exp(beta[i] - beta[j]) / (1 + exp(beta[i] - beta[j]))
		}
	}
  P.mat
}



# calculate z for every (binary) cell of 4-D array, and return array
z <- function(x, P.mat) {
	stopifnot(dim(x)[1:2] == dim(P.mat))  # test if x is 4D or less
	z_score <- array(NA, dim = dim(x))
	for (l in seq_len(dim(x)[4])) {  # need to use this only if x is 4D (criteria present)
		for (k in seq_len(dim(x)[3])) {  # only if judges present!
			for (j in seq_len(dim(x)[2])) {
				for (i in seq_len(dim(x)[1])) {
					if (!is.na(x[i, j, k, l])) {
		        z_score[i, j, k, l] <- (x[i, j, k, l] - P.mat[i, j]) / sqrt(P.mat[i, j] * (1 - P.mat[i, j]))
					}
				}
			}
		}
	}
	z_score
}


#' Item Outfit
#'
#' Return outfit test statistics for each performance.
#'
#' @param x Data array of binary pairwise comparison outcomes.
#' @param beta Numeric vector of location estimates for each performance
#' @export
item_outfit <- function(x, beta) {
	prob_matrix <- probs(beta)
	z_array <- z(x, prob_matrix)
	outfit_statistics <- apply(z_array, 1, function(x) sum(x^2, na.rm = TRUE)/sum(!is.na(x)))
	outfit_statistics
}
