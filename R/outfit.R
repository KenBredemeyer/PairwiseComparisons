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
#' @param x 4D data array of binary pairwise comparison outcomes.
#' @param beta Numeric vector of location estimates for each performance
#' @param type Outfit statistic type: "performance", "judge", or "criterion".
#'
#' @return Data frame of names and outfit statistics.
#'
#' @examples
#' x.dm <- pairs_format(sim_judgements_small)
#' x.array <- pairs_array(sim_judgements_small)
#' performance_estimates <- estimate_betas(x.dm)
#' outfit(x.array, performance_estimates$b)
#'
#' @export
outfit <- function(x, beta, type = "performance") {
	outfit_type <- switch(type, performance = 1, judge = 3, criterion = 4)
	prob_matrix <- probs(beta)
	z_array <- z(x, prob_matrix)
	outfit_statistics <- apply(z_array, outfit_type, function(x) sum(x^2, na.rm = TRUE)/sum(!is.na(x)))
	rv <- data.frame(ID = dimnames(x)[[outfit_type]], outfit = outfit_statistics)
	rv
}

#' Infit
#' @export
infit <- function(x, beta, type = "performance") {
	infit_type <- switch(type, performance = 1, judge = 3, criterion = 4)
	P <- probs(beta)
 	Information <- P * (1 - P)
	z_array <- z(x, P)
	IZ <- Information * z_array^2
  top <- apply(IZ, infit_type, function(x) sum(x, na.rm = TRUE))
  denom <- apply(Information, infit_type, function(x) sum(x, na.rm = TRUE))
  result <- top / denom
	data.frame(ID = colnames(x), infit=result)
}


# 2D array ----------------------------------------------------------------

# calculate z for every (binary) cell of 4-D array, and return array
z2D <- function(x, P.mat) {
	stopifnot(dim(x)[1:2] == dim(P.mat))  # test if x is 4D or less
	stopifnot(max(x, na.rm = TRUE) == 1)
	z_score <- array(NA, dim = dim(x))
		for (j in seq_len(dim(x)[2])) {
			for (i in seq_len(dim(x)[1])) {
				if (!is.na(x[i, j])) {
	        z_score[i, j] <- (x[i, j] - P.mat[i, j]) / sqrt(P.mat[i, j] * (1 - P.mat[i, j]))
				}
			}
		}
	z_score
}

#' outfit.2D
#' @export
outfit.2D <- function(x, beta, type = "performance") {
	outfit_type <- switch(type, performance = 1, judge = 3, criterion = 4)
	prob_matrix <- probs(beta)
	z_array <- z2D(x, prob_matrix)
	outfit_statistics <- apply(z_array, outfit_type, function(x) sum(x^2, na.rm = TRUE)/sum(!is.na(x)))
	rv <- data.frame(ID = dimnames(x)[[outfit_type]], outfit = outfit_statistics)
	rv
}
