#' Newton-Raphson estimation
#' Bradley-Terry-Luce
#'
#' @param x data matrix of proportions
#' @param convergence Numeric vector length 2.  Inner convergence criteria and
#'   outer convergence criteria.
#' @param loop_size Numeric vector length 2.  Inner maximum loops and outer
#'   maximum loops.
#' @export
estimate <- function(x, convergence_criteria = c(0.001, 0.001), loop_size = c(30, 100)) {
  N <- dim(x)[1]
  # set initial estimates
  b <- rep(0, N)

  iterate_b_inner <- matrix(NA, nrow = loop_size[1], ncol = dim(x)[2]) # betas from inner loop
  convergence <- matrix(NA, nrow = loop_size[2], ncol = dim(x)[2]) # betas from the outer loop
  convergence[1, ] <- 0
  se <- matrix(NA, nrow = N, ncol = 1)

  for (ot in 2:loop_size[2]) {  # outer loop
  	for (n in 1:N) {            # person loop
  		comparisons_i <- which(!is.na(x[n, ]))
  		bm <- b[comparisons_i]
  		involved <- x[n, comparisons_i] + x[comparisons_i, n]
  		for (i in 2:loop_size[1]) {
  			probs <- exp(b[n] - bm) / (1 + exp(b[n] - bm))
  			fp <- sum(involved * probs) - sum(x[n,], na.rm = TRUE)   # first deriviative
  			fpp <- sum(involved * probs * (1 - probs))
  			b[n] <- b[n] - fp / fpp
  			iterate_b_inner[i, n] <- b[n]
  			if (!is.na(iterate_b_inner[i, n]) & !is.na(iterate_b_inner[i - 1, n]) &
  					abs(iterate_b_inner[i, n] - iterate_b_inner[i-1, n]) <= convergence_criteria[1]) break}
  		se[n, ] <- 1 / sqrt(fpp)
  	}
  	b <- b - mean(b)
  	convergence[ot, ] <- b
  	if (!any(is.na(convergence[ot, ])) & !any(is.na(convergence[ot-1, ])) &
  			max(abs(convergence[ot, ] - convergence[ot-1, ])) < convergence_criteria[2]) break
  }
  results <- data.frame(name = dimnames(x)[[2]], b, se)
  attr(results, "convergence") <- list(convergence = convergence)
  results
}

convergence <- function(x) {
	attributes(x)$convergence$convergence
}

