#' Newton-Raphson estimation Bradley-Terry-Luce
#'
#' @param x full data matrix of comparisons
#' @param convergence Numeric vector length 2.  Inner convergence criteria and
#'   outer convergence criteria.
#' @param loop_size Numeric vector length 2.  Inner maximum loops and outer
#'   maximum loops.
#' @param extremes Character vector of extreme performance names (returned as an
#'   attribute from \code{remove_xtrms}.
#' @param betas Numeric vector of betas estimated from \code{estimate}
#' @export
estimate_anch <- function(x, extremes, betas,
	adjust_extreme = 0.25,
	convergence_criteria = c(0.001, 0.001), loop_size = c(30, 100)) {
   # which are extremes
	 extrm_i <- match(extremes, dimnames(x)[[2]])    # faster than colnames
   # which are not extremes
	 beta_i <- seq_along(as.data.frame(x))[-extrm_i]   # CHECK PERFORMANCE

  N <- dim(x)[1]
  # set initial estimates
  b <- vector("numeric", N)
  b[beta_i] <- betas
  b[extrm_i] <- 0          # vector initiallized to 0 anyway

  iterate_b_inner <- matrix(0, nrow = loop_size[1], ncol = dim(x)[2]) # betas from inner loop
  convergence <- matrix(NA, nrow = loop_size[2], ncol = length(extrm_i)) # betas from the outer loop
  convergence[1, ] <- 0
  se <- matrix(NA, nrow = N, ncol = 1)

  for (ot in 2:loop_size[2]) {  # outer loop
  	for (n in extrm_i) {            # person loop
  		comparisons_i <- which(!is.na(x[n, ]))
  		bm <- b[comparisons_i]
  		involved <- x[n, comparisons_i] + x[comparisons_i, n]
  		for (i in 2:loop_size[1]) {
  			probs <- exp(b[n] - bm) / (1 + exp(b[n] - bm))
  				if (sum(x[n, ], na.rm = TRUE) == 0) { # low extreme
		  			fp <- sum(involved * probs) - (sum(x[n,], na.rm = TRUE) + adjust_extreme)  # first deriviative
  				} else if (sum(x[, n], na.rm = TRUE) == 0) { # high extreme
  					fp <- sum(involved * probs) - (sum(x[n,], na.rm = TRUE) - adjust_extreme)
  				} else {
  					stop("Error: check extremes")    # should never be called
  				}
  			fpp <- sum(involved * probs * (1 - probs))
  			b[n] <- b[n] - fp / fpp
  			iterate_b_inner[i, n] <- b[n]
  			if (!is.na(iterate_b_inner[i, n]) & !is.na(iterate_b_inner[i - 1, n]) &
  					abs(iterate_b_inner[i, n] - iterate_b_inner[i-1, n]) <= convergence_criteria[1]) break}
  		se[n, ] <- 1 / sqrt(fpp)
  	}
  	#b <- b - mean(b)
  	convergence[ot, ] <- b[extrm_i]
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

