#' Newton-Raphson estimation
#' Bradley-Terry-Luce
#'
#' @param x data matrix of counts
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


#' Newton-Raphson estimation - faster?
#' Bradley-Terry-Luce
#'
#' @param x data matrix of proportions
#' @param convergence Numeric vector length 2.  Inner convergence criteria and
#'   outer convergence criteria.
#' @param loop_size Numeric vector length 2.  Inner maximum loops and outer
#'   maximum loops.
#' @export
estimate2 <- function(x, convergence_criteria = c(0.001, 0.001), loop_size = c(30, 100)) {
  N <- dim(x)[1]
  # set initial estimates
  b <- rep(0, N)

  iterate_b_inner <- matrix(NA, nrow = loop_size[1], ncol = dim(x)[2]) # betas from inner loop
  convergence <- matrix(NA, nrow = loop_size[2], ncol = dim(x)[2]) # betas from the outer loop
  convergence[1, ] <- 0
  se <- matrix(NA, nrow = N, ncol = 1)

  comparisons_i <- vector("list", length = N)   # no apparent performance gain
  comparisons_i <- apply(unname(x), 1, function(x) which(!is.na(x)))
  involved <- vector("list", length = N)
  for (n in 1:N) {
  	involved[[n]] <- x[n, comparisons_i[[n]]] + x[comparisons_i[[n]], n]
  }
  rm(n)

  for (ot in 2:loop_size[2]) {  # outer loop
  	for (n in 1:N) {            # person loop
  		bm <- b[comparisons_i[[n]]]
  		for (i in 2:loop_size[1]) {
  			probs <- exp(b[n] - bm) / (1 + exp(b[n] - bm))
  			fp <- sum(involved[[n]] * probs) - sum(x[n,], na.rm = TRUE)   # first deriviative
  			fpp <- sum(involved[[n]] * probs * (1 - probs))
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


#' Estimate BTL Parameters
#'
#' Estimate ability.
#' @param x data matrix of pairwise comparisons
#' @param adjust_extreme Adjustment for extremes
#' @export
estimate_betas <- function(x, adjust_extreme = 0.25) {
	if(has_extremes(x)) {

		x_small <- remove_xtrms(x)  #x_small is list of data matrices
		x_extremes <- rev(xtrms(x_small))  # list of removed performances (one element per nesting level)
		data_matrices <- rev(c(list(x), x_small))# reverse order of lists: last nesting level (smallest) first.
		betas <- list()
		betas[[1]] <- estimate(data_matrices[[1]]) # smallest set of estimates first
		# feed estimate_anch()  with one nesting level at a time
		xil <- length(x_small)
		for (i in seq_len(xil)) {
			# x is next smallest data matrix
			betas[[i+1]] <- estimate_anch(data_matrices[[i+1]], x_extremes[[i]], betas[[i]]$b, adjust_extreme = adjust_extreme)
		}
		rv <- betas[[i+1]] # return value = last iteration of estimation
	} else {
		rv <- estimate(x)
	}
	rv
}

