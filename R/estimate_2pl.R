#' Estimate parameters of the two parameter logistic model
#' @export
estimate_2pl <- function(x,
                         convergence_criteria = c(0.01, 0.01, 0.01, 0.01, 0.001),
												 max_iterations = c(outer_loop = 20, beta_loop = 20, alpha_loop = 20, inner_loop = 20)) {

	N <- dim(x)[1]
	# set initial estimates
	beta <- rep(0, N)
	alpha <- rep(1, N)
	iterate_b_inner <- matrix(NA, nrow = max_iterations["inner_loop"], ncol = dim(x)[2]) # betas from inner loop
	iterate_a_inner <- matrix(NA, nrow = max_iterations["inner_loop"], ncol = dim(x)[2]) # alphas from inner loop
	convergence_beta <- matrix(NA, nrow = max_iterations["beta_loop"], ncol = dim(x)[2]) # betas from the outer loop
	convergence_alpha <- matrix(NA, nrow = max_iterations["alpha_loop"], ncol = dim(x)[2]) # alphas from the outer loop
	convergence_beta[1, ] <- 0
	convergence_alpha[1, ] <- 1
	se_beta <- matrix(NA, nrow = N, ncol = 1)
	se_alpha <- matrix(NA, nrow = N, ncol = 1)
	# convergence_criteria = inner_difficulty, outer_difficulty, inner_discrim., outer_discrim., outer
	# loop_size = outer_loop_size, beta_loop_size, beta_loop_inner, alpha_loop_size, alpha_loop_inner

	for (outer_loop_i in 1:max_iterations["outer_loop"]) {
	# difficulty ---------------------------------------------------------------------------------
		for (beta_loop_i in 2:max_iterations["beta_loop"]) {  # beta loop
		  	for (n in 1:N) {            # person loop
		  		comparisons_i <- which(!is.na(x[n, ]))
		  		beta_m <- beta[comparisons_i]   # beta_m is vector of person betas who are compared with person 'n'
		  		involved <- x[n, comparisons_i] + x[comparisons_i, n]
		  		for (i in 2:max_iterations["inner_loop"]) {
		  			probs <- exp(alpha[n] * (beta[n] - beta_m)) / (1 + exp(alpha[n] * (beta[n] - beta_m)))
		  			fp_beta <- alpha[n] * sum(involved * probs) - alpha[n] * sum(x[n,], na.rm = TRUE)   # first deriviative
		  			fpp_beta <- alpha[n]^2 * sum(involved * probs * (1 - probs))
		  			beta[n] <- beta[n] - fp_beta / fpp_beta
		  			iterate_b_inner[i, n] <- beta[n]
		  			if (!is.na(iterate_b_inner[i, n]) & !is.na(iterate_b_inner[i - 1, n]) &
		  					abs(iterate_b_inner[i, n] - iterate_b_inner[i-1, n]) <= convergence_criteria[1]) break}
		  		se_beta[n, ] <- 1 / sqrt(fpp_beta)
		  	}
		  	beta <- beta - mean(beta)
		  	convergence_beta[beta_loop_i, ] <- beta
		  	if (!any(is.na(convergence_beta[beta_loop_i, ])) & !any(is.na(convergence_beta[beta_loop_i -1, ])) &
		  			max(abs(convergence_beta[beta_loop_i , ] - convergence_beta[beta_loop_i -1, ])) < convergence_criteria[2]) break
		}

		# discrimination --------------------------------------------------------------------------------
		for (alpha_loop_i in 2:max_iterations["alpha_loop"]) {  # alpha loop
		  	for (n in 1:N) {            # item loop
		  		comparisons_i <- which(!is.na(x[ , n]))
		  		beta_m <- beta[comparisons_i]
		  		involved <- x[n, comparisons_i] + x[comparisons_i, n]
		  		for (i in 2:max_iterations["inner_loop"]) {
		  			probs <- exp(alpha[n] * (beta_m - beta[n])) / (1 + exp(alpha[n] * (beta_m - beta[n])))        # vector of probabilities for each comparison
		  			fp_alpha <- sum((beta_m - beta[n]) * (na.omit(x[ , n]) - t(involved * probs)))                   # scalar
		  			fpp_alpha <- sum(involved * probs * (involved - involved * probs) * (beta_m - beta[n])^2)     # scalar
		  			alpha[n] <- alpha[n] - fp_alpha * (-1) / fpp_alpha
		  			iterate_a_inner[i, n] <- alpha[n]
		  			if (!is.na(iterate_b_inner[i, n]) & !is.na(iterate_b_inner[i - 1, n]) &
		  					abs(iterate_b_inner[i, n] - iterate_b_inner[i-1, n]) <= convergence_criteria[3]) break}
		  		se_alpha[n, ] <- 1 / sqrt(fpp_alpha)
		  	}
		  	#alpha <- alpha - geo_mean(alpha)
		  	convergence_alpha[alpha_loop_i, ] <- alpha
		  	if (!any(is.na(convergence_alpha[alpha_loop_i, ])) & !any(is.na(convergence_alpha[alpha_loop_i -1, ])) &
		  			max(abs(convergence_alpha[alpha_loop_i, ] - convergence_alpha[alpha_loop_i -1, ])) < convergence_criteria[4]) break
		}

		# constrain alpha within item sets & centre using geometric mean of all alphas

		#if (!any(is.na(convergence[outer_loop_i, ])) & !any(is.na(convergence[outer_loop_i -1, ])) &
		#  max(abs(convergence[outer_loop_i , ] - convergence[outer_loop_i -1, ])) < convergence_criteria[5]) break


	}
	return(data.frame(performance = rownames(x), location = beta, discrimination = alpha, se = se_beta))
}
