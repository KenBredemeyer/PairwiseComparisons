#' Fit the Logistic Measurement Function
#'
#' Quantify performance locations and discrimination for judges.
#'
#' @param x 3D data matrix (performances \* performances \* judges)
#' @param convergence_criteria Stopping criteria
#' @param max_iterations Maximum iterations
#' @references Humphry, S. M., Andrich, D. (Supervisor) (2005). Maintaining a
#'   common arbitrary unit in social measurement. Doctoral thesis.
#'   \url{https://librarysearch.murdoch.edu.au/discovery/delivery?vid=61MUN_INST:ResearchRepository&repId=12129673560007891#13130840450007891}
#'
#' @export
judge_lmf <- function(x,
                      convergence_criteria = c(0.01, 0.01, 0.01, 0.01, 0.001),
                      max_iterations = c(outer_loop = 20, beta_loop = 20, alpha_loop = 20, inner_loop = 20)) {

  # @param x 3D array: 3rd D judges
  npersons <- dim(x)[1]
  njudges <- dim(x)[3]

  # initialise lists for difficulty section
  comparisons_i_judge <- list()
  beta_m_judge <- list()
  probs_judge <- list()
  dm_counts_judge <- list()
  involved_judge <- list()
  fp_beta_judge <- list()
  fpp_beta_judge <- list()
  fp_alpha_judge <- list()
  fpp_alpha_judge <- list()

  probs <- matrix(NA, nrow = nrow(x), ncol = ncol(x))
  involved <- matrix(NA, nrow = nrow(x), ncol = ncol(x))
  loc_diff <- matrix(NA, nrow = nrow(x), ncol = ncol(x))
  fp_alpha <- vector("numeric", length = njudges)
  fpp_alpha <- vector("numeric", length = njudges)

  # set initial estimates
  beta <- rep(0, npersons)
  alpha <- rep(1, njudges)
  se_beta <- matrix(NA, nrow = npersons, ncol = 1)
  se_alpha <- matrix(NA, nrow = njudges, ncol = 1)

  # initialise convergence matrices
  convergence_beta_inner <- matrix(NA, nrow = max_iterations["inner_loop"], ncol = dim(x)[2])
  convergence_alpha_inner <- matrix(NA, nrow = max_iterations["inner_loop"], ncol = dim(x)[3])
  convergence_beta <- matrix(NA, nrow = max_iterations["beta_loop"], ncol = dim(x)[2]) # betas from the outer loop
  convergence_alpha <- matrix(NA, nrow = max_iterations["alpha_loop"], ncol = dim(x)[3]) # alphas from the outer loop
  convergence_beta[1, ] <- 0
  convergence_alpha[1, ] <- 1

  # initialise standard error objects
  se_beta <- matrix(NA, nrow = npersons, ncol = 1)
  se_alpha <- matrix(NA, nrow = njudges, ncol = 1)


  # outer loop
  for (outer_loop_i in 1:max_iterations["outer_loop"]) {
    # difficulty ------------------------------------------------------------------------------
    for (beta_loop_i in 2:max_iterations["beta_loop"]) {
      for (n in seq_len(npersons)) {    # person loop
        for (i in 2:max_iterations["inner_loop"]) {   # inner loop
          for (alpha_i in 1:njudges) {   # gather information across "virtual items"
            comparisons_i_judge[[alpha_i]] <- which(!is.na(x[n, , alpha_i]))
            beta_m_judge[[alpha_i]] <- beta[comparisons_i_judge[[alpha_i]]]
            probs_judge[[alpha_i]] <- exp(alpha[alpha_i] * (beta[n] - beta_m_judge[[alpha_i]])) /
                	                   (1 + exp(alpha[alpha_i] * (beta[n] - beta_m_judge[[alpha_i]])))
            dm_counts_judge[[alpha_i]] <- x[n , , alpha_i]
            involved_judge[[alpha_i]] <- na.omit(x[n , , alpha_i]) + na.omit(x[ , n, alpha_i])

            fp_beta_judge[[alpha_i]] <- alpha[[alpha_i]] * sum(involved_judge[[alpha_i]] * probs_judge[[alpha_i]]) -
                                        alpha[[alpha_i]] * sum(dm_counts_judge[[alpha_i]], na.rm = TRUE)
            fpp_beta_judge[[alpha_i]] <- alpha[[alpha_i]]^2 * sum(involved_judge[[alpha_i]] *
                                                 probs_judge[[alpha_i]] * (1 - probs_judge[[alpha_i]]))
          }
        # combine data across judges
        fp_beta <- do.call(sum, fp_beta_judge)
        fpp_beta <- do.call(sum, fpp_beta_judge)

        # update estimates
        beta[n] <- beta[n] - fp_beta / fpp_beta

        # inner loop convergence
        convergence_beta_inner[i, n] <- beta[n]
        # stopping criterion
        if (!is.na(convergence_beta_inner[i, n]) && !is.na(convergence_beta_inner[i-1, n]) &&
            abs(convergence_beta_inner[i, n] - convergence_beta_inner[i-1, n]) <= convergence_criteria[1]) {
          break
        }
      }
        se_beta[n, ] <- 1 / sqrt(fpp_beta)
      }
      beta <- beta - mean(beta)
      # beta convergence and stopping criterion
      convergence_beta[beta_loop_i-1, ] <- beta
      if (!any(is.na(convergence_beta[beta_loop_i, ])) && !any(is.na(convergence_beta[beta_loop_i -1, ])) &&
        max(abs(convergence_beta[beta_loop_i, ] - convergence_beta[beta_loop_i -1, ])) < convergence_criteria[2]) {
        break
      }
    }
    # discrimination -------------------------------------------------------------------------------------
    for (alpha_loop_i in 2:max_iterations["alpha_loop"]) {
      for (alpha_i in 1:njudges) {                          #  judge loop
        for (iteration_i in 2:max_iterations["inner_loop"]) {           # inner loop
        	for (i in 1:npersons) {
        		for (j in 1:npersons) {
        			if (!is.na(x[i, j, alpha_i])) {
        				probs[i, j] <- exp(alpha[alpha_i] * (beta[i] - beta[j])) / (1 + exp(alpha[alpha_i] * (beta[i] - beta[j])))
        				involved[i, j] <- x[i, j, alpha_i] + x[j, i, alpha_i]
        				loc_diff[i, j] <- beta[i] - beta[j]
        			}
        		}
        	}
        	fp_alpha[alpha_i]	<- sum(loc_diff * (x[ , , alpha_i] - probs * involved), na.rm = TRUE)
          fpp_alpha[alpha_i] <- sum(loc_diff^2 * involved^2 * probs * (1 -  probs), na.rm = TRUE)

        	alpha[alpha_i] <- alpha[alpha_i] + fp_alpha[alpha_i] / fpp_alpha[alpha_i]
          convergence_alpha_inner[iteration_i, alpha_i] <- alpha[alpha_i]
    			if (!is.na(convergence_alpha_inner[iteration_i, alpha_i]) && !is.na(convergence_alpha_inner[iteration_i - 1, alpha_i]) &
    					abs(convergence_alpha_inner[iteration_i, alpha_i] - convergence_alpha_inner[iteration_i-1, alpha_i]) <= convergence_criteria[3]) {
    			  break
    			}
          se_alpha[alpha_i, ] <- 1 / sqrt(fpp_alpha[alpha_i] * 1/2)   # half information function for aggregating across dm.
        }
      }
      alpha <- alpha / geo_mean(alpha)
    	convergence_alpha[alpha_loop_i, ] <- alpha
    	if (!any(is.na(convergence_alpha[alpha_loop_i, ])) & !any(is.na(convergence_alpha[alpha_loop_i -1, ])) &
    			max(abs(convergence_alpha[alpha_loop_i, ] - convergence_alpha[alpha_loop_i -1, ])) < convergence_criteria[4]) {break}
    }
  }

  result <- list(Performances = data.frame(performance = rownames(x),
  		                                     location = round(beta, 3),
  		                                     se = round(se_beta, 3)),
                 Judges = data.frame(names = dimnames(x)[[3]],
                                     discrimination = round(alpha, 3),
                                     se = round(se_alpha, 3))
                 )
}
