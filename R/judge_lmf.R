# @param x 3D array: 3rd D judges
npersons <- dim(x)[1]
njudges <- dim(x)[3]

# initialise lists for difficulty section
comparisons_i_judge <- list()
probs_judge <- list()
dm_counts_judge <- list()
involved_judge <- list()
fp_beta_judge <- list()
fpp_beta_judge <- list()

# set initial estimates
se_beta <- rep(0, npersons)
se_alpha <- rep(1, njudges)

# initialise convergence matrices

# initialise standard error objects
se_beta <- matrix(NA, nrow = npersons, ncol = 1)
se_alpha <- matrix(NA, nrow = njudges, ncol = 1)


# difficulty
 for (n in seq_len(npersons)) {    # person loop
  inner loop {
    for (alpha_i in 1:njudges) {   # gather information across "virtual items"
    comparisons_i_judge[[alpha_i]] <- which(!is.na(x[n, , alpha_i]))
    probs_judge[[alpha_i]] <- exp(alpha[alpha_i] * (beta[n] - beta_m)) /
    	                   (1 + exp(alpha[alpha_i] * (beta[n] - beta_m)))
    dm_counts_judge[[alpha_i]] <- x[item_i , , alpha_i]
    involved_judge[[alpha_i]] <- na.omit(x[item_i , , alpha_i]) + na.omit(x[ , item_i, alpha_i])

    fp_beta_judge[[alpha_i]] <- alpha[[alpha_i]] * sum(involved_judge[[alpha_i]] * probs_judge[[alpha_i]])
                          - alpha[[alpha_i]] * sum(dm_counts_judge[[alpha_i]], na.rm = TRUE)
    fpp_beta_judge[[alpha_i]] <- alpha[[alpha_i]]^2 * sum(involved_judge[[alpha_i]] * probs_judge[[alpha_i]] * (1 - probs_judge[[alpha_i]])
    }
    # combine data across judges
    comparisons_i <- do.call(c, comparisons_i_judge)
    comparisons_count <- lapply(comparisons_i_judge, length)
    probs <- do.call(c, probs_judge)
    dm_counts <- do.call(c, dm_counts_judge)
    involved <- do.call(c, involved_judge)

    fp_beta <- do.call(sum, fp_beta_judge)
    fpp_beta <- do.call(sum, fpp_beta_judge)

    # update estimates
    beta[n] <- beta[n] - fp_beta / fpp_beta

    # inner loop convergence

  }
  se_beta[n, ] <- 1 / sqrt(fpp_beta)

  beta <- beta - mean(beta)

  # beta convergence

}


# discrimination
