# @param x 3D array: 3rd D judges
njudges <- dim(x)[3]

comparisons_i <- list()


# difficulty
person loop (n in item_i) {
  inner loop {
    for (alpha_i in 1:njudge) {   # gather information across "virtual items"
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
