#' Allocate locations to performance names, using random distributions
#'
#' @param performances Character vector of performance names
#' @param distribution Character, either \code{"normal"} or \code{"uniform"}.
#'
#' @return data.frame of performances and locations.
#' @export
sim_betas <- function(performances, distribution = "normal", mean, sd, min, max) {
  stopifnot(any(distribution == c("uniform", "normal")))
	if (distribution == "normal" && missing(mean) && missing(sd)) {
		mean = 0; sd = 1
	}
	x <- data.frame(performance = performances)
  nrows_x <- dim(x)[1]

	if (distribution == "uniform") {
	  x$location <- stats::runif(nrows_x, min, max)
	} else if(distribution == "normal") {
		x$location <- stats::rnorm(nrows_x, mean, sd)
	}
  x
}



#' Simulate pairwise judgements
#'
#' @param performances data frame containing \code{performance} and \code{location}.
#' @param pairs List of pairs, one list element for each judge.
#' @param judges integer specifying the number of judges, or character vector
#'   giving the names of judges.
#' @param criteria integer specifying the number of criteria, or character
#'   vector giving the names of criteria.
#' @param judge_alpha Vector equal in length to the number of judges, providing
#'   the discrimination parameter as a property of judges.
#' @export
simulate_pw <- function(performances, pairs, judges, criteria=1,
	                      judge_alpha =1) {
	stopifnot(class(performances) == "data.frame",
		        colnames(performances) == c("performance", "location"))

	if (judges == 1) {
		j1pairs <- pairs
		pairs <- list()
		pairs[[1]] <- j1pairs
	}
  if (is.numeric(judges) && length(judges) == 1) {
  	judges <- paste0("Judge_", formatC(1:judges, width = nchar(judges), format = "d", flag = "0"))
  }
  if (is.numeric(criteria) && length(criteria) == 1) {
  	criteria <- paste0("Criterion_", formatC(1:criteria, width = nchar(criteria), format = "d", flag = "0"))
  }

	P <- function(b1, b2, alpha) {
		exp(alpha*(b1 - b2)) / (1 + exp(alpha*(b1 - b2)))
	}

	judgements <- list()
	sim_judgements <- list()
	for (judge_i in seq(judges)) {
		judgements[[judge_i]] <- dplyr::left_join(pairs[[judge_i]], performances, by = c("left" = "performance"))
		colnames(judgements[[judge_i]])[dim(judgements[[judge_i]])[2]] <- "left_beta"
		judgements[[judge_i]] <- dplyr::left_join(judgements[[judge_i]], performances, by = c("right" = "performance"))
		colnames(judgements[[judge_i]])[dim(judgements[[judge_i]])[2]] <- "right_beta"    # add suffix instead?

    # repeat comparisons for each criterion
		criteria_col <- rep(criteria, nrow(judgements[[judge_i]]))
		if (length(criteria) > 1) {
		judgements[[judge_i]] <- judgements[[judge_i]][rep(seq_len(nrow(judgements[[judge_i]])), each = length(criteria)), ]
		}

		left_wins <- stats::rbinom(dim(judgements[[judge_i]])[1], 1,
	                           P(judgements[[judge_i]][,"left_beta"], judgements[[judge_i]][,"right_beta"], judge_alpha[judge_i]))

		Selected <- vector("character", length = length(left_wins))
		for (i in 1:length(left_wins)) {
			if (left_wins[i]) {
				Selected[i] <- judgements[[judge_i]]$left[i]
			} else if (!left_wins[i]) {
				Selected[i] <- judgements[[judge_i]]$right[i]
			}
		}
	  judge_col <- rep(judges[[judge_i]], nrow(judgements[[judge_i]]))
		sim_judgements[[judge_i]] <- cbind(judge_col, criteria_col, judgements[[judge_i]], Selected)
		colnames(sim_judgements[[judge_i]])[1:4] <- c("Judge", "Criteria", "Item", "Item.1")
	}

	do.call(rbind, sim_judgements)
}
