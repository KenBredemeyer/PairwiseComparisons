#' Judge Characteristic Curves
#'
#' Plot left/right difference in logits against proportion of left selected for
#' each class interval.  Overlay probability curve.
#'
#' @param comparisons Data frame of comparisons with variables \code{Judge,
#'   Item, Item.1, Selected} and optionally \code{Criteria}.
#' @param estimates Data frame which inclues performance names and their
#'   estimated location in logits.
#' @param class_intervals Integer specifying the number of class intervals to
#'   form.
#'
#' @export
plot_JCCs <- function(comparisons, estimates, class_intervals) {
	stopifnot(!is.null(comparisons$Judge), !is.null(estimates$b), class_intervals %% 1 == 0,
		!any(is.na(estimates$b)))
	judges_comparisons <- split(comparisons, comparisons$Judge)
	judge_names <- attributes(judges_comparisons)$names

	# model curve
	x <- seq(-10, 10, 0.01)
	y <- exp(x) / (1+exp(x))

	for (judge_i in 1:length(judges_comparisons)) {
		# calculate difference in betas for each judges' comparisons
		judge_comparisons_betas <- dplyr::left_join(judges_comparisons[[judge_i]], estimates,
			                                          by = c("Item" = "name"))
		judge_comparisons_betas <- dplyr::left_join(judge_comparisons_betas, estimates,
	                                      by = c("Item.1" = "name"), suffix = c("_left", "_right"))
		judge_comparisons_betas <- cbind(judge_comparisons_betas, b_diff=judge_comparisons_betas$b_left - judge_comparisons_betas$b_right)

    # order on b_diff
    judge_comparisons_betas <- judge_comparisons_betas[order(judge_comparisons_betas$b_diff), ]

    # count left wins
		judge_comparisons_betas <- cbind(judge_comparisons_betas,
			                               left_wins = (judge_comparisons_betas$Selected == judge_comparisons_betas$Item) * 1)

		# add class interval numbers
		judge_comparisons_betas <- ci_index(judge_comparisons_betas, class_intervals)

		# calculate observed proportions
		ci_proportions <- aggregate(judge_comparisons_betas[, "left_wins"], list(judge_comparisons_betas$ci_index), proportion)
    ci_means <- aggregate(judge_comparisons_betas[, "b_diff"], list(judge_comparisons_betas$ci_index), mean)

    # plot
    plot(ci_means$x, ci_proportions$x,
    	   xlim = c(min(ci_means$x)-2, max(ci_means$x)+2), ylim = c(0, 1),
         main = judge_names[judge_i], sub = "Judge Characteristic Curve",
         xlab = "Logit Difference",
         ylab = "Expected Value")
    lines(x, y, col = "red")
	}
}

# add class interval index to a df as a column on the right
# class interval counts given as an attribute.
ci_index <- function(x, class_intervals) {
	stopifnot(is.data.frame(x), class_intervals %% 1 == 0)
	ci_numbers_min <- rep(dim(x)[1] %/% class_intervals, class_intervals)
	ci_numbers_extra <- rep(1, dim(x)[1] %% class_intervals)
	extra_counts_vec <- c(ci_numbers_extra, rep(0, length(ci_numbers_min) - length(ci_numbers_extra)))
	ci_reps <- ci_numbers_min + extra_counts_vec
	ci_index <- rep(1:class_intervals, ci_reps)
	rvalue <- cbind(x, ci_index)
	attr(rvalue, "ci_counts") <- list(ci_reps)
	rvalue
}

# return class interval counts
ci_counts <- function(x) {
  counts <- attributes(xxx)$ci_counts
  if (!exists("counts"))
  	stop("argument must be a data.frame returned from ci_index()")
  counts
}

# proportions
proportion <- function(x) {
	sum(x) / length(x)
}




