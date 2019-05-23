# Judge Characteristic Curves


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





