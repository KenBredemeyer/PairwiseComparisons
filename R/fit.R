#' Person Separation Index
#' @param estimates data frame of output, from \code{estimtea_BTL}, or similar
#' containing the variables \code{b} and \code{se}
#'
#' @export
PSI <- function(estimates) {
	stopifnot(!is.null(estimates$b), !is.null(estimates$se))
	MSE <- mean(estimates$se^2)
	(var(estimates$b) - MSE) / var(estimates$b)
}
