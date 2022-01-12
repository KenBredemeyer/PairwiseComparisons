#' Geometric Mean
#' @export
geo_mean <- function(x) {
	stopifnot(is.vector(x))
	n <- length(x)
	prod(x)^(1/n)
}
