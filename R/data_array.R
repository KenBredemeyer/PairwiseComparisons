#' Create data matrix for estimation
#'
#' Sum across criteria to estimate judge discrimination, and sum across judges
#' to estimate criteria discrimination.
#'
#' @param x data.frame containing the variables \code{Judge, Criteria, Item,
#'   Item.1, Selected}.
#' @param judges Numeric or character vector specifying judges to include.
#' @param criteria Numeric or character vector specifying criteria to include.
#' @param sum_across Character vector. \code{"judges"}, \code{"criteria"} or
#'   \code{c("judges", "criteria")}
#'
#' @return A data matrix (2D) if \code{sum_across = "judges"} or
#'   \code{sum_across = "criteria"}, or if \code{sum_across = c("judges",
#'   "criteria")} then a 3D array is returned.
#' @export
pairs_format2 <- function(x, judges = "all", criteria = "all", sum_across = c("judges", "criteria")) {
	options(stringsAsFactors = FALSE)
	# allow a 2D or 3D array to be returned.  Must sum across judges, or criteria, or both
	stopifnot(any(sum_across == "judges") | any(sum_across == "criteria"))
	if (length(judges) == 1 && judges == "all") judges <- unique(x$Judge)
	if (length(criteria) == 1 && criteria == "all") criteria <- unique(x$Criteria)
	x <- pref_codes(x)
	x <- left_preferred(x)
  x <- complete_comparisons2(x)
  x <- reshape2::acast(x,
	                Item ~ Item.1 ~ Judge ~ Criteria,
	                drop = FALSE,
	                value.var = "Selected")
  x <- x[ , , judges, criteria, drop = FALSE]
  if (any(sum_across == "judges") && any(sum_across == "criteria")) {
	  # flatten to data matrix
	  x <- apply(x, 1:2, sum2)
  } else if (all(sum_across == "judges")) {
  	x <- apply(x, c(1,2,4), sum2)
  } else if (all(sum_across == "criteria")) {
  	x <- apply(x, 1:3, sum2)
  }
  x
}


#' List all judges
#' @param x data.frame containing the variable \code{Judge}
#' @export
judges <- function(x) unique(x$Judge)

#' List all criteria
#' @param x data.frame containing the variable \code{Criteria}
#' @export
criteria <- function(x) unique(x$Criteria)


complete_comparisons2 <- function(x) {
	zero_win_rows <- vector()
	j <- 1
	Judges <- unique(x$Judge)
	for (i in 1:nrow(x)) {
		judge <- x[i, "Judge"]
		criterion <- x[i, "Criteria"]
		if(!any(x[x$Judge == judge & x$Criteria == criterion, "Item.1"] == x[i, "Item"] &
				    x[x$Judge == judge  & x$Criteria == criterion, "Item"] == x[i, "Item.1"])) {
	    zero_win_rows[j] <- i
	    j <- j+1
		}
	}
	zero_wins <- data.frame(matrix(ncol = ncol(x)))
	colnames(zero_wins) <- colnames(x)
	zero_wins[(1:j-1), c("Item", "Item.1")] <- x[zero_win_rows, c("Item.1", "Item")]
	zero_wins[ , "Selected"] <- 0
	zero_wins[ , -which(names(zero_wins) %in% c("Item", "Item.1", "Selected"))] <- x[ , -which(names(x) %in% c("Item", "Item.1", "Selected"))]
	if (j > 1) {
		x <- rbind(x, zero_wins)
	}
  x
}

