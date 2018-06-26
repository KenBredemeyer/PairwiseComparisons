complete_comparisons2 <- function(x) {
	zero_win_rows <- vector()
	j <- 1
	Judges <- unique(test_tile_tower$Judge)
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

#' Create data matrix for estimation
#'
#' @param x data.frame containing the variables \code{Judge, Criteria, Item,
#'   Item.1, Selected}.
#' @param judges Numeric or character vector specifying judges to include.
#' @param criteria Numeric or character vector specifying criteria to include.
#' @export
pairs_format2 <- function(x, judges = "all", criteria = "all") {
	options(stringsAsFactors = FALSE)
	if (length(judges) == 1 && judges == "all") judges <- judges(x)
	if (length(criteria) == 1 && criteria == "all") criteria <- criteria(x)
	x <- pref_codes(x)
	x <- left_preferred(x)
  x <- complete_comparisons2(x)
  x <- reshape2::acast(x,
	                Item ~ Item.1 ~ Judge ~ Criteria,
	                drop = FALSE,
	                value.var = "Selected")
  x <- x[ , , judges, criteria]
  # flatten to data matrix
  x <- apply(x, 1:2, sum2)
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


