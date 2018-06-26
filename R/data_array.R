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

## build in criteria ! ##
