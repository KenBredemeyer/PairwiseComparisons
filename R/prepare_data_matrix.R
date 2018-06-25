# use 'format_data()' to run all.
options(stringsAsFactors = FALSE)

# preference codes ------------------------------------------------------
pref_codes <- function(x) {
  lr_code <- apply(x, 1, function(x) which(x[c("Item", "Item.1")] == x["Selected"]))
  x["Selected"] <- lr_code
  x
}

# left preferred ----------------------------------------------------------
left_preferred <- function(d) {
	d <- apply(d, 1, function(x) {if(x["Selected"] == 2) x[c("Item", "Item.1")]<- x[c("Item.1", "Item")];x})
	d <- data.frame(t(d))
	d[,"Selected"] <- rep(1, nrow(d))
  d
}

# aggregagte comparisons --------------------------------------------------
# ignores judge and criteria
add_comparisons <- function(x) {
	combined <- aggregate(x, x[,c("Item", "Item.1")], length)
	colnames(combined) <- c("selected", "not_selected", "counts")
	combined <- combined[ , 1:3]
}

# 3column complete --------------------------------------------------------
#reversed_rows means rows containing comparisons with reversed l/r presentation
# complete by adding 0 scores to comparisons involved, but not left_preferred
complete_comparisons <- function(x) {
	zero_win_rows <- vector()
	j <- 1
	for (i in 1:nrow(x)) {
		if(!any(x[ , 2] == x[i, 1] & x[ , 1] == x[i, 2])) {
	    zero_win_rows[j] <- i
	    j <- j+1
		}
	}
	zero_wins <- data.frame(matrix(ncol = 3))
	colnames(zero_wins) <- c("selected", "not_selected", "counts")
	zero_wins[(1:j-1), 1:2] <- x[zero_win_rows, 2:1]
	zero_wins[ , 3] <- 0
	if (j > 1) {
		x <- rbind(x, zero_wins)
	}
  x
}

# reshape -----------------------------------------------------------------
data_matrix <- function(x) {
	wide_data <- reshape(x, idvar = "selected", timevar = "not_selected", direction = "wide")
	wide_data

	rownames(wide_data) <- wide_data[ , 1]; wide_data <- wide_data[ , -1]
	colnames(wide_data) <- unique(x$not_selected)

	# make symetrical
	wide_data <- wide_data[order(rownames(wide_data)), order(colnames(wide_data))]
	wide_data
}

# remove xtrms ------------------------------------------------------------
# remove nested extremes.  High and low extremes returned
# as attributes of the reduced data matrix
remove_xtrms <- function(data_matrix) {
	d <- dim(data_matrix)
	high_xtrm <- list()
	low_xtrm <- list()
	i <- 1
	# remove high extremes
	if (any(colSums(data_matrix, na.rm = TRUE) == 0)) {
		high_xtrm_i <- which(colSums(data_matrix, na.rm = TRUE) == 0)
		high_xtrm[[i]] <- colnames(data_matrix)[high_xtrm_i]
		data_matrix <- data_matrix[-high_xtrm_i , -high_xtrm_i]
	}
	# remove low extremes
	if (any(rowSums(data_matrix, na.rm = TRUE) == 0)) {
		low_xtrm_i <- which(rowSums(data_matrix, na.rm = TRUE) == 0)
		low_xtrm[[i]] <- rownames(data_matrix)[low_xtrm_i]
		data_matrix <- data_matrix[-low_xtrm_i, -low_xtrm_i]
	}
	while(dim(data_matrix)[1] < d[1]) { # repeat removing extrms while matrix gets smaller
		d <- dim(data_matrix)
		i <- i + 1
		# remove high extremes
		if (any(colSums(data_matrix, na.rm = TRUE) == 0)) {
			high_xtrm_i <- which(colSums(data_matrix, na.rm = TRUE) == 0)
			high_xtrm[[i]] <- colnames(data_matrix)[high_xtrm_i]
			data_matrix <- data_matrix[-high_xtrm_i , -high_xtrm_i]
		}
		# remove low extremes
		if (any(rowSums(data_matrix, na.rm = TRUE) == 0)) {
			low_xtrm_i <- which(rowSums(data_matrix, na.rm = TRUE) == 0)
			low_xtrm[[i]] <- rownames(data_matrix)[low_xtrm_i]
			data_matrix <- data_matrix[-low_xtrm_i, -low_xtrm_i]
		}
		if (dim(data_matrix)[1] < 3) stop("data matrix too small. check nested extremes.")
	}
  attr(data_matrix, "extremes") <- list(c(unlist(low_xtrm), unlist(high_xtrm)))
	data_matrix
}

# extract extremes
xtrms <- function(data_matrix) {
	extremes <- attributes(data_matrix)$extremes[[1]]
}

# extract full data matrix
full_matrix <- function(data_matrix) {
	full_matrix <- attributes(data_matrix)$data_matrix[[1]]
}

# convert data matrix to proportions --------------------------------------
proportions <- function(data_matrix) {
	p_matrix <- matrix(NA, nrow = nrow(data_matrix), ncol = ncol(data_matrix))
	for (i in 1:nrow(data_matrix)) {
		for (j in 1:ncol(data_matrix)) {
			if(!is.na(data_matrix[i, j])) {
			p_matrix[i, j] <- data_matrix[i, j] / (data_matrix[i, j] + data_matrix[j, i])
			}
		}
	}
	rownames(p_matrix) <- rownames(data_matrix)
	colnames(p_matrix) <- colnames(data_matrix)
	p_matrix
}


#   -----------------------------------------------------------------------
## sum so that NA + NA = NA, and NA + 0 = 0
sum2 <- function(x) {
	if (!all(is.na(x))) {
		sum(x, na.rm = TRUE)
	} else {
		sum(x, na.rm = FALSE)
	}
}
