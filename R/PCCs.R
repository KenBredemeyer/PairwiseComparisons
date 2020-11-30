#' Produce Person Characteristic Curves
#'
#' Choose input file interactively
#' @export
PCCs <- function(nci = 4) {
	fileIn <- file.choose()

	if (substring(fileIn, nchar(fileIn)-2, nchar(fileIn)) == "csv") {
	  pw_long_full <- read.csv(fileIn, header = TRUE, colClasses = rep("character", 3))

	} else if (substring(fileIn, nchar(fileIn)-2, nchar(fileIn)) == "lsx") {
	  require(xlsx)
	  pw_long_full <- read.xlsx(fileIn,  sheetIndex = 1, header = TRUE, colIndex = 1:6,
	                              colClasses = rep("character", 6), stringsAsFactors=FALSE)
	} else
	  message("sorry, file extension not recognized.  Please use .csv or .xlsx")

	pw_long_full <- pw_long_full[,c("Item", "Item.1", "Preferred")]
	str(pw_long_full)
	rm(fileIn)


	# Manipulate pw data from long_full to long & wide formats                       17/12/2015
	# data read in from "read pw long_f.R" as "pw_long_full"


	pw_data <- pw_long_full
	pw_data[,1] <- gsub("-", "_", pw_data[,1])
	pw_data[,2] <- gsub("-", "_", pw_data[,2])
	pw_data[,3] <- gsub("-", "_", pw_data[,3])
	head(pw_data)


	# Rearrange Script 1 as preferred
	l <- length(pw_data[,"Preferred"])
	players <- pw_data[,1:2]
	pref <- pw_data[,3]
	z <- matrix(NA, nrow=l, ncol=1)
	for (i in 1:l) {
	  z[i] <- which(players[i,]!=pref[i])}    # comp. effort
	pw.dat <- matrix(NA, nrow=l, ncol=3)
	pw.dat[,1] <- pw_data[,3]
	for (i in 1:l) {
	  pw.dat[i,2] <- pw_data[i,z[i]] }

	# Collapse repeated comparison/preferred
	colnames(pw.dat) <- c("wins","against","")
	suppressMessages(library(data.table))
	pw.datT <- data.table(pw.dat)
	win_counts <- pw.datT[, .N, c("wins","against")]
	rm(pw.datT)

	# reshape long to wide
	pw.wide <- reshape(win_counts, idvar="wins", timevar="against", direction="wide")
	# tidy up
	rm(i, l, z, pw_long_full, pw.dat, players, pref)

	## pw.wide convert to square matrix
	sw <- data.frame(pw.wide)            #scores_wide
	rownames(sw) <- sw[,1]
	sw <- sw[,-1]

	sw <- sw[order(rownames(sw)),order(unique(win_counts$against))]        # (produces alphabetical ordering of scripts - need to have some way of ordering rows and columns the same)

	if (all(substring(colnames(sw), 1, 2) == "N.")) {
	  colnames(sw) <- substring(colnames(sw), 3)
	}

	identical(rownames(sw), colnames(sw))           #TRUE for NAPLAN data set

	# remove columns with no positive comparisons from the data matrix
	if (identical(rownames(sw), colnames(sw))) {
	  message(" Identical rows and columns \n no low extremes ")
	} else {
	  notRow <- which(colnames(sw) == setdiff(colnames(sw), rownames(sw)))
	  if ((dim(sw)[2] - dim(sw)[1]) != length(setdiff(colnames(sw), rownames(sw)))) {
	    stop("formatting error! Please check the script labels for illegal characters (/, +, -, etc)")
	  }
	  message(" Deleting the following scripts, which have no favourable comparisons") ; print(setdiff(colnames(sw), rownames(sw)))
	  sw <- sw[ , -notRow]
	}

	if (!identical(colnames(sw), rownames(sw))) {
	  stop(" A formatting error has occured: data matrix (sw) should have the same row and column names")
	}

	# check NA diag. (replace message() with stop() )
	if (!is.na(all(diag(as.matrix(sw))))) {
	  message(" WARNING, formatting error. ")
	}

	wide_m <- sw                                        # sw from "ORDER(for format pw).R"

	# find and remove xtrm scripts w no losses
	zero <- which(colSums(sw) == 0)
	if (length(zero) == 0) {
	  message("The data contain no high extremes")
	} else {
	  wide_m <- wide_m[-zero, -zero]
	  message("the following (extreme) scripts have been removed, which have only positive comparisons"); print(colnames(wide_m)[zero])
	}

	# add zeros to the matrix where they belong
	for (i in 1:nrow(wide_m)) {
	  for (j in 1:nrow(wide_m)) {
	    if(!is.na(wide_m[i,j]) & is.na(wide_m[j,i])) {
	      wide_m[j,i] <- 0
	    }
	  }
	}
	#rm(sw, pw_data, win_counts, x, zero, pw.wide, colsum, notRow)

	# Number of comparisons minus those involved with extremes
	n_comparisons <- sum(rowSums(wide_m, na.rm = TRUE))   # since No. comparisons = No. preferred
	# Number of non-zero elements (need the number of criteria to convert this number to No. involved)
	# noNA <- sum(!is.na(wide_m))


	# convert data matrix to proportions
	p_data <- matrix(NA, nrow = nrow(wide_m), ncol = nrow(wide_m))
	for (i in 1:nrow(wide_m)) {
	  for (j in 1:nrow(wide_m)) {
	    p_data[i, j] <- wide_m[i, j]/(wide_m[i, j] + wide_m[j, i])
	  }
	}

	rownames(p_data) <- rownames(wide_m)
	colnames(p_data) <- colnames(wide_m)


	# estimate

	# Pairwise estimation algorithm                                   (4/04/2016)


	# Specify convergence criteria:
	ic <- 0.001                              # convergence criteria for (inner) iteration loop
	tc <- 0.001                              # convergence criteria for outer loop

	lf <- function (bn, bm) {
	  exp(bn - bm)/(1 + exp(bn - bm)) }

	# allocate response pattern
	x <- p_data

	N <- length(x[,1])                      # beta for each row
	b  <- rep(0, N)                         # initial values for person abilities
	v  <- matrix(0, nrow = 30, ncol = dim(x)[2])    # stores iterations of person estimates - inner loop
	convergence <- matrix(0, nrow = 100, ncol = dim(x)[2])     # stores iterations of person estimates - outer loop
	se <- matrix(NA, nrow = N, ncol = 1)


	for (ot in 2:100) {                        # outer loop
	# person n:
	  for (n in 1:N) {                       # person loop
	    bm <- b[which(!is.na(x[n, ]))]                          # so m != n
	    for (i in 2:30) {                    # iteration loop
	      probs <- lf(b[n], bm)
	      fp <- sum(probs) - sum(x[n,], na.rm = TRUE)
	      fpp <- sum(probs*(1 - probs))
	      b[n] <-  b[n] - fp/fpp
	      v[i,n] <- b[n]
	      if (!is.na(v[i,n]) & !is.na(v[i-1,n]) & abs(v[i,n] - v[i-1,n]) <= ic) break }
	    se[n, ] <- 1/sqrt(fpp)
	  }
	  b <- b - mean(b)
	  convergence[ot,] <- b
	  if (!is.na(v[i,n]) & !is.na(v[i-1,n]) & max(abs(convergence[ot,] - convergence[ot-1,])) < tc) break
	}


	names(b) <- colnames(x)
	b[order(b)]
	rm(x)

	# reorder on ability
	p_data <- p_data[order(b), order(b)]
	b <- as.vector(b)
	b <- b[order(b)]
	p_data <- cbind(p_data, b)
	#if (suppressWarnings(!is.na(sum(as.numeric(substring(colnames(p_data), 3)))))) {
	#  colnames(p_data) <- substring(colnames(p_data), 3)  }                           # doesn't seem to work.
	#p_data

	psi <- (var(b) - sum(se^2)/N)/var(b)


	# Display top and bottom 30 scripts.
	#data.frame(head(p_data[, "b"], 30))
	#data.frame(tail(p_data[, "b"], 30))



	##  class intervals pw.R
	# Form class intervals prior to calculating class means
	# Run this file after estimation                                           (10/12/2015)

	# this file must be able to be run repeatedly.
	# Do not modify objects from Estimation2.R: p_data, b.

	#numbers of comparisons per script
	nc <- apply(p_data, 2, function(x) length(na.omit(x)))
	message(" Min comparisons per script is ")
	min(nc)

	# Form class intervals
	# nci = number of class intervals
	pw <- vector("list", ncol(p_data)-1)
	index <- vector("list", ncol(p_data)-1)                    # index is a list of which ordered scripts are involved in the comparison
	index <- apply(p_data[,-(dim(p_data)[2])], 2, function(x) which(!is.na(x)))
	index <- lapply(index, function(x) unname(x, force = TRUE))


	#print("  Enter number of class intervals ")
	#nci <- scan(file = "", what = "", nmax = 1)    # <----  enter number of class intervals
	nci <- as.integer(nci)
	for (i in 1:length(index)) {
	  cicn <- (length(index[[i]]))/nci  #   average number of scripts in each class interval
	  l_o <- cicn - floor(cicn)

	  r1 <- (1 - l_o) * nci
	  r2 <- l_o*nci
	  a <- rep(floor(cicn), round(r1, 0))
	  bb <- rep(ceiling(cicn), round(r2, 0))
	  a <- data.frame(a)                            # rbind.fill needs data frames as arguments.
	  bb <- data.frame(bb)
	  pw.r <- vector("numeric", r1+r2)
	  pw.r[] <- na.omit(unlist(plyr::rbind.fill(a, bb)))   # na.omit removes NA formed by rbind.fill  (not NAs in data)
	# rm(a, bb)                                    # remove obselete variables when file is complete.

	  x <- c(1:nci)        # class interval number
	  pw[[i]] <- rep(x, round(pw.r, 0))
	# rm(x, r1, r2, cicn, nci)
	}





	#   p_data[index[[2]], 2]
	#unname(p_data[index[[2]], 2])
	comparisons <- cbind(unname(p_data[index[[2]], 2]), b[(index[[2]])], pw[[2]])

	# Create a matrix of comparisons for each script (a list of matrices)
	comparison <- vector("list", nrow(p_data))
	for (i in 1:nrow(p_data)) {
	  comparison[[i]] <- cbind(unname(p_data[index[[i]], i]), b[(index[[i]])], pw[[i]])
	  colnames(comparison[[i]]) <- c("Proportions", "Locations", "class interval")
	}

	# check:
	comparison[[200]]

	# run after "class intervals pw.R"

	#library(data.table)
	par(bg = "antiquewhite")     # set background colour
	xx <- seq(-13, 13, 0.01)

	rm(y)                        # remove previously defined "y"
	y <- vector("numeric", length(xx))
	# store mean proportion and ability for each CI for all scripts
	means_all <- vector("list", nrow(p_data))

	#par(ask = TRUE)
	for (n in 1:nrow(p_data)) {
	  # calculate mean proportions & mean locations
	  compare <- data.table(comparison[[n]])
	  means <- compare[ , .(mean_p = mean(Proportions), mean_a = mean(Locations)), by = "class interval"]
	  means <- data.frame(means)   # with = FALSE  (normal subsetting)
	  means_all[[n]] <- means

	  #points for plotting
	  for (i in 1:length(xx)) {
	    y[i] <-  exp(xx[i] - b[n])/(1 + exp(xx[i] - b[n]))
	  }

	  txt <- rle(pw[[n]])$lengths
	  # plot window range = range of ability estimates (for the group of persons)
	  plot(xx, y, xlim = c(min(b)-3, max(b)+3), ylim = c(0, 1.1), type = "l", xlab = "PCC for script ID",
	    ylab = "Expected Value", col="red", main = "Person Characteristic Curve", sub = rownames(p_data)[n])
	  axis(side=1, tick=TRUE, at= round(b[n], 3), padj=1, lwd.ticks = 2, col.ticks="red")
	  points(means[,3], means[,2], pch = 16)
	  text(means[,3], means[,2], col = "cornflowerblue", labels = txt, pos = 3, offset = 0.5)
	}


}


