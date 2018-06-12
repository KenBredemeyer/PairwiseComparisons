#' Form class intervals 
#' 
#' @param x data matrix, ordered on location
#' @param b Numeric vector. Locations returned from \code{estimate}
#' @param nci Integer.  Number of class intervals
#' @export
class_intervals <- function(x, b, nci) {
#numbers of comparisons per script
	nc <- apply(x, 2, function(x) length(na.omit(x)))
	message(paste0(" Min comparisons per script is ", min(nc)))
	
	pw <- vector("list", ncol(x)-1)    
	index <- vector("list", ncol(x)-1)                    # index is a list of which ordered scripts are involved in the comparison
	index <- apply(x[,-(dim(x)[2])], 2, function(x) which(!is.na(x)))
	index <- lapply(index, function(x) unname(x, force = TRUE))
	
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
	  pw.r <- na.omit(unlist(plyr::rbind.fill(a, bb)))   # na.omit removes NA formed by rbind.fill  (not NAs in data)

	  ci_number <- 1:nci        # class interval number
	  pw[[i]] <- rep(ci_number, round(pw.r, 0))
	}
	
	comparisons <- cbind(unname(x[index[[2]], 2]), b[(index[[2]])], pw[[2]])
	
	# Create a matrix of comparisons for each script (a list of matrices)
	comparison <- vector("list", nrow(x))
	for (i in 1:nrow(x)) {
	  comparison[[i]] <- cbind(unname(x[index[[i]], i]), b[(index[[i]])], pw[[i]])
	  colnames(comparison[[i]]) <- c("Proportions", "Locations", "class interval")
	}
	comparison
}
