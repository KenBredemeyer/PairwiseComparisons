#' Plot Person Characteristic Curves
#'
#' @param data_matrix Square data frame containing pairwise comparison data
#'   converted to proportions.  Null comparisons must be represented as NA.
#' @param betas Named vector of locations of performance ability, in logits.
#' @param class_intervals The number of class intervals.
#' @param performances Which performances to plot.  Character "all" for all performances, or a
#'   numeric (integer) vector of performance numbers to plot.
#' @examples
#' plot_PCCs(gtpa2018_proportions_dm, gtpa2018_locations,
#'           class_intervals = 3,
#'           performances = 21:22)
#'
#' @export
plot_PCCs <- function(data_matrix, betas, class_intervals = 4, performances = "all") {
  stopifnot(class(data_matrix) == "data.frame" && class(betas) == "numeric")

	comparison <- class_intervals(data_matrix, betas, class_intervals)

	xx <- seq(-13, 13, 0.01)

	y <- vector("numeric", length(xx))
	# store mean proportion and ability for each CI for all scripts
	means_all <- vector("list", nrow(data_matrix))

	#par(ask = TRUE)
	if (is.character(performances)) performances = 1:nrow(data_matrix)
	for (n in performances) {
		# class interval means
    means <- aggregate(comparison[[n]], list(comparison[[n]][,"class_interval"]), mean)
	  means_all[[n]] <- means

	  #points for plotting
	  for (i in 1:length(xx)) {
	    y[i] <-  exp(xx[i] - betas[n])/(1 + exp(xx[i] - betas[n]))
	  }

	  txt <- rle(comparison[[n]][ , "class_interval"])$lengths
	  # plot window range = range of ability estimates (for the group of persons)
	  plot(xx, y, xlim = c(min(betas)-2, max(betas)+1.2), ylim = c(0, 1.1), type = "l", xlab = "PCC for script ID",
	    ylab = "Expected Value", col="red", main = "Performance Characteristic Curve", sub = rownames(data_matrix)[n])
	  axis(side=1, tick=TRUE, at= round(betas[n], 3), padj=1, lwd.ticks = 2, col.ticks="red")
	  points(means[,3], means[,2], pch = 16)
	  text(means[,3], means[,2], col = "cornflowerblue", labels = txt, pos = 3, offset = 0.5)
	}
}
