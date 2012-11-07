#' @include deviceManager.R
NULL

#' Compute and draw clustering results
#'
#' This function is able to compute clustering and plot the results on a widget or in a file. It uses
#' euclidean distances and the Ward method. Supported file formats are .svg and .tiff. If another extension is detected, then the
#' function will raise an error.
#' 
#' @param mydata the data frame (or matrix) containing the data to work on. Columns are variables and rows are individuals.
#' @param drawingArea a GtkDrawable (usually a GtkDrawingArea) if \code{printToFile} is \code{FALSE} or a path to a file on which the results are to be plotted if \code{printToFile} is \code{TRUE}.
#' @param printToFile a boolean. If set to \code{TRUE}, the results will be printed in the file whose location is \code{drawingArea}. The type of the output (.svg or .tiff) depend on the filename provided in \code{drawingArea}.
drawClustering <- function(mydata, drawingArea, printToFile = FALSE) {
	data <- t(mydata)
	d <- dist(data, method = "euclidean")
	fit <- hclust(d, method="ward")
	if(!printToFile) {
		changeCairoDevice(drawingArea)
		par(mar=c(0,0,0.8,0))
		plot(fit, ylab = "", xlab ="",axes = FALSE, cex = 0.8, main  = NULL)
	} else {
		ext <- getExtension(drawingArea)
		if(ext == "tiff") {
			tiff(filename=drawingArea)
			par(mar=c(0,0,0.8,0))
			plot(fit, ylab = "", xlab ="",axes = FALSE, cex = 0.8, main  = NULL)
			dev.off()
		} else if(ext== "svg") {
			svg(filename=drawingArea)
			par(mar=c(0,0,0.8,0))
			plot(fit, ylab = "", xlab ="",axes = FALSE, cex = 0.8, main  = NULL)
			dev.off()
		} else {stop("Unrecognized file extension (only tiff and svg are supported)")}
	} 
}