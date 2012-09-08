drawClustering <- function(widget = NULL, mydata, drawingArea) {
	data <- t(mydata)
	d <- dist(data, method = "euclidean")
	fit <- hclust(d, method="ward")
	changeCairoDevice(drawingArea)
	par(mar=c(0,0,0.8,0))
	plot(fit, ylab = "", xlab ="",axes = FALSE, cex = 0.8, main  = NULL )
}
