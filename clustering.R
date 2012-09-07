drawClustering <- function(widget = NULL) {
	data <- t(selectedData[,-1])
	d <- dist(data, method = "euclidean")
	fit <- hclust(d, method="ward")
	changeCairoDevice(widgets$clusteringDrawingArea)
	par(mar=c(0,0,0.8,0))
	plot(fit, ylab = "", xlab ="",axes = FALSE, cex = 0.8, main  = NULL )
}
