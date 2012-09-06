
computePCA <- function() {
	PCAdata <<- prcomp(t(selectedData[,-1]), center = TRUE, scale = TRUE)
	nbPC <- ncol(PCAdata$rotation)
	# Emptying comboxes
	gSignalHandlerDisconnect(widgets$PC1ComboBox, handlerID1)
	gSignalHandlerDisconnect(widgets$PC2ComboBox, handlerID2)
	treeModel <- widgets$PC1ComboBox$getModel()
	treeModel$clear()
	treeModel <- widgets$PC2ComboBox$getModel()
	treeModel$clear()

	for(i in 1:nbPC) {
		widgets$PC1ComboBox$appendText(paste("PC", i, sep=""))
		widgets$PC2ComboBox$appendText(paste("PC", i, sep=""))
	}
	widgets$PC1ComboBox$setActive(0)
	widgets$PC2ComboBox$setActive(1)
	handlerID1 <<- gSignalConnect(widgets$PC1ComboBox, "changed", drawPCA)
	handlerID2 <<- gSignalConnect(widgets$PC2ComboBox, "changed", drawPCA)
}

drawPCA <- function(widget = NULL) {
	changeCairoDevice(widgets$PCAArea)
	par(mar=c(2,2,0.2,0.2))
	cols = c("cornflowerblue", "darkblue", "red", "deeppink3", "orange", "chocolate4", "darkorchid1", "darkmagenta", "darkseagreen", "darkslategrey")
	colVec <- rep(cols, rep(nbReplicats, length(cols)))	
	plot(PCAdata$x[,widgets$PC1ComboBox$getActive()+1], PCAdata$x[,widgets$PC2ComboBox$getActive()+1], 
	     xlab=widgets$PC1ComboBox$getActiveText(),
	     ylab=widgets$PC2ComboBox$getActiveText(),
	     pch =16,
	     cex = 0.9,
	     cex.axes = 0.8,
	     col = colVec
	     )
	text(PCAdata$x[,widgets$PC1ComboBox$getActive()+1], PCAdata$x[,widgets$PC2ComboBox$getActive()+1], labels=names(selectedData[,-1]), cex=0.9, pos=4, col="black")
	changeCairoDevice(widgets$PCAArea2)
	biplot(PCAdata, pc.biplot=TRUE)
}


handlerID1 = gSignalConnect(widgets$PC1ComboBox, "changed", drawPCA)
handlerID2 = gSignalConnect(widgets$PC2ComboBox, "changed", drawPCA)


