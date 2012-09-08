
computePCA <- function(mydata) {
	return(prcomp(t(mydata), center = TRUE, scale = TRUE))
}

handlePCAComboBoxes <- function(nbPC, PC1ComboBox, PC2ComboBox) {
	# Emptying comboxes
	# gSignalHandlerDisconnect(widgets$PC1ComboBox, handlerID1)
	# gSignalHandlerDisconnect(widgets$PC2ComboBox, handlerID2)
	treeModel <- PC1ComboBox$getModel()
	treeModel$clear()
	treeModel <- PC2ComboBox$getModel()
	treeModel$clear()

	for(i in 1:nbPC) {
		PC1ComboBox$appendText(paste("PC", i, sep=""))
		PC2ComboBox$appendText(paste("PC", i, sep=""))
	}
	PC1ComboBox$setActive(0)
	PC2ComboBox$setActive(1)
	# handlerID1 <<- gSignalConnect(PC1ComboBox, "changed", drawPCA)
	# handlerID2 <<- gSignalConnect(PC2ComboBox, "changed", drawPCA)
}
drawPCA <- function(widget = NULL, PC1, PC2, PCdata, Veclabels, drawingArea) { 
	if(PC1 < 1 | PC2 < 1) return(FALSE)
	changeCairoDevice(drawingArea)
	par(mar=c(5,5,0.2,0.2))
	cols = c("cornflowerblue", "darkblue", "red", "deeppink3", "orange", "chocolate4", "darkorchid1", "darkmagenta", "darkseagreen", "darkslategrey")
	colVec <- rep(cols, rep(nbReplicats, length(cols)))	
	plot(PCdata$x[,PC1], PCdata$x[,PC2], 
	     xlab=paste("PC",PC1, sep=""),
	     ylab=paste("PC",PC2, sep=""),
	     pch =16,
	     cex = 0.9,
	     cex.axes = 0.8,
	     col = colVec
	     )
	text(PCdata$x[,PC1], PCdata$x[,PC2], labels=Veclabels, cex=0.9, pos=4, col="black")
	return(TRUE)
}

drawEigenValues <- function(widget, PCdata, drawingArea) {
	changeCairoDevice(drawingArea)
	par(mar=c(0.2,2,1,1))
	plot(PCdata, main = NULL, xlab="")
	return(TRUE)
}

updatePCA <- function(widget) {
	 drawPCA(widget, widgets$PC1ComboBox$getActive()+1, widgets$PC2ComboBox$getActive()+1, PCAdata, names(selectedData[,-1]), widgets$PCAArea)
}

updateInitialPCA <- function(widget) {
	columns <- c()
	        for(i in 1:nbExperiments) {
			                        columns <- c(columns, i* (nbReplicats * 2 + 2) , i*(nbReplicats*2+2) + 1 )
	        }

	 drawPCA(widget, widgets$PC1DataComboBox$getActive()+1, widgets$PC2DataComboBox$getActive()+1, PCAinitialData, names(baseData[,-c(1, columns)]), widgets$PCADataArea)
}



