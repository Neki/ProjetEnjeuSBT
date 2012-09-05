PCChooserHBox <- builder$getObject("PCChooserHBox")
PCAArea  <- builder$getObject("PCAArea") # naming convention changed due to a glitch in Glade
PCAArea2 <- builder$getObject("PCAArea2")

PC1ComboBox <- gtkComboBoxNewText()
PC2ComboBox <- gtkComboBoxNewText()
PCChooserHBox$packStart(PC1ComboBox, FALSE)
PCChooserHBox$reorderChild(PC1ComboBox, 1)
PCChooserHBox$packStart(PC2ComboBox, FALSE)
PCChooserHBox$reorderChild(PC2ComboBox, 3)

computePCA <- function() {
	PCAdata <<- prcomp(t(selectedData[,-1]), center = TRUE, scale = TRUE)
	nbPC <- ncol(PCAdata$rotation)
	# Emptying comboxes
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
}

drawPCA <- function(widget = NULL) {
	changeCairoDevice(PCAArea)
	par(mar=c(0,0,0,0))
	plot(PCAdata$x[,PC1ComboBox$getActive()+1], PCAdata$x[,PC2ComboBox$getActive()+1], 
	     xlab=PC1ComboBox$getActiveText(),
	     ylab=PC2ComboBox$getActiveText(),
	     mar=c(1,1,1,1),
	     text(PCAdata$x[,PC1ComboBox$getActive()+1], PCAdata$x[,PC2ComboBox$getActive()+1], labels=names(selectedData[,-1]), cex=0.9, pos=4, col="black")
	     )
	changeCairoDevice(PCAArea2)
	biplot(PCAdata, pc.biplot=TRUE)
}

gSignalConnect(PC1ComboBox, "changed", drawPCA)
gSignalConnect(PC2ComboBox, "changed", drawPCA)


