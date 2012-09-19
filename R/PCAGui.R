#' @include deviceManager.R
NULL

#' Compute the Principal Compenent Analysis of a data frame
#' 
#' This is merely a wrapper of \code{\link{prcomp}}
#' 
#' @param mydata a data frame which will be transposed and submitted to prcomp
#' @return an object of type \code{prcomp} 
#' @seealso \code{\link{handlePCAComboBoxes}}, \code{\link{drawPCA}}
computePCA <- function(mydata) {
	return(prcomp(t(mydata), center = TRUE, scale = TRUE))
}

#' Helper for updating to set the choices of two comboboxes
#' 
#' This function will set the choices of two comboxes to "PC1 PC2 ... PCn" where n can be choosed
#' Warning : this will trigger the "choice_set" signal of the comboboxes
#' 
#' @param nbPC the number of choices to display
#' @param PC1ComboBox a combobox to be changed
#' @param PC2ComboBox another combobox to be changed
#' @return always \code{TRUE} 
#' @seealso \code{\link{drawPCA}}
handlePCAComboBoxes <- function(nbPC, PC1ComboBox, PC2ComboBox) {	
	# gSignalHandlerDisconnect(widgets$PC1ComboBox, handlerID1)
	# gSignalHandlerDisconnect(widgets$PC2ComboBox, handlerID2)
	
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
	# handlerID1 <<- gSignalConnect(PC1ComboBox, "changed", drawPCA)
	# handlerID2 <<- gSignalConnect(PC2ComboBox, "changed", drawPCA)
}

#' Draw a PCA correlation plot to a GtkDrawable (usually a GtkDrawingArea) or a file
#' 
#' This function supports .tiff and .svg extensions when plotting to a file. If another extension is given
#' this function will raise an error.
#' 
#' @param widget used for callbacks (now ignored)
#' @param PCx the number of the principal component (as an integer) which will be the x coordinate of points
#' @param PCy the number of the principal component (as an integer) which will be the y coordinate of points
#' @param PCdata an object of type \code{prcomp}
#' @param Veclabels a vector of strings containing the points labels
#' @param drawingArea a GtkDrawable (usually a GtkDrawingArea) if printToFile is set to \code{FALSE}, a string indicating the path of the file to create otherwise.
#' @param printToFile a boolean. If set to \code{FALSE}, this function will plot to the GtkDrawable \code{drawingArea}. If set to \code{TRUE}
#' this function will plot to the file whose path is \code{drawingArea}
#' @return \code{FALSE} if the function failed at plotting, \code{TRUE} otherwise
#' @seealso \code{\link{computePCA}}, \code{\link{handlePCAComboBoxes}}
drawPCA <- function(widget = NULL, PCx, PCy, PCdata, Veclabels, drawingArea, printToFile = FALSE) { 
	if(PCx < 1 | PCy < 1) return(FALSE)
	cols = c("cornflowerblue", "darkblue", "red", "deeppink3", "orange", "chocolate4", "darkorchid1", "darkmagenta", "darkseagreen", "darkslategrey")
	colVec <- rep(cols, rep(nbReplicats, length(cols)))	
	if(printToFile) {
		ext <- getExtension(drawingArea)
		if(ext == "tiff") {
			tiff(filename=drawingArea)
			par(mar=c(5,5,0.2,0.2))
			plot(PCdata$x[,PCx], PCdata$x[,PCy], 
					xlab=paste("PC",PCx, sep=""),
					ylab=paste("PC",PCy, sep=""),
					pch =16,
					cex = 0.9,
					cex.axes = 0.8,
					col = colVec
			)
			text(PCdata$x[,PCx], PCdata$x[,PCy], labels=Veclabels, cex=0.9, pos=4, col="black")
			return(TRUE)
			dev.off()
		} else if(ext== "svg") {
			svg(filename=drawingArea)
			par(mar=c(5,5,0.2,0.2))
			plot(PCdata$x[,PCx], PCdata$x[,PCy], 
					xlab=paste("PC",PCx, sep=""),
					ylab=paste("PC",PCy, sep=""),
					pch =16,
					cex = 0.9,
					cex.axes = 0.8,
					col = colVec
			)
			text(PCdata$x[,PCx], PCdata$x[,PCy], labels=Veclabels, cex=0.9, pos=4, col="black")
			return(TRUE)
			dev.off()
		} else {stop("Unrecognized file extension (only tiff and svg are supported)")}
	} else {
		changeCairoDevice(drawingArea)
		par(mar=c(5,5,0.2,0.2))
		plot(PCdata$x[,PCx], PCdata$x[,PCy], 
		     xlab=paste("PC",PCx, sep=""),
		     ylab=paste("PC",PCy, sep=""),
		     pch =16,
		     cex = 0.9,
		     cex.axes = 0.8,
		     col = colVec
		     )
		text(PCdata$x[,PCx], PCdata$x[,PCy], labels=Veclabels, cex=0.9, pos=4, col="black")
		return(TRUE)
	}
}

#' Draw a graph representing the importance of PCA components to a GtkDrawable (usually a GtkDrawingArea) or a file
#' 
#' This function supports .tiff and .svg extensions when plotting to a file. If another extension is given
#' this function will raise an error.
#' 
#' @param widget used for callbacks, ignored
#' @param PCdata an object of type \code{prcomp}
#' @param drawingArea a GtkDrawable (usually a GtkDrawingArea) if \code{printTofile} is set to \code{FALSE},
#' otherwise the path to a file
#' @param printToFile a boolean. If set to \code{TRUE}, will plot to a file.
#' @return always \code{TRUE}
#' @seealso \code{\link{drawPCA}}
drawEigenValues <- function(widget = NULL, PCdata, drawingArea, printToFile = FALSE) {
	if(printToFile) {
		ext <- getExtension(drawingArea)
		if(ext == "tiff") {
			tiff(filename=drawingArea)
			par(mar=c(0.2,2,1,1))
			plot(PCdata, main = NULL, xlab="")
			dev.off()
		}
		if(ext == "svg") {
			svg(filename=drawingArea)
			par(mar=c(0.2,2,1,1))
			plot(PCdata, main = NULL, xlab="")
			dev.off()
		}
	} else {
		changeCairoDevice(drawingArea)
		par(mar=c(0.2,2,1,1))
		plot(PCdata, main = NULL, xlab="")
		return(TRUE)
	}
}

# Below are functions which are only wrappers for drawPCA
# They are used as callbacks
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



