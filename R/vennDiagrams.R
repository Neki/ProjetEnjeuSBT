#' Compute and draw Venn diagrams to a GtkDrawable or a file
#' 
#' This function uses global variables (#TODO : fix that) but does not write in global variables
#' 
#' @param widget used for callbacks, ignored
#' @param fileUp if set to NULL, will draw the "up diagram" to widgets$upDrawingArea.
#' Otherwise, fileUp must be a string indictaing the path where the diagram will be plotted 
#' @param fileDown same principle for the "down diagram" 
drawVennDiagrams  <- function(widget = NULL, fileUp = NULL, fileDown = NULL) {
	widgets$intersectsStatusBar$push(widgets$intersectsStatusBar$getContextId("info"), "Drawing Venn diagrams...")
	checkBoxesList <- widgets$listsToDisplayVBox$getChildren()
	checkBoxesList[[1]] <- NULL
	# Up
	listUp <- list()
	namesUp <- NULL 
	colorsVec <-c("red", "green", "blue", "brown")
	usedColors <- c()
	for (i in 1:length(checkBoxesList)) {
		if(gtkToggleButtonGetActive(checkBoxesList[[i]])){
		   listUp <- c(listUp, genesUp[i])
		   if(length(genesUp[[i]] != 0 )) namesUp <- c(namesUp, i)
		   usedColors <- c(usedColors,colorsVec[i])
		}
	}
	res <- venn.diagram(listUp, filename = fileUp, category.names = namesUp,  main = "Venn diagram (up)", col = usedColors, alpha = c(0.1), fill = usedColors)
	if(is.null(fileUp)) {
		changeCairoDevice(widgets$upDrawingArea)
		grid.newpage()
		grid.draw(res)
	}

	# Down
	listDown <- list()
	namesDown <- NULL 
	usedColors <- c()
	for (i in 1:length(checkBoxesList)) {
		if(gtkToggleButtonGetActive(checkBoxesList[[i]])){
		   listDown <- c(listDown, genesDown[i])
		   if(length(genesDown[[i]] != 0 )) namesDown <- c(namesDown, i)
		   usedColors <- c(usedColors,colorsVec[i])
		}
	}
	res <- venn.diagram(listDown, filename = fileDown, category.names = namesDown,  main = "Venn diagram (down)", col = usedColors, alpha = c(0.1), fill = usedColors)
	if(is.null(fileDown)) {
		changeCairoDevice(widgets$downDrawingArea)
		grid.newpage()
		grid.draw(res)
	}

	widgets$intersectsStatusBar$push(widgets$intersectsStatusBar$getContextId("info"), "Venn diagrams drawn.")

	#TODO : handle the case of empty upGenes or downGenes list

}
