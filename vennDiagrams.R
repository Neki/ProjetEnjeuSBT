
upDrawingArea <- builder$getObject("upDrawingArea") 
downDrawingArea <- builder$getObject("downDrawingArea")

drawVennDiagrams  <- function(widget = NULL) {
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
	res <- venn.diagram(listUp, filename = NULL, category.names = namesUp,  main = "Venn diagram (up)", col = usedColors, alpha = c(0.1), fill = usedColors)
	changeCairoDevice(upDrawingArea)
	grid.newpage()
	grid.draw(res)

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
	res <- venn.diagram(listDown, filename = NULL, category.names = namesDown,  main = "Venn diagram (down)", col = usedColors, alpha = c(0.1), fill = usedColors)
	changeCairoDevice(downDrawingArea)
	grid.newpage()
	grid.draw(res)

	widgets$intersectsStatusBar$push(widgets$intersectsStatusBar$getContextId("info"), "Venn diagrams drawn.")

	#TODO : handle the case of empty upGenes or downGenes list

}
