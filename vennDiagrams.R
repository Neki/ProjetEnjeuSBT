library(grid)
library(VennDiagram)
library(cairoDevice)
library(gridBase)

upDrawingArea <- builder$getObject("upDrawingArea") 
downDrawingArea <- builder$getObject("downDrawingArea")

drawVennDiagrams  <- function(widget = NULL) {
	checkBoxesList <- listsToDisplayVBox$getChildren()
	checkBoxesList[[1]] <- NULL
	# Up
	listUp <- list()
	namesUp <- NULL 
	colorsVec <-c("red", "green", "blue", "brown")
	for (i in 1:length(checkBoxesList)) {
		if(gtkToggleButtonGetActive(checkBoxesList[[i]])){
		   listUp <- c(listUp, genesUp[i])
		   if(length(genesUp[[i]] != 0 )) namesUp <- c(namesUp, i)
		}
	}
	colorsVec <- colorsVec[1:length(checkBoxesList)] 
	res <- venn.diagram(listUp, filename = NULL, category.names = namesUp,  main = "Venn diagram (up)", col = colorsVec)
	asCairoDevice(upDrawingArea)
	grid.draw(res)

	# Down
	listDown <- list()
	namesDown <- NULL 
	for (i in 1:length(checkBoxesList)) {
		if(gtkToggleButtonGetActive(checkBoxesList[[i]])){
		   listDown <- c(listDown, genesDown[i])
		   if(length(genesDown[[i]] != 0 )) namesDown <- c(namesDown, i)
		}
	}
	res <- venn.diagram(listDown, filename = NULL, category.names = namesDown,  main = "Venn diagram (down)", col = colorsVec)
	asCairoDevice(downDrawingArea)
	grid.draw(res)


	#TODO : handle the case of empty upGenes or downGenes list

}
