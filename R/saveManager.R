#' Save current results in a folder
#' 
#' This function uses global variables created by the package (thus need only one parameter)
#' Create the following files :
#'   gene lists:
#'     step1/genesUp.csv			list of up genes in each group (after step 1)
#'     step1/genesDown.csv 			list of down genes in each group (after step 1)
#'     step1/selectionCriteria.txt 	folds and p-values use to create these lists in step 1
#'     step2/selectedGenes.txt 		list of all selected genes (after step 2)
#'     step3/selectedData.csv		the data frame used for PCA analysis in step 3
#'   PCA
#'     PCA.tiff and PCA.svg 	PCA plots
#'     components.txt 			variances and cumulated variances
#'     correlationMatrix.csv 	computed correlation matrix
#'     projections.csv 			indicate the projection of each gene on eigenvectors
#'   clustering
#'     clustering.tiff and clustering.svg	clustering plots
#'   vennDiagrams
#'     upVennDiagram.tiff and downVennDiagram.tiff	Venn diagrams as displayed on the main window
#' 	
#' @param saveFolder the path to the folder which will contain the results.
saveResults <- function(saveFolder) {
	sepCharacter <- widgets$colEntry$getText()
	
	unlink(file.path(saveFolder, "step1"), recursive = TRUE)	
	dir.create(file.path(saveFolder, "step1"))	
	# Saving genesUp.txt
	file = file.path(saveFolder, "step1", "genesUp.csv")
	for(i in 1:length(genesUp)) {
		y <- genesUp[[i]]
		write(paste(i,":", names(baseData)[(i-1)*(nbReplicats*2+2) + 2], names(baseData)[(i-1)*(nbReplicats*2+2)+nbReplicats+2], "etc."),
				file = file, append = TRUE)
		write(paste(y, baseData[y,1], sep = sepCharacter), file = file, append = TRUE)		
	}
	
	# Saving genesDown.txt
	file = file.path(saveFolder, "step1", "genesDown.csv")
	for(i in 1:length(genesDown)) {
		y <- genesDown[[i]]
		write(paste(i,":", names(baseData)[(i-1)*(nbReplicats*2+2) + 2], names(baseData)[(i-1)*(nbReplicats*2+2)+nbReplicats+2], "etc."),
				file = file, append = TRUE)
		write(paste(y, baseData[y,1], sep = sepCharacter), file = file, append = TRUE)		
	}
	
	# ...
	file = file.path(saveFolder, "step1", "selectionCriteria.txt")
	write(paste("Up : fold between",foldUpMin,"and", foldUpMax,"\n Down : fold between",
					foldDownMin,"and",foldDownMax))

	file = file.path(saveFolder, "step2", "selectedGenes.csv")
	y <- row.names(selectedData)
	# TODO : finish
}
	
on_saveButton_clicked <- function(widget) {	
	saveFolder <- widgets$saveFolderChooserButton$getFilename() # Retrieve folder to save in
	saveResults(saveFolder)
}