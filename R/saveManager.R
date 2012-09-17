#' Save current results in a folder
#' 
#' This function uses global variables created by the package (thus need only one parameter)
#' Create the following subfolders :
#'     genesLists
#'     PCA
#'     clustering
#'     vennDiagrams
#' 
#' Create the following files :
#'   in genesLists :
#'     upGenes.txt 				list of up genes in each group (after step 1)
#'     downGenes.txt 			list of down genes in each group (after step 1)
#'     selectionCriteria.txt 	folds and p-values use to create these lists in step 1 + an indication on the criterium used in step 2 
#'     selectedUpGenes.txt 		list of all up selected genes (after step 2)
#'     selectedDownGenes.txt 	list of all down selected genes (after step 2)
#'     selectedGenes.txt 		list of all selected genes after step 2 (union of the 2 previous files)
#'     selectedData.csv			the data frame used for PCA analysis
#'   in PCA
#'     PCA.tiff and PCA.svg 	PCA plots
#'     components.txt 			variances and cumulated variances
#'     correlationMatrix.csv 	computed correlation matrix
#'     projections.csv 			indicate the projection of each gene on eigenvectors
#'   in clustering
#'     clustering.tiff and clustering.svg	clustering plots
#'   in vennDiagrams
#'     upVennDiagram.tiff and downVennDiagram.tiff	Venn diagrams as displayed on the main window
#' 	
#' @param saveFolder the path to the folder which will contain the results.
saveResults <- function(saveFolder) {
	dir.create(file.path(saveFolder, "genesList"))
	dir.create(file.path(saveFolder, "PCA"))
	dir.create(file.path(saveFolder, "clustering"))
	dir.create(file.path(saveFolder, "vennDiagrams"))
}

on_saveButton_clicked <- function(widget) {
	# Retrieve folder to save in
	saveFolder <- widgets$saveFolderChooserButton$getFilename()
	saveResults(saveFolder)
}