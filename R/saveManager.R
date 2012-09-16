#' Save current results in a folder
#' 
#' This function uses global variables created by the package (thus need only one parameter)
#' Create the following subfolders :
#' 
#' Create the following files :
#' 
#' @param saveFolder the path to the folder which will contain the results
saveResults <- function(saveFolder) {
	
}

on_saveButton_clicked <- function(widget) {
	# Retrieve folder to save in
	saveFolder <- widgets$saveFolderChooserButton$getFilename()
	saveResults(saveFolder)
}