#' A GUI to facilitate gene selection based on p-value and folds criteria from DNA chip experiments
#' 
#' Using pre-processed DNA chip data, this package enable the user to
#' use a GUI to select genes. Base criteria include fold & p-value. Consult
#' the pdf help document for details.
#' 
#' The main function of this package is \code{\link{genesSelectorGUI}}
#'  
#' @import grid VennDiagram RGtk2 cairoDevice
#' @exportPattern "_"
#' @title A GUI to facilitate gene selection based on p-value and folds criteria from DNA chip experiments
#' @docType package
#' @name genesSelector
NULL

#' @include init.R
NULL

# Notes on the code
#
# This code does not separate the model (which is mainly represented 
# by global variables) from the controler (callbacks) from the view (RGtk2 GUI)
# If this app was to grow, it would be necessary to refactor the code.
# 
# Global variables are used extensively throughout the code. The environment "widgets" created
# in init.R, function initWidgets(), contain widgets and is accessible at any point in the program.
# Other global variables are created in init.R, function init()
# When reading the code, you can assume that any function can read any global variable and read/write
# any widget state.
# If a function writes in a global variable, it should (but may not) be documented.
#
# Most "include" Roxygen tags are not really necesssary, but they facilitate reading the code 
# by indicating where functions used in a file are defined.
# 
# In order for the function gtkBuilderConnectSignals to work, callbacks must be defined in the global
# environmnment, so the package exports all functions containing "_" in their names.
# Make sure any callback added contain an underscore in his name, otherwise the function will fail.

#' Launch the genes selector GUI
#' 
#' This function start a new Graphical User Interface for the genes selector. Make sure you have not 
#' any instance of the GUI running before calling this function.
#' The full documentation on this tool can be acessed through the help manual, located in the "doc"
#' subfolder of the package installation folder.
#' 
#' @keywords GUI, gene, selection
#' @export
genesSelectorGUI <- function() {
	
	require(VennDiagram, quietly = TRUE) # Workaround to avoid a "error in as.environment(pos)"
	widgets <<- initWidgets()
	init()
	
	gSignalConnect(widgets$PC1ComboBox, "changed", updatePCA)
	gSignalConnect(widgets$PC2ComboBox, "changed", updatePCA)
	gSignalConnect(widgets$PC1DataComboBox, "changed", updateInitialPCA)
	gSignalConnect(widgets$PC2DataComboBox, "changed", updateInitialPCA)
	widgets$loadFileWindow$show()
}


