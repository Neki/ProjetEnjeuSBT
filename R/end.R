#' @include init.R
NULL

#' Launch the genes selector GUI
#' 
#' This function start a new Graphical User Interface for the genes selector. Make sure you have not 
#' any instance of the GUI running before calling this function.
#' The full documentation on this tool can be acessed through the help manual, located in the "inst/doc"
#' subfolder of the package.
#' 
#' @keywords GUI, gene, selection
#' @export
genesSelectorGUI <- function() {
	widgets <<- initWidgets()
	init()
	gSignalConnect(widgets$PC1ComboBox, "changed", updatePCA)
	gSignalConnect(widgets$PC2ComboBox, "changed", updatePCA)
	gSignalConnect(widgets$PC1DataComboBox, "changed", updateInitialPCA)
	gSignalConnect(widgets$PC2DataComboBox, "changed", updateInitialPCA)
	builder$connectSignals()
	widgets$loadFileWindow$show()
}


