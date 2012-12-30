#' @include PCAGui.R
#' @include clustering.R

# Side effect : write in PCAinitialData
on_dataButton_clicked <- function(widget) {
	columns <- c()
	for(i in 1:nbExperiments) {
		columns <- c(columns, i* (nbReplicats * 2 + 2) , i*(nbReplicats*2+2) + 1 )
	}
	PCAinitialData <<- computePCA(baseData[,-c(1, columns)])	
	widgets$datasetInformationWindow$show()
	handlePCAComboBoxes(ncol(PCAinitialData$rotation), widgets$PC1DataComboBox, widgets$PC2DataComboBox)
	drawPCA(widgets$PC1DataComboBox$getActive()+1, widgets$PC2DataComboBox$getActive()+1, PCAinitialData, names(baseData[,-c(1, columns)]), widgets$PCADataArea)
	drawEigenValues(PCAinitialData, widgets$eigenInitialDrawingArea)
	drawClustering(baseData[,-c(1, columns)], widgets$clusteringInitialDrawingArea)
	
	
}

configureDataInformation <- function() {
	widgets$fileNameLabel$setText(paste("File name:", basename(widgets$dataFileChooserButton$getFilename())))
	widgets$locationLabel$setText(paste("File location:", dirname(widgets$dataFileChooserButton$getFilename())))
	widgets$genesInfoLabel$setText(paste("Genes:", nbGenes))
	widgets$expInfoLabel$setText(paste("Experiments:", nbExperiments))
	widgets$samplesInfoLabel$setText(paste("Samples:", 2*nbReplicats*nbExperiments))
	widgets$repInfoLabel$setText(paste("Replicats:", nbReplicats))
	# Retrieving samples names by creating a vector containing columns numbers to be ignored
	columns <- c()
	for(i in 1:nbExperiments) {
		columns <- c(columns, i* (nbReplicats * 2 + 2) , i*(nbReplicats*2+2) + 1 )
	}
	samplesNames <- colnames(baseData)[-c(1,columns)]
	widgets$samplesNamesLabel$setText(paste(samplesNames, collapse=" "))
	
}

on_continueInfoButton_clicked <- function(widget) {
	widgets$datasetInformationWindow$hide()
}

on_changeDatasetButton_clicked <- function(widget) {
	init()
	widgets$datasetInformationWindow$hide()
	widgets$intersectsWindow$hide()	
	widgets <<- initWidgets()
	gSignalConnect(widgets$PC1ComboBox, "changed", updatePCA)
	gSignalConnect(widgets$PC2ComboBox, "changed", updatePCA)
	gSignalConnect(widgets$PC1DataComboBox, "changed", updateInitialPCA)
	gSignalConnect(widgets$PC2DataComboBox, "changed", updateInitialPCA)
	widgets$loadFileWindow$show()	
	setCurrentStep(0)
}