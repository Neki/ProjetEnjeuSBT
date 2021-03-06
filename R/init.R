
init <- function() {
	
# Initializing global variables
	# TODO : comment them and indicate where there are used
	baseData <<- NULL
	dataFileOK <<- FALSE
	foldUpMin <<- 1.20
	foldUpMax <<- 1.20
	foldDownMin <<- -1.20
	foldDownMax <<- -1.20
	pvalMax <<- 0.06
	nbReplicats  <<- 0
	nbGenes <<- 0
	nbSamples <<- 0
	nbExperiments <<- 0
	genesUp <<- list()
	genesDown <<-list() 
	checkBoxesList <<- NULL
	buttonState <<- c()
	customLists <<- list()
	customListsNames <<- c()
	selectedData <<- data.frame()
	PCAdata <<- NULL
	PCAinitialData <<- NULL
	PCAfinalData <<- NULL
	finalData <<- NULL
	currentStep <<- 0
}

retrieveBuilder <- function() {
	builder <- gtkBuilderNew()
	builder$addFromFile(file.path(path.package("genesSelector"), "extdata/gui.glade") )
	return(builder)
}

initWidgets <- function() {	
	
	# Creating a GtkBuilder from which we will be able to retrieve the necesary GUI elements 
	builder <- retrieveBuilder()
	
	widgets <- new.env()
	widgets$loadFileWindow <- builder$getObject("loadFileWindow")
	widgets$loadFileStatusbar  <- builder$getObject("loadFileStatusbar")
	widgets$nbReplicatsSpinButton <- builder$getObject("nbReplicatsSpinButton")
	widgets$fileInfoLabel <- builder$getObject("fileInfoLabel")
	widgets$dataFileChooserButton <- builder$getObject("dataFileChooserButton")
	widgets$decEntry <- builder$getObject("decEntry")
	widgets$colEntry <- builder$getObject("colEntry")	
	
	widgets$intersectsWindow <- builder$getObject("intersectsWindow")
	widgets$dataButton <- builder$getObject("dataButton")
	widgets$listsToDisplayVBox <- builder$getObject("listsToDisplayVBox")
	widgets$option3RadioButton <- builder$getObject("option3RadioButton")
	widgets$useUpperLimitButton <- builder$getObject("useUpperLimitButton")
	widgets$useLowerLimitButton <- builder$getObject("useLowerLimitButton")
	widgets$foldUpMaxSpinButton <- builder$getObject("foldUpMaxSpinButton")
	widgets$foldDownMinSpinButton <- builder$getObject("foldDownMinSpinButton")
	widgets$foldUpMinSpinButton <- builder$getObject("foldUpMinSpinButton")
	widgets$foldDownMaxSpinButton <- builder$getObject("foldDownMaxSpinButton")
	widgets$linkFoldLimitsCheckButton <- builder$getObject("linkFoldLimitsCheckButton")
	widgets$pvalueSpinButton <- builder$getObject("pvalueSpinButton")
	widgets$intersectsStatusBar <- builder$getObject("intersectsStatusBar")
	widgets$option1RadioButton <- builder$getObject("option1RadioButton")
	widgets$option2RadioButton <- builder$getObject("option2RadioButton")
	widgets$option4RadioButton <- builder$getObject("option4RadioButton")
	widgets$nbGroupsSpinButton <- builder$getObject("nbGroupsSpinButton")
	#widgets$customListsWindow <- builder$getObject("customListsWindow")
	widgets$vennDiagramsFrame <- builder$getObject("vennDiagramsFrame")
	
	widgets$tableAlignment <- builder$getObject("tableAlignment")
	#widgets$optionTable <- builder$getObject("optionTable")
	
	widgets$intersectFrame <- builder$getObject("intersectFrame")
	widgets$PCAFrame <- builder$getObject("PCAFrame")
	widgets$PCChooserHBox <- builder$getObject("PCChooserHBox")
	widgets$PCAArea  <- builder$getObject("PCAArea") # naming convention changed due to a glitch in Glade
#widgets$PCAArea2 <- builder$getObject("PCAArea2")
	widgets$PCChooserHBox <- builder$getObject("PCChooserHBox")
	widgets$eigenDrawingArea <- builder$getObject("eigenDrawingArea")
	widgets$clusteringDrawingArea <- builder$getObject("clusteringDrawingArea")
	widgets$selectedGenesLabel <- builder$getObject("selectedGenesLabel")
	
	
	widgets$dataPCChooserHBox <- builder$getObject("dataPCChooserHBox")
	
	#widgets$datasetInformationWindow <- builder$getObject("datasetInformationWindow")
	
	widgets$upDrawingArea <- builder$getObject("upDrawingArea") 
	widgets$downDrawingArea <- builder$getObject("downDrawingArea")
	widgets$saveFolderChooserButton <- builder$getObject("saveFolderChooserButton")
	widgets$refineButton <- builder$getObject("refineButton")
	
	widgets$refinementWindow <- builder$getObject("refinementWindow")
	widgets$limitersVBox <- builder$getObject("limitersVBox")
	widgets$PCAFinalArea <- builder$getObject("PCAFinalArea")
	widgets$finalEigenArea <- builder$getObject("finalEigenArea")
	widgets$finalUpdateButton <- builder$getObject("finalUpdateButton")
	widgets$finalClusteringArea <- builder$getObject("finalClusteringArea")
	widgets$finalPCAHBox <- builder$getObject("finalPCAHBox")
	widgets$datasetNameLabel2 <- builder$getObject("datasetNameLabel2")
	widgets$finalSelectedGenesLabel <- builder$getObject("finalSelectedGenesLabel")
	widgets$saveFolderStep4ChooserButton <- builder$getObject("saveFolderStep4ChooserButton")
	
# As of 3.8, Glade does not support gtkComboNewText() (and as for now RGtk2 does not support gtkComboBoxText() ) so we are to create text comboboxes manually
	widgets$PC1ComboBox <- gtkComboBoxNewText()
	widgets$PC2ComboBox <- gtkComboBoxNewText()
	widgets$PCChooserHBox$packStart(widgets$PC1ComboBox, FALSE)
	widgets$PCChooserHBox$reorderChild(widgets$PC1ComboBox, 1)
	widgets$PCChooserHBox$packStart(widgets$PC2ComboBox, FALSE)
	widgets$PCChooserHBox$reorderChild(widgets$PC2ComboBox, 3)
	
	
	
	widgets$PC1FinalComboBox <- gtkComboBoxNewText()
	widgets$PC2FinalComboBox <- gtkComboBoxNewText()
	widgets$finalPCAHBox$packStart(widgets$PC1FinalComboBox, FALSE)
	widgets$finalPCAHBox$reorderChild(widgets$PC1FinalComboBox, 1)
	widgets$finalPCAHBox$packStart(widgets$PC2FinalComboBox, FALSE)
	widgets$finalPCAHBox$reorderChild(widgets$PC2FinalComboBox, 3)
	
	csvFilter <- gtkFileFilterNew()
	csvFilter$addPattern("*.csv")
	widgets$dataFileChooserButton$setFilter(csvFilter)
	
	builder$connectSignals()
	
	return(widgets)
}