#' @include vennDiagrams.R
#' @include PCAGui.R
#' @include clustering.R
#' @include init.R

on_useUpperLimitButton_toggled <- function(widget) {
	if(!widgets$useUpperLimitButton$getActive()) widgets$foldUpMaxSpinButton$setSensitive(FALSE)
	if(widgets$useUpperLimitButton$getActive()) widgets$foldUpMaxSpinButton$setSensitive(TRUE)	
	setCurrentStep(1)
}

on_useLowerLimitButton_toggled <- function(widget) {
	if(!widgets$useLowerLimitButton$getActive()) widgets$foldDownMinSpinButton$setSensitive(FALSE)
	if(widgets$useLowerLimitButton$getActive()) widgets$foldDownMinSpinButton$setSensitive(TRUE)	
	setCurrentStep(1)
	
}

# Side effect : write in foldUpMin
on_foldUpMinSpinButton_value_changed <- function(widget) {
	foldUpMin <<- widgets$foldUpMinSpinButton$getValue()
	if(widgets$linkFoldLimitsCheckButton$getActive()) widgets$foldDownMaxSpinButton$setValue(-foldUpMin)
	if(widgets$foldUpMaxSpinButton$getValue() < foldUpMin) widgets$foldUpMaxSpinButton$setValue(foldUpMin)
	setCurrentStep(1)
}

# Side effect : write in foldUpMax
on_foldUpMaxSpinButton_value_changed <- function(widget) {
	foldUpMax <<- widgets$foldUpMaxSpinButton$getValue()
	if(widgets$linkFoldLimitsCheckButton$getActive()) widgets$foldDownMinSpinButton$setValue(-foldUpMax)
	if(foldUpMax < widgets$foldUpMinSpinButton$getValue()) widgets$foldUpMinSpinButton$setValue(foldUpMax)
	setCurrentStep(1)
}

# Side effect : write in foldDownMin
on_foldDownMinSpinButton_value_changed <- function(widget) {
	foldDownMin <<- widgets$foldDownMinSpinButton$getValue()
	if(widgets$linkFoldLimitsCheckButton$getActive()) widgets$foldUpMaxSpinButton$setValue(-foldDownMin)
	if(widgets$foldDownMaxSpinButton$getValue() < foldDownMin) widgets$foldDownMaxSpinButton$setValue(foldDownMin)
	setCurrentStep(1)
}

# Side effect : write in foldDownMax
on_foldDownMaxSpinButton_value_changed <- function(widget) {
	foldDownMax <<- widgets$foldDownMaxSpinButton$getValue()
	if(widgets$linkFoldLimitsCheckButton$getActive()) widgets$foldUpMinSpinButton$setValue(-foldDownMax)
	if(foldDownMax < widgets$foldDownMinSpinButton$getValue()) widgets$foldDownMinSpinButton$setValue(foldDownMax)
	setCurrentStep(1)
}

# Side effect : write in pvalMax
on_pvalueSpinButton_value_changed <- function(widget) {
	setCurrentStep(1)
	pvalMax  <<- widgets$pvalueSpinButton$getValue()
}

# Side effect : write in genesUp and genesDown
on_createListsButton_clicked <- function(widget) {
	widgets$intersectsStatusBar$push(widgets$intersectsStatusBar$getContextId("info"), "Creating lists...")
	# Creating lists
	for(i in 1:nbExperiments) {
		folds <- baseData[i*(nbReplicats*2+2)]
		pvalues <- baseData[i*(nbReplicats*2+2) + 1]
		if (widgets$useUpperLimitButton$getActive()) {genesUp[[i]]  <<- row.names(baseData)[pvalues < pvalMax & folds > foldUpMin & folds < foldUpMax]} else {genesUp[[i]]  <<- row.names(baseData)[pvalues < pvalMax & folds > foldUpMin]} 
		if (widgets$useLowerLimitButton$getActive()) {genesDown[[i]] <<- row.names(baseData)[pvalues < pvalMax & folds > foldDownMin & folds < foldDownMax]} else {genesDown[[i]]  <<- row.names(baseData)[pvalues < pvalMax & folds < foldDownMax]}
	}
	
	widgets$intersectsStatusBar$push(widgets$intersectsStatusBar$getContextId("info"), "Lists created.")
	
	# Drawing Venn Diagrams using the function in vennDiagramms.R
	widgets$intersectsStatusBar$push(widgets$intersectsStatusBar$getContextId("info"), "Lists created, drawing Venn Diagrams...")
	drawVennDiagrams()
	setCurrentStep(2)
}

# Side effect : write in selectedData and PCAdata
on_intersectsButton_clicked <- function(widget) {
	isUpList <- NULL
	isDownList  <- NULL
	selectedGenes <- c()
	for (i in 1:nbExperiments) {
		vecUp <- row.names(baseData) %in% genesUp[[i]]
		vecDown <- row.names(baseData) %in% genesDown[[i]]
		isUpList <- cbind(isUpList, vecUp)
		isDownList <- cbind(isDownList, vecDown)
	}
	if(widgets$option1RadioButton$getActive()) {
		upList <- row.names(baseData)[rowSums(isUpList) >= 2]
		downList <- row.names(baseData)[rowSums(isDownList) >=2]
		selectedGenes <- union(upList, downList)
	}
	if(widgets$option2RadioButton$getActive()) {
		x <- widgets$nbGroupsSpinButton$getValueAsInt()
		upList <- row.names(baseData)[rowSums(isUpList) >= x]
		downList <- row.names(baseData)[rowSums(isDownList) >=x]
		selectedGenes <- union(upList, downList)
	}
	if(widgets$option3RadioButton$getActive()) {
		upList <- row.names(baseData)[rowSums(isUpList) >= 2 & rowSums(isDownList) == 0]
		downList <- row.names(baseData)[rowSums(isDownList) >=2 & rowSums(isDownList) == 0]
		selectedGenes <- union(upList, downList)
	}
	if(widgets$option4RadioButton$getActive()) {
		selectedGenes <- unique(unlist(customLists))
	}
	selectedData <<- baseData[row.names(baseData) %in% selectedGenes,] 
	# We will no longer need the folds and p-values
	columns <- c()
	for(i in 1:nbExperiments) {
		columns <- c(columns, i* (nbReplicats * 2 + 2) , i*(nbReplicats*2+2) + 1 )
	}  
	selectedData <<- selectedData[, - columns]
	
	widgets$selectedGenesLabel$setText(paste(nrow(selectedData), "genes selected"))
	PCAdata <<- computePCA(selectedData[,-1])
	nbPC <- ncol(PCAdata$rotation)
	handlePCAComboBoxes(nbPC, widgets$PC1ComboBox, widgets$PC2ComboBox)
	drawPCA(widgets$PC1ComboBox$getActive()+1, widgets$PC2ComboBox$getActive()+1, PCAdata, names(selectedData[,-1]), widgets$PCAArea)
	drawEigenValues(PCAdata, widgets$eigenDrawingArea)
	drawClustering(selectedData[,-1], widgets$clusteringDrawingArea)
	setCurrentStep(3)
}

# This function need widgets$customListsWindow AND widgets$optionTable to be correctly defined
# see how it is used in on_customListsButton_clicked
configureCustomListsWindow <- function() {
	# Configuring custom lists window according to dataset
	# gtkTableGetChildren() returns a list of objects which have for only type "GtkTableChild" so it is not possible to retrieve the state of the GtkCheckButtons
	# Workaround : using a global dataframe which update each time a check button is toggled, and connecting appropriate signals  
	
	widgets$optionTable$resize(rows = nbExperiments + 2, columns = 5)
	for (i in 1:nbExperiments) {
		widgets$optionTable$attachDefaults(gtkLabelNew(paste(i,":", names(baseData)[(i-1)*(nbReplicats*2+2) + 2], names(baseData)[(i-1)*(nbReplicats*2+2)+nbReplicats+2], "etc.")), 0, 1, i+1, i+2)
		button <- gtkCheckButtonNew()
		gSignalConnect(button, 'toggled', updateButtonsState,c(i, 1))
		widgets$optionTable$attachDefaults(button, 1, 2, i+1, i+2)
		button <- gtkCheckButtonNew()
		gSignalConnect(button, 'toggled', updateButtonsState,c(i, 2))
		widgets$optionTable$attachDefaults(button, 2, 3, i+1, i+2)
		button <- gtkCheckButtonNew()
		gSignalConnect(button, 'toggled', updateButtonsState,c(i, 3))
		widgets$optionTable$attachDefaults(button, 3, 4, i+1, i+2)
		button <- gtkCheckButtonNew()
		gSignalConnect(button, 'toggled', updateButtonsState, c(i,4))
		widgets$optionTable$attachDefaults(button, 4, 5, i+1, i+2)
	}
	buttonState <<- array(data = FALSE, dim = c(nbExperiments, 4))
}

on_customListsButton_clicked <- function(widget) {
	# beware of issue #1 on github : hiding/showing windows is not a good way in RGtk2 to manage
	# multiple windows ; because if the usre click on the rd X it will destroy the window
	# and it wil be impossible to show it again after that
	builder <- retrieveBuilder()
	widgets$customListsWindow <- builder$getObject("customListsWindow")	
	widgets$optionTable <- builder$getObject("optionTable")
	widgets$listNameEntry  <- builder$getObject("listNameEntry")
	widgets$listsScrolledWindow <- builder$getObject("listsScrolledWindow")
	
	configureCustomListsWindow()
	widgets$customListsWindow$show()
	
	builder$connectSignals()
}



on_confirmListsButton_clicked <- function(widget) {
	widgets$customListsWindow$destroy()
}

updateButtonsState <- function(widget, a) {
	if(buttonState[a[1], a[2]]) {buttonState[a[1], a[2]] <<- FALSE} else {buttonState[a[1], a[2]]  <<- TRUE}
}

# Side effect : write in customLists and customListsNames
on_addListButton_clicked <- function(widget) {
	clist <- row.names(baseData) 
	for (i in 1:nbExperiments) {
		if(buttonState[i, 1]) {clist = intersect(clist, genesUp[[i]])}
		if(buttonState[i, 2]) {clist = intersect(clist, genesDown[[i]])}
	}	
	for (i in 1:nbExperiments) {
		if(buttonState[i, 3]) {clist = setdiff(clist, genesUp[[i]])}
		if(buttonState[i, 4]) {clist = setdiff(clist, genesDown[[i]])}
	}
	customLists[[length(customLists)+1]] <<- clist
	if(widgets$listNameEntry$getText() != "") {customListsNames <<- c(customListsNames, widgets$listNameEntry$getText())} else {customListsNames <<-c(customListsNames, 'custom list')}
	model <- rGtkDataFrame(customListsNames)
	view <- gtkTreeView(model)
	view$getSelection()$setMode("browse")
	column  <- gtkTreeViewColumn("Lists", gtkCellRendererText(), text = 0)
	view$appendColumn(column)
	widgets$listsScrolledWindow$remove(widgets$listsScrolledWindow$getChild())
	widgets$listsScrolledWindow$add(view)
}

# Side effect : write in customLists and customListsNames
on_removeListButton_clicked <- function(widget) {
	toBeRemoved <- widgets$listsScrolledWindow$getChild()$getSelection()$getSelectedRows()$retval[[1]]$getIndices()[[1]]
	customLists[[toBeRemoved + 1]] <<- NULL
	customListsNames  <<- customListsNames[-(toBeRemoved + 1)] 	
	model <- rGtkDataFrame(customListsNames)
	view <- gtkTreeView(model)
	view$getSelection()$setMode("browse")
	column  <- gtkTreeViewColumn("Lists", gtkCellRendererText(), text = 0)
	view$appendColumn(column)
	widgets$listsScrolledWindow$remove(widgets$listsScrolledWindow$getChild())
	widgets$listsScrolledWindow$add(view)
	
}

#' Change the current step
#' 
#' Desactivate widgets and change currentStep global variable. Does not change focus or hide or unhide windows.
#' 
#' @param step integer, currently between 0 (file loading screen) and 4
setCurrentStep <- function(step) {	
	if (step == 1) {
		widgets$intersectFrame$setSensitive(FALSE)
		widgets$PCAFrame$setSensitive(FALSE)
		widgets$vennDiagramsFrame$setSensitive(FALSE)
		# TODO : finish
	}
	else if (step == 2) {
		widgets$PCAFrame$setSensitive(FALSE)
		widgets$intersectFrame$setSensitive(TRUE)
		widgets$vennDiagramsFrame$setSensitive(TRUE)
	}
	else if (step == 3) {
		widgets$PCAFrame$setSensitive(TRUE)
		widgets$intersectFrame$setSensitive(TRUE)
		widgets$vennDiagramsFrame$setSensitive(TRUE)
	}
	currentStep <<- step
}
