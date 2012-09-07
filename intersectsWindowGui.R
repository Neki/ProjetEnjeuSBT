# Creating callbacks

on_useUpperLimitButton_toggled <- function(widget) {
	if(!widgets$useUpperLimitButton$getActive()) widgets$foldUpMaxSpinButton$setSensitive(FALSE)
	if(widgets$useUpperLimitButton$getActive()) widgets$foldUpMaxSpinButton$setSensitive(TRUE)	
	widgets$intersectFrame$setSensitive(FALSE)
	widgets$PCAFrame$setSensitive(FALSE)
}

on_useLowerLimitButton_toggled <- function(widget) {
	if(!widgets$useLowerLimitButton$getActive()) widgets$foldDownMinSpinButton$setSensitive(FALSE)
	if(widgets$useLowerLimitButton$getActive()) widgets$foldDownMinSpinButton$setSensitive(TRUE)	
	widgets$intersectFrame$setSensitive(FALSE)
	widgets$PCAFrame$setSensitive(FALSE)

}

on_foldUpMinSpinButton_value_changed <- function(widget) {
	foldUpMin <<- widgets$foldUpMinSpinButton$getValue()
	if(widgets$linkFoldLimitsCheckButton$getActive()) widgets$foldDownMaxSpinButton$setValue(-foldUpMin)
	if(widgets$foldUpMaxSpinButton$getValue() < foldUpMin) widgets$foldUpMaxSpinButton$setValue(foldUpMin)
	widgets$intersectFrame$setSensitive(FALSE)
	widgets$PCAFrame$setSensitive(FALSE)
}

on_foldUpMaxSpinButton_value_changed <- function(widget) {
	foldUpMax <<- widgets$foldUpMaxSpinButton$getValue()
	if(widgets$linkFoldLimitsCheckButton$getActive()) widgets$foldDownMinSpinButton$setValue(-foldUpMax)
	if(foldUpMax < widgets$foldUpMinSpinButton$getValue()) widgets$foldUpMinSpinButton$setValue(foldUpMax)
	widgets$intersectFrame$setSensitive(FALSE)
	widgets$PCAFrame$setSensitive(FALSE)
}


on_foldDownMinSpinButton_value_changed <- function(widget) {
	foldDownMin <<- widgets$foldDownMinSpinButton$getValue()
	if(widgets$linkFoldLimitsCheckButton$getActive()) widgets$foldUpMaxSpinButton$setValue(-foldDownMin)
	if(widgets$foldDownMaxSpinButton$getValue() < foldDownMin) widgets$foldDownMaxSpinButton$setValue(foldDownMin)
	widgets$intersectFrame$setSensitive(FALSE)
	widgets$PCAFrame$setSensitive(FALSE)
}

on_foldDownMaxSpinButton_value_changed <- function(widget) {
	foldDownMax <<- widgets$foldDownMaxSpinButton$getValue()
	if(widgets$linkFoldLimitsCheckButton$getActive()) widgets$foldUpMinSpinButton$setValue(-foldDownMax)
	if(foldDownMax < widgets$foldDownMinSpinButton$getValue()) widgets$foldDownMinSpinButton$setValue(foldDownMax)
	widgets$intersectFrame$setSensitive(FALSE)
	widgets$PCAFrame$setSensitive(FALSE)
}

on_pvalueSpinButton_value_changed <- function(widget) {
	widgets$intersectFrame$setSensitive(FALSE)
	pvalMax  <<- widgets$pvalueSpinButton$getValue()
}

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
	widgets$intersectFrame$setSensitive(TRUE)
	# Drawing Venn Diagrams using the function in vennDiagramms.R
	widgets$intersectsStatusBar$push(widgets$intersectsStatusBar$getContextId("info"), "Lists created, drawing Venn Diagrams...")
	drawVennDiagrams()
	
}

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
	widgets$PCAFrame$setSensitive(TRUE)
	computePCA()
	drawPCA()
	drawClustering()
}

on_customListsButton_clicked <- function(widget) {
	widgets$customListsWindow$show()
}

on_confirmListsButton_clicked <- function(widget) {
	widgets$customListsWindow$hide()
	#TODO : finish
}

updateButtonsState <- function(widget, a) {
	if(buttonState[a[1], a[2]]) {buttonState[a[1], a[2]] <<- FALSE} else {buttonState[a[1], a[2]]  <<- TRUE}
}

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


