# Retrieving GUI elements
dataNameLabel <- builder$getObject("dataNameLabel")
useUpperLimitButton <- builder$getObject("useUpperLimitButton")
useLowerLimitButton <- builder$getObject("useLowerLimitButton")
foldUpMaxSpinButton <- builder$getObject("foldUpMaxSpinButton")
foldDownMinSpinButton <- builder$getObject("foldDownMinSpinButton")
foldUpMinSpinButton <- builder$getObject("foldUpMinSpinButton")
foldDownMaxSpinButton <- builder$getObject("foldDownMaxSpinButton")
linkFoldLimitsCheckButton <- builder$getObject("linkFoldLimitsCheckButton")
pvalueSpinButton <- builder$getObject("pvalueSpinButton")
intersectsStatusBar <- builder$getObject("intersectsStatusBar")
option1RadioButton <- builder$getObject("option1RadioButton")
option2RadioButton <- builder$getObject("option2RadioButton")
nbGroupsSpinButton <- builder$getObject("nbGroupsSpinButton")
customListsWindow <- builder$getObject("customListsWindow")
listNameEntry  <- builder$getObject("listNameEntry")
tableAlignment <- builder$getObject("tableAlignment")
optionTable <- builder$getObject("optionTable")
listsScrolledWindow <- builder$getObject("listsScrolledWindow")

# Creating callbacks

on_useUpperLimitButton_toggled <- function(widget) {
	if(!useUpperLimitButton$getActive()) foldUpMaxSpinButton$setSensitive(FALSE)
	if(useUpperLimitButton$getActive()) foldUpMaxSpinButton$setSensitive(TRUE)	
}

on_useLowerLimitButton_toggled <- function(widget) {
	if(!useLowerLimitButton$getActive()) foldDownMinSpinButton$setSensitive(FALSE)
	if(useLowerLimitButton$getActive()) foldDownMinSpinButton$setSensitive(TRUE)	

}

on_foldUpMinSpinButton_value_changed <- function(widget) {
	foldUpMin <<- foldUpMinSpinButton$getValue()
	if(linkFoldLimitsCheckButton$getActive()) foldDownMaxSpinButton$setValue(-foldUpMin)
	if(foldUpMaxSpinButton$getValue() < foldUpMin) foldUpMaxSpinButton$setValue(foldUpMin)
}

on_foldUpMaxSpinButton_value_changed <- function(widget) {
	foldUpMax <<- foldUpMaxSpinButton$getValue()
	if(linkFoldLimitsCheckButton$getActive()) foldDownMinSpinButton$setValue(-foldUpMax)
	if(foldUpMax < foldUpMinSpinButton$getValue()) foldUpMinSpinButton$setValue(foldUpMax)
}


on_foldDownMinSpinButton_value_changed <- function(widget) {
	foldDownMin <<- foldDownMinSpinButton$getValue()
	if(linkFoldLimitsCheckButton$getActive()) foldUpMaxSpinButton$setValue(-foldDownMin)
	if(foldDownMaxSpinButton$getValue() < foldDownMin) foldDownMaxSpinButton$setValue(foldDownMin)
}

on_foldDownMaxSpinButton_value_changed <- function(widget) {
	foldDownMax <<- foldDownMaxSpinButton$getValue()
	if(linkFoldLimitsCheckButton$getActive()) foldUpMinSpinButton$setValue(-foldDownMax)
	if(foldDownMax < foldDownMinSpinButton$getValue()) foldDownMinSpinButton$setValue(foldDownMax)
}

on_pvalueSpinButton_value_changed <- function(widget) {
	pvalMax  <<- pvalueSpinButton$getValue()
}

on_createListsButton_clicked <- function(widget) {
	intersectsStatusBar$push(intersectsStatusBar$getContextId("info"), "Creating lists...")
	# Creating lists
	for(i in 1:nbExperiments) {
		folds <- baseData[i*(nbReplicats*2+2)-1]
		pvalues <- baseData[i*(nbReplicats*2+2)]
		if (useUpperLimitButton$getActive()) {genesUp[[i]]  <<- row.names(baseData)[pvalues < pvalMax & folds > foldUpMin & folds < foldUpMax]} else {genesUp[[i]]  <<- row.names(baseData)[pvalues < pvalMax & folds > foldUpMin]} 
		if (useLowerLimitButton$getActive()) {genesDown[[i]] <<- row.names(baseData)[pvalues < pvalMax & folds > foldDownMin & folds < foldDownMax]} else {genesDown[[i]]  <<- row.names(baseData)[pvalues < pvalMax & folds < foldDownMax]}
	}

	# Drawing Venn Diagrams using the function in vennDiagramms.R
	drawVennDiagrams()
	intersectsStatusBar$push(intersectsStatusBar$getContextId("info"), "Lists created.")
}

on_intersectsButton_clicked <- function(widget) {
	isUpList <- NULL
	isDownList  <- NULL
	for (i in 1:nbExperiments) {
		vecUp <- row.names(baseData) %in% upGenes[[i]]
		vecDown <- row.names(baseData) %in% downGenes[[i]]
		isUpList <- cbind(isUpList, vecUp)
		isDownList <- cbind(isDownList, vecDown)
	}
	if(option1RadioButton$getActive()) {
		upList <- row.names(baseData)[sum(isUpList) >= 2]
		downList <- row.names(baseData)[sum(isDownList) >=2]
	}
	if(option2RadioButton$getActive()) {
		x <- nbGroupsSpinButton$getValueAsInt()
		upList <- row.names(baseData)[sum(isUpList) >= x]
		downList <- row.names(baseData)[sum(isDownList) >=x]
	}
	if(option3RadioButton$getActive()) {
		upList <- row.names(baseData)[sum(isUpList) >= 2 & sum(isDownList) == 0]
		downList <- row.names(baseData)[sum(isDownList) >=2 & sum(isDownList) == 0]
	}


}

on_customListsButton_clicked <- function(widget) {
	customListsWindow$show()
}

on_confirmListsButton_clicked <- function(widget) {
	customListsWindow$hide()
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
	customLists <<- c(customLists, clist)
	if(listNameEntry$getText() != "") {customListsNames <<- c(customListsNames, listNameEntry$getText())} else {customListsNames <<-c(customListsNames, 'custom list')}
	model <- rGtkDataFrame(customListsNames)
	view <- gtkTreeView(model)
	view$getSelection()$setMode("browse")
	column  <- gtkTreeViewColumn("Lists", gtkCellRendererText(), text = 0)
	view$appendColumn(column)
	listsScrolledWindow$remove(listsScrolledWindow$getChild())
	listsScrolledWindow$add(view)
}

on_removeListButton_clicked <- function(widget) {
	toBeRemoved <- listsScrolledWindow$getChild()$getSelection()$getSelectedRows()$retval[[1]]$getIndices()[[1]]
	customLists[[toBeRemoved + 1]] <<- NULL
        customListsNames  <<- customListsNames[-(toBeRemoved + 1)] 	
	model <- rGtkDataFrame(customListsNames)
	view <- gtkTreeView(model)
	view$getSelection()$setMode("browse")
	column  <- gtkTreeViewColumn("Lists", gtkCellRendererText(), text = 0)
	view$appendColumn(column)
	listsScrolledWindow$remove(listsScrolledWindow$getChild())
	listsScrolledWindow$add(view)

}


# Displaying the GUI
builder$connectSignals()
loadFileWindow$show()
