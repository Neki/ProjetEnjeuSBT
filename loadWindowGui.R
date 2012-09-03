# Retrieving GUI elements 
loadFileWindow <- builder$getObject("loadFileWindow")
loadFileStatusbar  <- builder$getObject("loadFileStatusbar")
nbReplicatsSpinButton <- builder$getObject("nbReplicatsSpinButton")
fileInfoLabel <- builder$getObject("fileInfoLabel")
dataFileChooserButton <- builder$getObject("dataFileChooserButton")
intersectsWindow <- builder$getObject("intersectsWindow")
dataNameLabel <- builder$getObject("dataNameLabel")
listsToDisplayVBox <- builder$getObject("listsToDisplayVBox")
option3RadioButton <- builder$getObject("option3RadioButton")


csvFilter <- gtkFileFilterNew()
csvFilter$addPattern("*.csv")
dataFileChooserButton$setFilter(csvFilter)

# Creating callbacks
on_nbReplicatsSpinButton_value_changed <- function(widget) {
	if (!is.null(baseData)) {
		nbGenes <<- nrow(baseData)
		nbReplicats <<- nbReplicatsSpinButton$getValueAsInt()
		nbExperiments <<- (ncol(baseData) - 1) / (2 * (nbReplicats + 1) )
		if (nbExperiments == round(nbExperiments)) {
			nbSamples <-  nbExperiments*nbReplicats*2
			fileInfoLabel$setText(paste("Number of experiment groups : ",
						    nbExperiments,"\n",
						    "Number of samples : " ,
						    nbSamples,"\n",
						    "Number of genes : ",
						    nbGenes,
						    "\n\n\nPlease check those numbers before processing.",
						    sep="")
						)
			dataFileOK <<- TRUE
		} else {
			fileInfoLabel$setText(paste("Number of genes : ", nbGenes,
						    "\n",
						    "Unable to retrieve the number of experiment groups.\nCheck the number of replicats and the integrity of your data file.\n\nConsult he manual for help.", 
						    sep=""))
			dataFileOK <<- FALSE

		}
	}


}

on_dataFileChooserButton_file_set<- function(widget) {
	filename  <- dataFileChooserButton$getFilename()
	loadFileStatusbar$push(loadFileStatusbar$getContextId("info"), paste("Loading ", filename, ", please wait...", dec=decCharacter, sep =""))
	baseData <<- read.csv2(filename, header = TRUE, stringsAsFactors = FALSE, sep=sepCharacter, row.names=1)
	loadFileStatusbar$push(loadFileStatusbar$getContextId("info"), paste(filename, "successfully loaded."))
	on_nbReplicatsSpinButton_value_changed(widget)
}


on_confirmButton_clicked <- function(widget) {
	if (is.null(baseData)) {
		fileInfoLabel$setText("Please select a valid data file and set the number of replicats used.")
	} else if (!dataFileOK) {
		fileInfoLabel$setText("Please set the number of replicats used.")
	} else {
		# Configuring the next window according to the dataset
		dataNameLabel$setText(paste("Data :", basename(dataFileChooserButton$getFilename())))
		for (i in 1:nbExperiments) {
		listsToDisplayVBox$packStart(gtkCheckButton(paste(i,":", names(baseData)[(i-1)*(nbReplicats*2+2) + 2], names(baseData)[(i-1)*(nbReplicats*2+2)+nbReplicats+2], "etc.")), fill = FALSE)
		}
		# checkBoxesList[[1]] is a GtkLabel !
		checkBoxesList <- listsToDisplayVBox$getChildren()
		n  <- min(4, length(checkBoxesList)-1) 
		for (i in 1:n) {
			gtkToggleButtonSetActive(checkBoxesList[[i+1]],TRUE)
			gSignalConnect(checkBoxesList[[i+1]], "toggled", drawVennDiagrams)
		}
		option3RadioButton$setSensitive(nbExperiments==3)  
		loadFileWindow$destroy()
		intersectsWindow$show()
		# Configuring custom lists window according to dataset
		# gtkTableGetChildren() returns a list of objects which have for only type "GtkTableChild" so it is not possible to retrieve the state of the GtkCheckButtons
		# Workaround : using a global dataframe which update each time a check button is toggled, and connecting appropriate signals  
		optionTable$resize(rows = nbExperiments + 2, columns = 5)
        	for (i in 1:nbExperiments) {
                	optionTable$attachDefaults(gtkLabelNew(paste(i,":", names(baseData)[(i-1)*(nbReplicats*2+2) + 2], names(baseData)[(i-1)*(nbReplicats*2+2)+nbReplicats+2], "etc.")), 0, 1, i+1, i+2)
			button <- gtkCheckButtonNew()
			gSignalConnect(button, 'toggled', updateButtonsState,c(i, 1))
                	optionTable$attachDefaults(button, 1, 2, i+1, i+2)
			button <- gtkCheckButtonNew()
			gSignalConnect(button, 'toggled', updateButtonsState,c(i, 2))
                	optionTable$attachDefaults(button, 2, 3, i+1, i+2)
			button <- gtkCheckButtonNew()
        		gSignalConnect(button, 'toggled', updateButtonsState,c(i, 3))
         		optionTable$attachDefaults(button, 3, 4, i+1, i+2)
			button <- gtkCheckButtonNew()
			gSignalConnect(button, 'toggled', updateButtonsState, c(i,4))
                	optionTable$attachDefaults(button, 4, 5, i+1, i+2)
        	}
		buttonState <<- array(FALSE, c(4, nbExperiments))
	}
}
# Connecting signals
# builder$connectSignals()

# Displaying the GUI
# loadFileWindow$show()
