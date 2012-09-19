#' @include init.R

# Side effects : writing in nbGenes, nbReplicats, nbExperiments, dataFileOK
on_nbReplicatsSpinButton_value_changed <- function(widget) {
	if (!is.null(baseData)) {
		nbGenes <<- nrow(baseData)
		nbReplicats <<- widgets$nbReplicatsSpinButton$getValueAsInt()
		nbExperiments <<- (ncol(baseData) - 1) / (2 * (nbReplicats + 1) )
		if (nbExperiments == round(nbExperiments)) {
			nbSamples <-  nbExperiments*nbReplicats*2
			widgets$fileInfoLabel$setText(paste("Number of experiment groups : ",
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
			widgets$fileInfoLabel$setText(paste("Number of genes : ", nbGenes,
						    "\n",
						    "Unable to retrieve the number of experiment groups.\nCheck the number of replicats and the integrity of your data file.\n\nConsult he manual for help.", 
						    sep=""))
			dataFileOK <<- FALSE

		}
	}


}

# Side effect : writing in baseData
on_dataFileChooserButton_file_set<- function(widget) {
	filename  <- widgets$dataFileChooserButton$getFilename()
	sepCharacter <- widgets$colEntry$getText()
	decCharacter <- widgets$decEntry$getText()
	widgets$loadFileStatusbar$push(widgets$loadFileStatusbar$getContextId("info"), paste("Loading ", filename, ", please wait...",  sep =""))
	baseData <<- read.csv2(filename, header = TRUE, stringsAsFactors = FALSE, sep=sepCharacter, dec=decCharacter, row.names=1)
	widgets$loadFileStatusbar$push(widgets$loadFileStatusbar$getContextId("info"), paste(filename, "successfully loaded."))
	on_nbReplicatsSpinButton_value_changed(widget)
}


on_confirmButton_clicked <- function(widget) {
	if (is.null(baseData)) {
		widgets$fileInfoLabel$setText("Please select a valid data file and set the number of replicats used.")
	} else if (!dataFileOK) {
		widgets$fileInfoLabel$setText("Please set the number of replicats used.")
	} else {
		# Configuring the next window according to the dataset
		# TODO : use separate function to do this !
		widgets$dataButton$setLabel(paste("Data:", basename(widgets$dataFileChooserButton$getFilename())," (click to show details)" ))
		for (i in 1:nbExperiments) {
		widgets$listsToDisplayVBox$packStart(gtkCheckButton(paste(i,":", names(baseData)[(i-1)*(nbReplicats*2+2) + 2], names(baseData)[(i-1)*(nbReplicats*2+2)+nbReplicats+2], "etc.")), fill = FALSE, expand=FALSE)
		}
		# checkBoxesList[[1]] is a GtkLabel !
		checkBoxesList <- widgets$listsToDisplayVBox$getChildren()
		n  <- min(4, length(checkBoxesList)-1) 
		for (i in 1:n) {
			gtkToggleButtonSetActive(checkBoxesList[[i+1]],TRUE)
			gSignalConnect(checkBoxesList[[i+1]], "toggled", drawVennDiagrams)
		}
		widgets$option3RadioButton$setSensitive(nbExperiments==3)  
		widgets$loadFileWindow$hide()
		widgets$intersectsWindow$show()
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
		buttonState <<- array(FALSE, c(nbExperiments, 4))
		# Configuring data information window
		configureDataInformation()
	}
}