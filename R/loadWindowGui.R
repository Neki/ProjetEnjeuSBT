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
							"Unable to retrieve the number of experiment groups.\nCheck the number of replicates and the integrity of your data file.\n\nConsult he manual for help.", 
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

# Configure the next window according to the dataset
configureIntersectsWindow <- function() {	
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
}


on_confirmButton_clicked <- function(widget) {
	if (is.null(baseData)) {
		widgets$fileInfoLabel$setText("Please select a valid data file and set the number of replicates used.")
	} else if (!dataFileOK) {
		widgets$fileInfoLabel$setText("Please set the number of replicates used.")
	} else {
		configureIntersectsWindow()
		setCurrentStep(1)
		widgets$loadFileWindow$hide()
		widgets$intersectsWindow$show()
		# configureCustomListsWindow() # now done when the user asks for this window
		# Configuring data information window
		# configureDataInformation()
	}
}