on_refineButton_clicked <- function(widget) {
	setCurrentStep(4)
	widgets$refinementWindow$show()
	widgets$intersectsWindow$hide()
	configureRefinementWindow() # Must be done after the window is visible, otherwise plotting to GtkDrawingArea will have indetermined behaviour
}

on_backToStep3Button_clicked <- function(widget) {
	setCurrentStep(3)
	widgets$refinementWindow$hide()
	widgets$intersectsWindow$show()	
}

on_saveFolderStep4ChooserButton_file_set <- function(widget) {
	widgets$saveFolderChooserButton$setFilename(widgets$saveFolderStep4ChooserButton$getFilename())
}

# Side effect : write in correlationLimits and finalData
configureRefinementWindow <- function() {
	# Let's remove any older child widget
	children <- widgets$limitersVBox$getChildren()
	for(i in 1:length(children)) {
		children[[i]]$destroy()
	}
	nbPC <- ncol(PCAdata$rotation)
	
	# and now we can add our limiters
	for(i in 1:nbPC) {
		addCorellationLimiter(i)
	}
	handlePCAComboBoxes(nbPC, widgets$PC1FinalComboBox, widgets$PC2FinalComboBox)
	
	# Update dataset name
	widgets$datasetNameLabel2$setText(paste("Dataset:",widgets$dataFileChooserButton$getFilename()))
	
	# Initial PCA and clustering
	# IF THERE ARE PROBLEMS CHECK THIS *************************************************
	# And now a dirty hack
	# If I omit the next line, I get an error "plot.new() : margins too large"
	# I suspect that RGtk2 does some poor multithreading and that the next window is still not visible when I call changeCairoDevice()
	# which results in weird behaviour in drawing area handling
	# Oh, and checking wether widgets$finalEigenArea is visible before processing does not work
	Sys.sleep(0.2)
	on_finalUpdateButton_clicked(NULL)
}

addCorellationLimiter <- function(PCi) {
	hbox <- gtkHBoxNew(homogeneous = FALSE)
	ltLabel <- gtkLabelNew(paste(" < |PC", PCi, "| < ", sep =""))
	minSpinButton <- gtkSpinButtonNewWithRange(min = 0, max = 1, step = 0.01)
	maxSpinButton <- gtkSpinButtonNewWithRange(min = 0, max = 1, step = 0.01)
	minSpinButton$setValue(-1)
	maxSpinButton$setValue(1)
	hbox$packStart(minSpinButton, expand = FALSE, fill = TRUE)
	hbox$packStart(ltLabel, expand = FALSE, fill = TRUE)
	hbox$packStart(maxSpinButton, expand = FALSE, fill = TRUE)
	widgets$limitersVBox$add(hbox)
}

# col = 1 to retrieve min limiter, col = 2 to retrieve max limiter
getLimiterValue <- function(PCi, col) {
	children <- widgets$limitersVBox$getChildren()[[PCi]]$getChildren()
	if(col == 1) {
		return(children[[1]]$getValue())
	} else if(col == 2) {
		return(return(children[[3]]$getValue()))
	} else {
		stop(paste("Invalid parameter : col must be equal to 1 or 2, while you provided :", col))
	}
}

# This function read the correlation limit values, discard genes which do not meet these criteria,
# then calls computePCA and return the result
# Side effect : write in finalData
computeFinalPCA <- function() {
	nbPC <- ncol(PCAdata$rotation)
	finalData <<- selectedData
	minPC <- c()
	maxPC <- c()
	vec <- data.frame()
	for(i in 1:nbPC) {
		minPC = c(minPC, getLimiterValue(i, 1))
		maxPC = c(maxPC, getLimiterValue(i, 2))	
	}
	correl = t(t(PCAdata$rotation)*PCAdata$sdev) # Building correlation matrix
	for(i in 1:nbPC) {
		vec <- (correl[,i] > minPC[i] & correl[,i] < maxPC[i]) | (correl[,i] > -maxPC[i] & correl[,i] < -minPC[i] )
		finalData <<- finalData[vec,] # warning : finalData, after 1 iteration, no longer has the same number of rows as correl
		correl <- correl[vec,] # making sure that correl always has the same number of rows as finalData
	}
	return(computePCA(finalData[,-1]))
}

on_finalUpdateButton_clicked <- function(widget) {
	PCAfinalData <<- computeFinalPCA()
	# debugonce(updateFinalPCA)
	updateFinalPCA(widget) # Draw the PCA plot
	drawClustering(finalData[,-1], widgets$finalClusteringArea, FALSE)
	
	drawEigenValues(PCAfinalData, widgets$finalEigenArea, FALSE)
	widgets$finalSelectedGenesLabel$setText(paste(nrow(finalData), "genes selected"))
	setCurrentStep(5)
}