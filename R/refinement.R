on_refineButton_clicked <- function(widget) {
	configureRefinementWindow()	
	widgets$refinementWindow$show()
	widgets$intersectsWindow$hide()
	setCurrentStep(4)
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
	finalData <<- selectedData
	widgets$finalSelectedGenesLabel$setText(paste(nrow(finalData), "genes selected"))
	drawClustering(finalData[,-1], widgets$finalClusteringArea, FALSE)
	drawEigenValues(PCAfinalData, widgets$finalEigenArea, FALSE)
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
	updateFinalPCA(widget) # Draw the PCA plot
	drawClustering(finalData[,-1], widgets$finalClusteringArea, FALSE)
	drawEigenValues(PCAfinalData, widgets$finalEigenArea, FALSE)
	widgets$finalSelectedGenesLabel$setText(paste(nrow(finalData), "genes selected"))
}