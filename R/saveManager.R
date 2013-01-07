#' Save current results in a folder
#' 
#' This function uses global variables created by the package (thus need only one parameter)
#' Create the following files :
#'   gene lists:
#'     step1/genesUp.csv			list of up genes in each group (after step 1)
#'     step1/genesDown.csv 			list of down genes in each group (after step 1)
#'     step1/selectionCriteria.txt 	folds and p-values use to create these lists in step 1
#'     step2/selectedGenes.txt 		list of all selected genes (after step 2)
#'     step3/selectedData.csv		the data frame used for PCA analysis in step 3
#'   PCA
#'     PCA.tiff and PCA.svg 	PCA plots
#'     components.txt 			variances and cumulated variances
#'     correlationMatrix.csv 	computed correlation matrix
#'     projections.csv 			indicate the projection of each gene on eigenvectors
#'   clustering
#'     clustering.tiff and clustering.svg	clustering plots
#'   vennDiagrams
#'     upVennDiagram.tiff and downVennDiagram.tiff	Venn diagrams as displayed on the main window
#' 	
#' @param saveFolder the path to the folder which will contain the results.
saveResults <- function(saveFolder) {
	sepCharacter <- widgets$colEntry$getText()
	decCharacter <- widgets$decEntry$getText()
	print(sepCharacter)
	
	# Step 1
	if (currentStep >= 2) { # If we are at step 2, step 1 is finished
		unlink(file.path(saveFolder, "step1"), recursive = TRUE)	
		dir.create(file.path(saveFolder, "step1"))
		
		file = file.path(saveFolder, "step1", "criteria.txt")
		write(buildCriteriaString(), file = file)
		
		# Saving genesUp.txt		
		for(i in 1:length(genesUp)) {
			serie = paste(i, names(baseData)[(i-1)*(nbReplicats*2+2) + 2], names(baseData)[(i-1)*(nbReplicats*2+2)+nbReplicats+2], "etc", sep ="_")
			file = file.path(saveFolder, "step1", paste0("up", serie, ".csv"))
			y <- genesUp[[i]]
			write(paste(y, baseData[y,1], sep = sepCharacter), file = file)		
		}
		
		# Saving genesDown.txt
		for(i in 1:length(genesUp)) {
			serie = paste(i, names(baseData)[(i-1)*(nbReplicats*2+2) + 2], names(baseData)[(i-1)*(nbReplicats*2+2)+nbReplicats+2], "etc", sep ="_")
			file = file.path(saveFolder, "step1", paste0("down", serie, ".csv"))
			y <- genesDown[[i]]
			write(paste(y, baseData[y,1], sep = sepCharacter), file = file)		
		}
		
		file = file.path(saveFolder, "step1", "selectionCriteria.txt")
		write(paste("Up : fold between",foldUpMin,"and", foldUpMax,"\n Down : fold between",
						foldDownMin,"and",foldDownMax))
		
		drawVennDiagrams(widget = NULL, fileUp = file.path(saveFolder, "step1", "vennDiagramUp.tiff"),
						fileDown = file.path(saveFolder, "step1", "vennDiagramDown.tiff"))
	}
	
	
	# Step 2
	if (currentStep >= 3) {
		unlink(file.path(saveFolder, "step2"), recursive = TRUE)	
		dir.create(file.path(saveFolder, "step2"))	
		file = file.path(saveFolder, "step2", "selectedGenes.csv")
		print(sepCharacter)
		print(head(selectedData))
		write.table(selectedData, file = file, sep = sepCharacter, dec = decCharacter, row.names = TRUE)
	}
		
	# Step 3
	if (currentStep >= 3) { # this is not a mistake
		unlink(file.path(saveFolder, "step3"), recursive = TRUE)	
		dir.create(file.path(saveFolder, "step3"))	
		file = file.path(saveFolder, "step3", "PCA.tiff")
		drawPCA(widgets$PC1ComboBox$getActive()+1, widgets$PC2ComboBox$getActive()+1, PCAdata, names(selectedData[,-1]), file, printToFile = TRUE)
		file = file.path(saveFolder, "step3", "PCA.svg")
		drawPCA(widgets$PC1ComboBox$getActive()+1, widgets$PC2ComboBox$getActive()+1, PCAdata, names(selectedData[,-1]), file, printToFile = TRUE)
		
		file = file.path(saveFolder, "step3", "eigenValues.tiff")
		drawEigenValues(PCAdata, file, printToFile = TRUE)
		file = file.path(saveFolder, "step3", "clustering.tiff")
		drawClustering(selectedData[,-1], file, printToFile = TRUE)
		
		file = file.path(saveFolder, "step3", "components.txt")
		capture.output(summary(PCAdata), file=file)
		
		file = file.path(saveFolder, "step3", "correlationMatrix.csv")
		correl = t(t(PCAdata$rotation)*PCAdata$sdev) # Correlation matrix
		write.table(correl, file = file, sep = sepCharacter, dec = decCharacter, row.names = TRUE)
	}
	
	# Step 4
	if (currentStep >= 5) {
		unlink(file.path(saveFolder, "step4"), recursive = TRUE)	
		dir.create(file.path(saveFolder, "step4"))	
		
		file = file.path(saveFolder, "step4", "correlation_limits.txt")
		write(buildLimitersString(), file = file)		
		
		file = file.path(saveFolder, "step4", "PCA.tiff")
		drawPCA(widgets$PC1FinalComboBox$getActive()+1, widgets$PC2FinalComboBox$getActive()+1, PCAfinalData, names(selectedData[,-1]), file, printToFile = TRUE)
		file = file.path(saveFolder, "step4", "PCA.svg")
		drawPCA(widgets$PC1FinalComboBox$getActive()+1, widgets$PC2FinalComboBox$getActive()+1, PCAfinalData, names(selectedData[,-1]), file, printToFile = TRUE)
		
		file = file.path(saveFolder, "step4", "eigenValues.tiff")
		drawEigenValues(PCAfinalData, file, printToFile = TRUE)
		file = file.path(saveFolder, "step4", "clustering.tiff")
		drawClustering(finalData[,-1], file, printToFile = TRUE)
		
		file = file.path(saveFolder, "step4", "components.txt")
		capture.output(summary(PCAfinalData), file=file)
		
		file = file.path(saveFolder, "step4", "correlationMatrix.txt")
		correl = t(t(PCAfinalData$rotation)*PCAfinalData$sdev) # Correlation matrix
		write.table(correl, file = file, sep = sepCharacter, dec = decCharacter, row.names = TRUE)
	}
}

buildCriteriaString <- function() {
	res = "Criteria used to define up or down genes\n\nUp : "
	if(widgets$useUpperLimitButton$getActive()) {
		res = paste0(res, "fold between ", foldUpMin, " and ", foldUpMax)
	} else {
		res = paste0(res, "fold greater than ", foldUpMin)
	}
	res = paste0(res, "\nDown : ")
	if(widgets$useLowerLimitButton$getActive()) {
		res = paste0(res, "fold between ", foldDownMin, " and ", foldDownMax)
	} else {
		res = paste0(res, "fold lower than ", foldDownMax)
	}
	res = paste0(res, "\n\np-value lower than ", pvalMax)
	return(res)
}

buildLimitersString <- function() {
	res = ""
	nbPC <- ncol(PCAdata$rotation)
	for(i in 1:nbPC) {
		res = paste0(res, "|PC", i, "| between ")
		res = paste0(res, getLimiterValue(i, 1), " and ", getLimiterValue(i, 2))
		res = paste0(res, "\n")
	}
	return(res)
}

buildListsString <- function() {
	res = "Criteria used to build the selected genes list\n\n"
	if(widgets$option1RadioButton$getActive()) {
		res = paste0(res, "Default\nA gene is selected if and only if:\nit is up in at least 2 groups; or\nit is down in at least two groups")
	}
	if(widgets$option2RadioButton$getActive()) {
		x <- widgets$nbGroupsSpinButton$getValueAsInt()
		res = paste0(res, "A gene is selected if and only if:\nit is up in at least ",x," groups; or\nit is down in at least ",x," groups")
	}
	if(widgets$option3RadioButton$getActive()) {
		res = paste0(res, "Default + exclude divergent genes\n")
		res = paste0(res, "A gene is selected if and only if:\n")
		res = paste0(res, "it is up in at least 2 groups; or\nit is down in at least two groups")
		res = paste0(res, "but not if it is up in 2 groups and down in the third one\n")
		res = paste0(res, "and not if it is down in two groups but up in the third one.")
	}
	if(widgets$option4RadioButton$getActive()) {
		res = paste0(res, "Custom lists used")
	}

	return(res)
}
	
on_saveButton_clicked <- function(widget) {	
	saveFolder <- widgets$saveFolderChooserButton$getFilename() # Retrieve folder to save in
	saveResults(saveFolder)
}

on_saveButton1_clicked <- function(widget) {	
	saveFolder <- widgets$saveFolderStep4ChooserButton$getFilename() # Retrieve folder to save in
	saveResults(saveFolder)
}
