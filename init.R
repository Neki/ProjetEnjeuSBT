# Initializing global variables
baseData <- NULL
dataFileOK <- FALSE
foldUpMin <- 1.20
foldUpMax <- 1.20
foldDownMin <- -1.20
foldDownMax <- -1.20
pvalMax <- 0.06
nbReplicats  <- 0
nbGenes <- 0
nbSamples <- 0
nbExperiments <- 0
genesUp <- list()
genesDown <-list() 
checkBoxesList <- NULL
buttonState <- c()
customLists <- list()
customListsNames <- c()
selectedData <- data.frame()
PCAdata <- NULL

# Creating a global GtkBuilder from which we will be able to retrieve the necesary GUI elements 
builder <- gtkBuilderNew()
builder$addFromFile("gui.glade")

# Creating a global widget environment which will contain our widgets
widgets <- new.env()

# Populating this environment
widgets$loadFileWindow <- builder$getObject("loadFileWindow")
widgets$loadFileStatusbar  <- builder$getObject("loadFileStatusbar")
widgets$nbReplicatsSpinButton <- builder$getObject("nbReplicatsSpinButton")
widgets$fileInfoLabel <- builder$getObject("fileInfoLabel")
widgets$dataFileChooserButton <- builder$getObject("dataFileChooserButton")
widgets$intersectsWindow <- builder$getObject("intersectsWindow")
widgets$dataNameLabel <- builder$getObject("dataNameLabel")
widgets$listsToDisplayVBox <- builder$getObject("listsToDisplayVBox")
widgets$option3RadioButton <- builder$getObject("option3RadioButton")
widgets$dataNameLabel <- builder$getObject("dataNameLabel")
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
widgets$customListsWindow <- builder$getObject("customListsWindow")
widgets$listNameEntry  <- builder$getObject("listNameEntry")
widgets$tableAlignment <- builder$getObject("tableAlignment")
widgets$optionTable <- builder$getObject("optionTable")
widgets$listsScrolledWindow <- builder$getObject("listsScrolledWindow")
widgets$intersectFrame <- builder$getObject("intersectFrame")
widgets$PCAFrame <- builder$getObject("PCAFrame")
widgets$PCChooserHBox <- builder$getObject("PCChooserHBox")
widgets$PCAArea  <- builder$getObject("PCAArea") # naming convention changed due to a glitch in Glade
#widgets$PCAArea2 <- builder$getObject("PCAArea2")
widgets$PCChooserHBox <- builder$getObject("PCChooserHBox")
widgets$playwithButton <- builder$getObject("playwithButton")
widgets$eigenDrawingArea <- builder$getObject("eigenDrawingArea")
widgets$clusteringDrawingArea <- builder$getObject("clusteringDrawingArea")

widgets$PC1ComboBox <- gtkComboBoxNewText()
widgets$PC2ComboBox <- gtkComboBoxNewText()
widgets$PCChooserHBox$packStart(widgets$PC1ComboBox, FALSE)
widgets$PCChooserHBox$reorderChild(widgets$PC1ComboBox, 1)
widgets$PCChooserHBox$packStart(widgets$PC2ComboBox, FALSE)
widgets$PCChooserHBox$reorderChild(widgets$PC2ComboBox, 3)

