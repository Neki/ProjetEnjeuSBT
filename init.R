library(RGtk2)

sepCharacter = ";"
decCharacter = ","

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

# Creating a global GtkBuilder from which we will be able to retrieve the necesary GUI elements later
builder <- gtkBuilderNew()
builder$addFromFile("gui.glade")

