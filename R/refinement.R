on_refineButton_clicked <- function(widget) {
	configureRefinementWindow()
	widgets$refinementWindow$show()
	widgets$intersectsWindow$hide()
}

# Side effect : write in correlationLimits
configureRefinementWindow <- function(widget) {
	# Let's remove any older child widget
	children <- widgets$limitersVBox$getChildren()
	for(i in 1:length(children)) {
		children[[i]]$destroy()
	}
	nbPC <- ncol(PCAdata$rotation)
	correlationLimits <<- array(data = c(-1,1), dim = c(nbPC, 2))
	for(i in 1:nbPC) {
		addCorellationLimiter(i)
	}
}

addCorellationLimiter <- function(PCi) {
	hbox <- gtkHBoxNew(homogeneous = FALSE)
	ltLabel <- gtkLabelNew(paste(" < PC", PCi, " < ", sep =""))
	minSpinButton <- gtkSpinButtonNewWithRange(min = -1, max = 1, step = 0.01)
	maxSpinButton <- gtkSpinButtonNewWithRange(min = -1, max = 1, step = 0.01)
	minSpinButton$setValue(-1)
	maxSpinButton$setValue(1)
	hbox$packStart(minSpinButton, expand = FALSE, fill = TRUE)
	hbox$packStart(ltLabel, expand = FALSE, fill = TRUE)
	hbox$packStart(maxSpinButton, expand = FALSE, fill = TRUE)
	widgets$limitersVBox$add(hbox)
}

