setwd("/home/benoit/workspace/ProjetEnjeuSBT/R")

# Note that this app code does not separate the model (which is mainly represented 
# by global variables) from the controler (callbacks) from the view (RGtk2 GUI)
# If this app was to grow, it would be necessary to refactor the code

# Load required packages
source("loadPackages.R")

# Create three utility functions to manage drawing on GtkDrawingArea using Cairo
source("deviceManager.R")

# Create global variables used trhougout the program, including widgets
# This is nto ideal for functionnal programming but R does not allow another easy way
source("init.R")

# Create parts of the GUI related to the window "Dataset details"
source("dataDetailsGUI.R")

# Manage Venn Diagram drawing
source("vennDiagrams.R")

# Manage parts of the GUI related to loading the dataset and adapting the GUI to it
source("loadWindowGui.R")

# Manage the creation of gene lists
source("intersectsWindowGui.R")

# Manage PCAs (including drawing)
source("PCAGui.R")

# Manage clusterings (including drawing)
source("clustering.R")

# Manage saves
source("saveManager.R")

# To be executed at the end ; actual launching of the main program loop
source("end.R")
