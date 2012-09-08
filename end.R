# # Displaying the GUI
gSignalConnect(widgets$PC1ComboBox, "changed", updatePCA)
gSignalConnect(widgets$PC2ComboBox, "changed", updatePCA)
gSignalConnect(widgets$PC1DataComboBox, "changed", updateInitialPCA)
gSignalConnect(widgets$PC2DataComboBox, "changed", updateInitialPCA)


builder$connectSignals()
widgets$loadFileWindow$show()
