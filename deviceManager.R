registerCairoDevice <- function(mydevice) {
	# Check if the device was already registered
	if(!is.null(gObjectGetData(mydevice, "device.number"))) return(FALSE)

	asCairoDevice(mydevice)

	#Check if mydevice is visible
	if(is.null(dev.cur())) return (FALSE)

	gObjectSetData(mydevice, "device.number", data=dev.cur())
	return(TRUE)
}

changeCairoDevice <- function(mydevice) {
	# Registering the device (will fail if already registered)
	registerCairoDevice(mydevice)

	dev.set(gObjectGetData(mydevice, "device.number"))
}

deleteCairoDevice <- function(mydevice) {
	if(!is.null(gObjectGetData(mydevice, "device.number"))) dev.off(gObjectGetData(mydevice, "device.number"))
}
