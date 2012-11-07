#' Open a Cairo device bound to a GtkDrawingArea
#' 
#' This function open a device using Cairo to draw on a GtkDrawable (usually a GtkDrawingArea).
#' If the device is already open, nothing will happen. If the \code{mydevice} is not visible, 
#' the function will fail (it is not possible to create a device with a non-visible GtkDrawable).
#' This function does two things : first, changing the current device to \code{mydevice}, then,
#' adding a key to \code{mydevice} (which is a gObject) under the name "device.number" to store the device number
#' allocated to this GtkDrawable.
#' This function ishould not be used to change the current device ; to do this, see 
#' \code{\link{changeCairoDevice}}
#' 
#' @param mydevice a GtkDrawable (usually a GtkDrawingArea) to draw on. Must be visible.
#' @return \code{TRUE} is the new device was successfully opened, \code{FALSE} if
#' the device was not visible or already opened.
#' @seealso \code{\link{changeCairoDevice}} \code{\link{deleteCairoDevice}}
#' @examples \dontrun{
#' require(RGtk2)
#' device <- gtkDrawingAreaNew()
#' device$setVisible(TRUE)
#' registerCairoDevice(device)
#' }
registerCairoDevice <- function(mydevice) {
	# Check if the device was already registered
	if(!is.null(gObjectGetData(mydevice, "device.number"))) return(FALSE)
	
	asCairoDevice(mydevice)
	
	#Check if mydevice is visible
	if(is.null(dev.cur())) return (FALSE)
	
	gObjectSetData(mydevice, "device.number", data=dev.cur())
	return(TRUE)
}

#' Change current Cairo device to change the target of R plot commands to a GtkDrawable
#' 
#' This function calls \code{\link{registerCairoDevice}} first, to there is no need to register the device manually.
#' If the device is not already registered, it must be visible, otherwise the function will fail.
#' 
#' @param mydevice a GtkDrawable (usually a GtkDrawingArea) to be the target of next R plot commands
#' @seealso \code{\link{registerCairoDevice}} \code{\link{deleteCairoDevice}}
#' @examples \dontrun{
#' require(RGtk2)
#' device <- gtkDrawingAreaNew()
#' device$setVisible(TRUE)
#' changeCairoDevice(device)
#' plot(x=c(1,2,3), y=c(1,2,3), main="Example")
#' }
changeCairoDevice <- function(mydevice) {
	# Registering the device (will do nothing if already registered)
	registerCairoDevice(mydevice)
	
	dev.set(gObjectGetData(mydevice, "device.number"))
}

#' Close a Cairo Device linked to a GtkDrawable
#' 
#' Warning : this also delete the GtkDrawable.
#' 
#' @param mydevice a GtkDrawable used as a device (\code{\link{changeCairoDevice}} or
#' \code{\link{registerCairoDevice}} should have been used on the GtkDrawable before).
#' @seealso \code{\link{registerCairoDevice}} \code{\link{changeCairoDevice}}
deleteCairoDevice <- function(mydevice) {
	if(!is.null(gObjectGetData(mydevice, "device.number"))) dev.off(gObjectGetData(mydevice, "device.number"))
}

#' Return the extension of a file
#' 
#' @param path the path to a file, or a file name 
#' @return the extension of the file pointed by \code{path}. The leading dot is not included in
#' the result (see examples) so the extension of "example.tiff" is "tiff" (and not ".tiff").
#' @examples
#' \dontrun{
#' getExtension("/home/name/image.tiff")
#' getExtension("example.jpg")
#' }
getExtension <- function(path) {
	parts <- strsplit(path, "\\.")[[1]]
	last <- parts[length(parts)]
	return(last)
}