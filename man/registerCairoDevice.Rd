\name{registerCairoDevice}
\alias{registerCairoDevice}
\title{Open a Cairo device bound to a GtkDrawingArea}
\usage{
  registerCairoDevice(mydevice)
}
\arguments{
  \item{mydevice}{a GtkDrawable (usually a GtkDrawingArea)
  to draw on. Must be visible.}
}
\value{
  \code{TRUE} is the new device was successfully opened,
  \code{FALSE} if the device was not visible or already
  opened.
}
\description{
  This function open a device using Cairo to draw on a
  GtkDrawable (usually a GtkDrawingArea). If the device is
  already open, nothing will happen. If the \code{mydevice}
  is not visible, the function will fail (it is not
  possible to create a device with a non-visible
  GtkDrawable). This function does two things : first,
  changing the current device to \code{mydevice}, then,
  adding a key to \code{mydevice} (which is a gObject)
  under the name "device.number" to store the device number
  allocated to this GtkDrawable. This function ishould not
  be used to change the current device ; to do this, see
  \code{\link{changeCairoDevice}}
}
\examples{
\dontrun{
require(RGtk2)
device <- gtkDrawingAreaNew()
device$setVisible(TRUE)
registerCairoDevice(device)
}
}
\seealso{
  \code{\link{changeCairoDevice}}
  \code{\link{deleteCairoDevice}}
}

