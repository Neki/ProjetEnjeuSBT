\name{changeCairoDevice}
\alias{changeCairoDevice}
\title{Change current Cairo device to change the target of R plot commands to a GtkDrawable}
\usage{
  changeCairoDevice(mydevice)
}
\arguments{
  \item{mydevice}{a GtkDrawable (usually a GtkDrawingArea)
  to be the target of next R plot commands}
}
\description{
  This function calls \code{\link{registerCairoDevice}}
  first, to there is no need to register the device
  manually. If the device is not already registered, it
  must be visible, otherwise the function will fail.
}
\examples{
\dontrun{
require(RGtk2)
device <- gtkDrawingAreaNew()
device$setVisible(TRUE)
changeCairoDevice(device)
plot(x=c(1,2,3), y=c(1,2,3), main="Example")
}
}
\seealso{
  \code{\link{registerCairoDevice}}
  \code{\link{deleteCairoDevice}}
}

