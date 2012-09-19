\name{deleteCairoDevice}
\alias{deleteCairoDevice}
\title{Close a Cairo Device linked to a GtkDrawable}
\usage{
  deleteCairoDevice(mydevice)
}
\arguments{
  \item{mydevice}{a GtkDrawable used as a device (the
  function \code{\link{changeCairoDevice}} or
  \code{\link{registerCairoDevice}} should have been used
  on the GtkDrawable before).}
}
\description{
  Warning : this also delete the GtkDrawable.
}
\seealso{
  \code{\link{registerCairoDevice}}
  \code{\link{changeCairoDevice}}
}

