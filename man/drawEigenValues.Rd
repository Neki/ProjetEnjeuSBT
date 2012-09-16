\name{drawEigenValues}
\alias{drawEigenValues}
\title{Draw a graph representing the importance of PCA components to a GtkDrawable (usually a GtkDrawingArea) or a file}
\usage{
  drawEigenValues(widget = NULL, PCdata, drawingArea,
    plotToFile = FALSE)
}
\arguments{
  \item{widget}{used for callbacks, ignored #TODO : remove
  this parameter}

  \item{PCdata}{an object of type \code{prcomp}}

  \item{drawingArea}{a GtkDrawable (usually a
  GtkDrawingArea) if \code{\link{plotTofile}} is set to
  \code{FALSE}, otherwise the path to a file}

  \item{plotToFile}{a boolean. If set to \code{TRUE}, will
  plot to a file.}
}
\value{
  always \code{TRUE}
}
\description{
  This function supports .tiff and .svg extensions when
  plotting to a file. If another extension is given this
  function will raise an error.
}
\seealso{
  \code{\link{drawPCA}}
}

