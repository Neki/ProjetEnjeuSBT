\name{drawPCA}
\alias{drawPCA}
\title{Draw a PCA correlation plot to a GtkDrawable (usually a GtkDrawingArea) or a file}
\usage{
  drawPCA(widget = NULL, PCx, PCy, PCdata, Veclabels,
    drawingArea, printToFile = FALSE)
}
\arguments{
  \item{widget}{used for callbacks (now ignored) #TODO :
  remove this parameter}

  \item{PCx}{the number of the principal component (as an
  integer) which will be the x coordinate of points}

  \item{PCy}{the number of the principal component (as an
  integer) which will be the y coordinate of points}

  \item{PCdata}{an object of type \code{\prcomp}}}

  \item{Veclabels}{a vector of strings containing the
  points labels}

  \item{drawingArea}{a GtkDrawable (usually a
  GtkDrawingArea) if printToFile is set to \code{FALSE}, a
  string indicating the path of the file to create
  otherwise.}

  \item{printToFile}{a boolean. If set to \code{FALSE},
  this function will plot to the GtkDrawable
  \code{drawingArea}. If set to \code{TRUE} this function
  will plot to the file whose path is \code{drawingArea}}
}
\value{
  \code{FALSE} if the function failed at plotting,
  \code{TRUE} otherwise
}
\description{
  This function supports .tiff and .svg extensions when
  plotting to a file. If another extension is given this
  function will raise an error.
}
\seealso{
  \code{\link{computePCA}},
  \code{\link{handlePCAComboBoxes}}
}

