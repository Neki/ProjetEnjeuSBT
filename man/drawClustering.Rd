\name{drawClustering}
\alias{drawClustering}
\title{Compute and draw clustering results}
\usage{
  drawClustering(widget = NULL, mydata, drawingArea,
    printToFile = FALSE)
}
\arguments{
  \item{widget}{used for callbacks, ignored}

  \item{mydata}{the data frame (or matrix) containing the
  data to work on. Columns are variables and rows are
  individuals.}

  \item{drawingArea}{a GtkDrawable (usually a
  GtkDrawingArea) if \code{printToFile} is \code{FALSE} or
  a path to a file on which the results are to be plotted
  if \code{printToFile} is \code{TRUE}.}

  \item{printToFile}{a boolean. If set to \code{TRUE}, the
  results will be printed in the file whose location is
  \code{drawingArea}. The type of the output (.svg or
  .tiff) depend on the filename provided in
  \code{drawingArea}.}
}
\description{
  This function is able to compute clustering and plot the
  results on a widget or in a file. It uses euclidean
  distances and the Ward method. Supported file formats are
  .svg and .tiff. If another extension is deteced, then the
  function will return an error.
}

