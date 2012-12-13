\name{drawVennDiagrams}
\alias{drawVennDiagrams}
\title{Compute and draw Venn diagrams to a GtkDrawable or a file}
\usage{
  drawVennDiagrams(widget = NULL, fileUp = NULL,
    fileDown = NULL)
}
\arguments{
  \item{widget}{used for callbacks, ignored}

  \item{fileUp}{if set to NULL, will draw the "up diagram"
  to widgets$upDrawingArea. Otherwise, fileUp must be a
  string indicating the path where the diagram will be
  plotted}

  \item{fileDown}{same principle for the "down diagram"}
}
\description{
  This function uses global variables but does not write in
  global variables
}

