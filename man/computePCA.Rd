\name{computePCA}
\alias{computePCA}
\title{Compute the Principal Component Analysis of a data frame}
\usage{
  computePCA(mydata)
}
\arguments{
  \item{mydata}{a data frame which will be transposed and
  submitted to prcomp}
}
\value{
  an object of type \code{prcomp}
}
\description{
  This is merely a wrapper of \code{\link{prcomp}}
}
\seealso{
  \code{\link{handlePCAComboBoxes}}, \code{\link{drawPCA}}
}

