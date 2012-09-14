\name{getExtension}
\alias{getExtension}
\title{Return the extension of a file}
\usage{
  getExtension(path)
}
\arguments{
  \item{path}{the path to a file, or a file name}
}
\value{
  the extension of the file pointed by \code{path}. The
  leading dot is not included in the result (see examples)
  so the extension of "example.tiff" is "tiff" (and not
  ".tiff").
}
\description{
  Return the extension of a file
}
\examples{
getExtension("/home/name/image.tiff")
getExtension("example.jpg")
}

