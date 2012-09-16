\name{handlePCAComboBoxes}
\alias{handlePCAComboBoxes}
\title{Helper for updating to set the choices of two comboboxes}
\usage{
  handlePCAComboBoxes(nbPC, PC1ComboBox, PC2ComboBox)
}
\arguments{
  \item{nbPC}{the number of choices to display}

  \item{PC1ComboBox}{a combobox to be changed}

  \item{PC2ComboBox}{another combobox to be changed}
}
\value{
  always \code{TRUE}
}
\description{
  This function will set the choices of two comboxes to
  "PC1 PC2 ... PCn" where n can be choosed Warning : this
  will trigger the "choice_set" signal of the comboboxes
}
\seealso{
  \code{\link{drawPCA}}
}

