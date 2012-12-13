\name{saveResults}
\alias{saveResults}
\title{Save current results in a folder}
\usage{
  saveResults(saveFolder)
}
\arguments{
  \item{saveFolder}{the path to the folder which will
  contain the results.}
}
\description{
  This function uses global variables created by the
  package (thus need only one parameter) Create the
  following files : gene lists: step1/genesUp.csv list of
  up genes in each group (after step 1) step1/genesDown.csv
  list of down genes in each group (after step 1)
  step1/selectionCriteria.txt folds and p-values use to
  create these lists in step 1 step2/selectedGenes.txt list
  of all selected genes (after step 2)
  step3/selectedData.csv the data frame used for PCA
  analysis in step 3 PCA PCA.tiff and PCA.svg PCA plots
  components.txt variances and cumulated variances
  correlationMatrix.csv computed correlation matrix
  projections.csv indicate the projection of each gene on
  eigenvectors clustering clustering.tiff and
  clustering.svg clustering plots vennDiagrams
  upVennDiagram.tiff and downVennDiagram.tiff Venn diagrams
  as displayed on the main window
}

