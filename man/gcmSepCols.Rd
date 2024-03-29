% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gcmSepCols.R
\name{gcmSepCols}
\alias{gcmSepCols}
\title{Separate Components of ClimateNA File Names}
\usage{
gcmSepCols(files)
}
\arguments{
\item{files}{A list of ClimateNA future projection outputs.}
}
\value{
List
}
\description{
Using the file names from the projected ClimateNA future models data, a data frame splitting up the components of the file name (time frames, SSPs, and GCM models).
}
\examples{
files <- list.files()
models <- gcmSepCols(files)
}
\author{
Michael Burnett - UBC Faculty of Forestry
}
