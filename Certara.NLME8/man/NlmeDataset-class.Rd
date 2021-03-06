% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataset.r
\docType{class}
\name{NlmeDataset-class}
\alias{NlmeDataset-class}
\alias{NlmeDataset}
\title{NLME dataset object

Class represents an NLME dataset object}
\arguments{
\item{dataFile}{subject datafile}

\item{colDefFile}{datafile to model mapping file}

\item{modelFile}{PML model file}

\item{estimatesDefFile}{Initial estimates mapping file}

\item{estimatesDataFile}{Initial estimates values}

\item{doseDefFile}{dose column definition file}

\item{doseDataFile}{dose datafile}

\item{ranEffectDefFile}{random effects column definition file}

\item{ranEffectDataFile}{random effects data file}

\item{outputFilename}{Name of output file}

\item{phoenixSourceDir}{Directory containing phoenix generated files}

\item{workingDir}{Directory containing datafiles(default cwd)}
}
\description{
NLME dataset object

Class represents an NLME dataset object
}
\examples{
dataset = NlmeDataset(workingDir)

}
