% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pml_model.r
\name{modelColumnMapping<-}
\alias{modelColumnMapping<-}
\title{Sets mapping between model variables and data columns}
\usage{
modelColumnMapping(.Object) <- value
}
\arguments{
\item{List}{of variable/column pairs}
}
\description{
Sets mapping between model variables and data columns
}
\examples{
      modelColumnMapping(model)=c(id="Subject", CObs="Conc",A1="Amount")j

}
