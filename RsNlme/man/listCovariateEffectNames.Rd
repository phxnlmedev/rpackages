% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pml_model.r
\name{listCovariateEffectNames}
\alias{listCovariateEffectNames}
\title{listCovariateEffectNames}
\usage{
listCovariateEffectNames(.Object)
}
\arguments{
\item{model}{PK/PD model}
}
\description{
Lists covariate effect names in the model
}
\examples{
   listCovariateEffectNames(model)
       "dVdweight"  "dVdage"     "dCldsex2"   "dCldweight"

   scenario=NlmeScenario("scenario1", "1,3")
       defines a scenario for covariate effect dVdweight and dCldsex2

}
