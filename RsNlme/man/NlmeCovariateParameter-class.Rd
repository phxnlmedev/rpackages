% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/covariate_param.r
\docType{class}
\name{NlmeCovariateParameter-class}
\alias{NlmeCovariateParameter-class}
\alias{NlmeCovariateParameter}
\title{NlmeCovariateParameter}
\arguments{
\item{name}{Name of covariate parameter}

\item{type}{Type of covariate parameter: Continuous|Category|Occasion}

\item{direction}{Curve fitting method: Forward|Interpolate|Backward}

\item{isDiagonal}{Is the structure diagonal (TRUE)| or box (FALSE)}

\item{centerValue}{Covariate centering value}

\item{isPositive}{Are the covariate values all positive}

\item{continuousType}{Type of value to use for the centering value: 
CovarNumber|CovarMean|CovarMedian}

\item{covarItems}{}
}
\description{
Class represents an NLME Covariate parameter
}
\examples{
      weight=NlmeCovariateParameter(name="weight",
                         type=Continuous,
                         continuousType=CovarNumber
                         centerValue="70",
                         direction=Forward)

}
