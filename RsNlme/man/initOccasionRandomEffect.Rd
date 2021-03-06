% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pml_model.r
\name{initOccasionRandomEffect}
\alias{initOccasionRandomEffect}
\title{initOccasionRandomEffect}
\usage{
initOccasionRandomEffect(.Object, covariateName)
}
\arguments{
\item{covariateName}{Name of the occasion covariate}

\item{model}{A PK/PD model}
}
\description{
Lists/sets initial values for an inter-occasion variability
}
\examples{
    initOccasionRandomEffect(pkpdmodel,"Occasion")

    initOccasionRandomEffect(pkpdmodel,"Occasion") = c(0.1,0.02,0.1)

}
