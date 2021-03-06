% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dosing.r
\docType{class}
\name{DosePoint-class}
\alias{DosePoint-class}
\alias{DosePoint}
\title{DosePoint}
\arguments{
\item{isZeroOrderAbsorption}{One of No|RateDose|DurationDose}

\item{isBioavail}{Does dose point has bioavailability}

\item{bioavailExpression}{Bioavailability expression}

\item{isTlag}{Does dose have time lag}

\item{tlagExpression}{Time lag expression}

\item{rateExpression}{Rate or Duration Expression has the form
StructuralParameter is created if new to the model.
Optional Formula defines how to express dose in PML}

\item{dobefore}{Code to execute before dose is administrated}

\item{doafter}{Code to execute after dose is administrated}
}
\description{
Defines a dosepoint for a compartment
}
\examples{

dosePoint= DosePoint(isZeroOrderAbsorption = DurationDose,
                     durationExpression = "DUR",
                     isTlag = FALSE,
                     isBioavail = TRUE,
                     bioavailExpression = "logitF1;1-ilogit(logitF1)")

absDosePoint= DosePoint(isZeroOrderAbsorption = NoDose,
                        isBioavail = TRUE,
                        bioavailExpression = "logitF1;ilogit(logitF1)")

}
