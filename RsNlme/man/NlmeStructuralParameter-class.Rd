% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/structural_param.r
\docType{class}
\name{NlmeStructuralParameter-class}
\alias{NlmeStructuralParameter-class}
\alias{NlmeStructuralParameter}
\title{NlmeStructuralParameter}
\arguments{
\item{name}{Name of the structural parameter}

\item{fixedEffName}{Name to use for fixed effects}

\item{randomEffName}{Name to use for random effects}

\item{hasRandomEffect}{Does the parameter have a random effect}

\item{style}{Parameter style:
LogNormal, Normal, Combination, Log, Logit, Custom}

\item{initialValue}{Initial value for the parameter}

\item{lowerBound}{Lower limit for the parameter value}

\item{upperBound}{Upper limit for the parameter value}

\item{units}{Unit of measurement for the parameter}

\item{isFrozen}{Is the parameter frozen}

\item{isSequential}{Estimate the parameter sequentially}

\item{ranEffInitValue}{Initial value for the random effect}

\item{code}{For Custom style, PML code to override the definition}

\item{extraCode}{Extra lines of code that relates to this parameter}
}
\description{
Class represents an NLME structural parameter
}
\examples{
     NlmeStructuralParameter(STP_SUM_ETA,"EC50")
     NlmeStructuralParameter(STP_SUM_EXP,"Imax")

}
