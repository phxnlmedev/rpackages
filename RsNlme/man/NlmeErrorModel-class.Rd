% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/error_model.r
\docType{class}
\name{NlmeErrorModel-class}
\alias{NlmeErrorModel-class}
\alias{NlmeErrorModel}
\title{NlmeErrorModel}
\arguments{
\item{effectsList}{List of residual effects to include in the error model}

\item{numberOfEffects}{Number of effects being included in the error model}
}
\description{
Class represents an NLME/PML error model
}
\examples{
resEff1=NlmeResidualEffect(ERR_ADDITIVE,"C")
resEff2=NlmeResidualEffect(ERR_SUM_EXP,"E")
errorModel=NlmeErrorModel(c(resEff1,resEff2))

}
