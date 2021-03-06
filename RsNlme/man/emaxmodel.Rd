% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pml_model.r
\name{emaxmodel}
\alias{emaxmodel}
\title{emaxmodel}
\usage{
emaxmodel(isPopulation = TRUE, checkBaseline = FALSE,
  checkFractional = FALSE, checkInhibitory = FALSE,
  checkSigmoid = FALSE, covariates = c(), modelName = "",
  workingDir = "")
}
\arguments{
\item{isPopulation}{Population vs Individual models}

\item{checkBaseline}{Does EMAX have a baseline}

\item{checkFractional}{Is EMAX fractional}

\item{checkInhibitory}{Is EMAX inhibitory}

\item{checkSigmoid}{Is EMAX sigmoid}

\item{covariates}{List of covariates defined for the model}

\item{modelName}{Visual name for the model}

\item{workingDir}{Working directory to run the model}
}
\description{
Method to create an Emax PML model
}
\examples{
       model=emaxmodel()

       OR

       model=emaxmodel(checkBaseline=TRUE,
                           checkFractional=TRUE,
                           checkInhibitory=TRUE))

       print(attr(model,"statements"))
       "test(){"
       "    covariate(C)"
       "    E = E0 * (1  -  Imax * C / (IC50 + C))"
       "    error(EEps=1)"
       "    observe(EObs=E + EEps)"
       "    stparm(IC50 = tvIC50 * exp(nIC50))"
       "    stparm(E0 = tvE0 * exp(nE0))"
       "    stparm(Imax = tvImax * exp(nImax))"
       "    fixef( tvIC50 = c(,1,))"
       "    fixef( tvE0 = c(,1,))"
       "    fixef( tvImax = c(,1,))"
       "    ranef(diag(nIC50,nE0,nImax) =  c(1,1,1))"
       "}"

}
