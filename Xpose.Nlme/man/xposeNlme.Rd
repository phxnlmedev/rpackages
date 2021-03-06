% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xpose_nlme.r
\name{xposeNlme}
\alias{xposeNlme}
\title{xposeNlme : Imports results of an NLME run into xpose database}
\usage{
xposeNlme(dir = "", modelName = "", dmpFile = "dmp.txt",
  dataFile = "data1.txt", logFile = "nlme7engine.log",
  mapFile = "cols1.txt")
}
\arguments{
\item{dir}{path to Nlme Run directory}

\item{dmpFile}{optional Nlme generated S/R file}

\item{dataFile}{optional Input file for Nlme Run}

\item{logFile}{optional engine log file}

\item{mapFile}{column mapping file name}
}
\description{
xposeNlme : Imports results of an NLME run into xpose database
}
\examples{
       xp = xposeNlme("./")
       xp = xposeNlme(dmpFile="dmp.txt",dataFile="data1.txt",
                      logFile="nlme7engine.log")

       dv_vs_pred(xp)
       v_vs_ipred(xp)
       res_vs_pred(xp,res="IWRES")
       ind_plots(xp, res="IWRES")
       res_vs_idv(xp,res="WRES")

}
