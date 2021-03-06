% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FillSetting.R
\name{FillSetting}
\alias{FillSetting}
\title{FillSetting}
\usage{
FillSetting(sim, GradTable, method = "hzar", tails = "none")
}
\arguments{
\item{sim}{A list of simulation summaries, output of \code{ReadSummary}.
Please refer to documentation of \code{ReadSummary} function for detailed usage.}

\item{GradTable}{A data.frame of simulations, output of \code{ReadSetting}.
Please refer to documentation of \code{ReadSetting} function for detailed usage.}

\item{method}{set to 'hzar' for fit performed by package hzar (default) or
'nlm' for fit performed by fit of logistic curve to cline using non-linear least square method.
'nlm' mehtod is faster, but does not perform well on clines deviating a lot from logistic shape
(multilocus clines, clines under strong drift ...)}

\item{tails}{('mirror') to fit logistic model with exponenctial functions as tails;
('none') to fit logistic model only}
}
\value{
A data.frame object, integrating information about the settings used in the simulation run and the summary of the simulation run in a R-friendly format.
}
\description{
\code{FillSetting} computes widths and centers of set of clines (simulations).
Both 1D and 2D simulations can be input
}
\examples{
{
   mysim=ReadSummary(nameIn='../../Conjunction/out')
   myGradTable=ReadSetting(InputFile='../../Conjunction/setting.txt')
   myFilledSetting = FillSetting(sim=mysim, GradTable=myGradTable)
}

}
\author{
Kamil Jaron \email{kamiljaron at gmail.com}
}
