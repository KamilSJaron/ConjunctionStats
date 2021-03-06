% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PlotHybridZone.R
\name{PlotHybridZone}
\alias{PlotHybridZone}
\title{PlotHybridZone}
\usage{
PlotHybridZone(sim, center = "start", logit = T, add = F, xlim = NA,
  ylim = NA, ...)
}
\arguments{
\item{sim}{A list (usually output of \code{ReadSummary})}

\item{center}{set to 'start' to center by a start of the simulation (deme 0),
set to NA not to center at all, set to corresponding data.frame with
simulations (usually output of \code{FillSetting}) to center by computed
center in}

\item{logit}{= T if average hybrid should be plot in logit transfomation}

\item{add}{= F, if lines should be added to current plot, or}

\item{xlim}{is equvalent of parameter xlim from plot}

\item{ylim}{NA means c(-10,10) for logit and c(0,1) for non-logit clines}

\item{...}{is passed to function \code{lines} (for instance col for change of colour)}
}
\description{
\code{PlotHybridZone}
}
\author{
Kamil Jaron \email{kamiljaron at gmail.com}
}
