% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/litteR.R
\docType{package}
\name{litteR-package}
\alias{litteR}
\alias{litteR-package}
\title{Litter Analysis}
\description{
A tool for the analysis of various litter types, e.g.,
beach litter, riverain litter, floating litter, and seafloor litter.
}
\details{
\if{html}{\figure{litter.png}{options: width='900'}}
\if{latex}{\figure{litter.png}{options: width=100mm}}
\cr
\cr

The easiest way to get convenient with \pkg{litteR} is to create an empty
project directory and fill it with example files by calling the function
\code{\link{create_litter_project}}. The workhorse function in \pkg{litteR}
is called \code{\link{litter}}. This function will start a simple user
interface and lets you select an input file (*.csv) and a settings file
(*.yaml). It will produce an HTML-report with litter analysis results
according to the selected options in the settings file.
See the package vignette for more details.
}
\references{
Schulz, Marcus, Dennis J.J. Walvoort, Jon Barry,
David M. Fleet & Willem M.G.M. van Loon, 2019.
Baseline and power analyses for the assessment of beach litter reductions
in the European OSPAR region. Environmental Pollution 248:555-564
<doi:10.1016/j.envpol.2019.02.030>
}
\author{
\strong{Maintainer}: Dennis Walvoort \email{dennis.Walvoort@wur.nl} [copyright holder]

Authors:
\itemize{
  \item Willem van Loon \email{willem.van.loon@rws.nl} [copyright holder]
}

Other contributors:
\itemize{
  \item Rijkswaterstaat - The Netherlands [copyright holder, funder, data contributor]
}

}
