% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stats.R
\name{theil_sen}
\alias{theil_sen}
\alias{slope.theil_sen}
\alias{intercept.theil_sen}
\title{Theil Sen Slope Estimator}
\usage{
theil_sen(x, y, ...)

\method{slope}{theil_sen}(x, ...)

\method{intercept}{theil_sen}(x, ...)
}
\arguments{
\item{x}{time vector (\code{numeric}, or \code{Date}).}

\item{y}{numeric value.}

\item{\dots}{further arguments passed to or from other methods.}
}
\value{
object of class \code{Theil_Sen}.
}
\description{
Theil Sen Slope Estimator
}
\section{Methods (by generic)}{
\itemize{
\item \code{slope(theil_sen)}: Extract slope.

\item \code{intercept(theil_sen)}: Extract intercept.

}}
\examples{

# create theil_sen object
ts <- theil_sen(1:5, c(1, 2, 3, 5, 9))

# get slope
slope(ts)

# get intercept
intercept(ts)
}
\references{
\url{https://en.wikipedia.org/wiki/Theil-Sen_estimator}
}
