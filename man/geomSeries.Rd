% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geomSeries.R
\name{geomSeries}
\alias{geomSeries}
\title{Produces a geometric series for a specified base.}
\usage{
geomSeries(max, base)
}
\arguments{
\item{max}{A numeric values indicating the largest value in the output series, i.e., {1, ..., max}.}

\item{base}{A numeric values indicating the log-base to use in creating the geometric series}
}
\value{
Returns a geometric series of values.
}
\description{
Produces a geometric series from 1 to a specified limit, at intervals determined by the specified base.
}
\examples{
geomSeries(base=2, max=400)
}
\author{
Bjorn J. Brooks
}
