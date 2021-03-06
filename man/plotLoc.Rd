% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotLoc.R
\name{plotLoc}
\alias{plotLoc}
\title{Plots a regional map of US showing location of raster.}
\usage{
plotLoc(ras, crs.proj = NULL, iname = NULL)
}
\arguments{
\item{ras}{A raster object to plot on the map.}

\item{crs.proj}{(optional) Coordinate Reference System of the raster if it is projected. Coordinates assumed to be long, lat if unspecified.}

\item{iname}{(optional) Full name to give the png file (e.g., 'test.png')}
}
\value{
Returns an map image showing the raster location.
}
\description{
Wrapper function that plots a regional map of US, and then plots the input raster on top.
}
\author{
Bjorn J. Brooks
}
