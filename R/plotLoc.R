#' @name plotLoc
#' @title Plots a regional map of US showing location of raster.
#' @description Wrapper function that plots a regional map of US, and then plots the input raster on top.
#' @importFrom raster plot
#' @importFrom sp bbox spTransform
#' @importFrom graphics box plot
#' @importFrom grDevices dev.off png
#' @importFrom utils data
#' @param ras A raster object to plot on the map.
#' @param crs.proj (optional) Coordinate Reference System of the raster if it is projected. Coordinates assumed to be long, lat if unspecified.
#' @param iname (optional) Full name to give the png file (e.g., 'test.png')
#' @return Returns an map image showing the raster location.
#' @author Bjorn J. Brooks
#' @export

###
plotLoc <- function(ras, crs.proj=NULL, iname=NULL) {
  if (! exists('us.bbox')) {
    us <- raster::getData("GADM",country="USA",    # Load state boundaries
			  level=2)
    us <- us[! us$NAME_1 %in%                      # Filter out AK, HI
		  c('Alaska', 'Hawaii'),]
    us <- spTransform(us, crs.proj)                # Reproject
    us.bbox <- bbox(us)                            # Get extent
    ras.bbox <- bbox(ras)                          # Get extent
    us.bbox <- (us.bbox+ras.bbox)/2                # Shrink US bbox
  }
  xlim <- c(min(us.bbox[1,1], ras.bbox[1,1]),
	    max(us.bbox[1,2], ras.bbox[1,2]))
  ylim <- c(min(us.bbox[2,1], ras.bbox[2,1]),
	    max(us.bbox[2,2], ras.bbox[2,2]))
  if (! is.null(iname)) {
    png(filename=iname, width=400, height=400, units='px')
  }
  plot(us, xlim=xlim, ylim=ylim)
  plot(ras, col='red', legend=FALSE, add=TRUE)
  box()
  if (! is.null(iname)) {
    dev.off()
  }
}
