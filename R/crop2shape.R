#' @name crop2shape
#' @title Crops and masks a raster to a shapefile.
#' @description Crop a raster to the perimeter of a shapefile returning only pixels within shapefile extent. Both raster and shapefile must have same geospatial projection.
#' @importFrom raster crop mask extent
#' @param r A raster to be cropped.
#' @param s A shapefile marking the perimeter to crop the raster to.
#' @return Returns a cropped and masked raster.
#' @author Bjorn J. Brooks
#' @export

###
crop2shape <- function(r, s) {
  r2 <- crop(r, extent(s))                         # Clip
  output <- mask(r2, s)                            # Mask

  return(output)
}
