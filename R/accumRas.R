#' @name accumRas
#' @title Accumulate values across a stack of rasters.
#' @description Wrapper that uses raster::calc() to apply a given function across a raster stack, resulting in a single raster representing the accumulated result of the stack.
#' @importFrom raster writeRaster stack calc Which
#' @param ifpath A character string indicating the path to the rasters to process, e.g., "/home/user/data/"
#' @param ptrn A regular expression character string that expands to list the rasters to process, e.g., "^raster200[0-9].tif$".
#' @param ofname A character string indicatin the desired output raster file name.
#' @param ovname A character string designating the variable name to include in the output raster metadata.
#' @param datatype A character string to be passed to writeRaster() and used as its datatype argument.
#' @param format A character string to be passed to writeRaster() and used as its format argument.
#' @param FUN The function to apply to aggregate the data for each cell across the raster stack.
#' @param verbose A boolean argument controlling whether or not to report the names of rasters being processed.
#' @details accumRas returns a single raster that is the aggregate across a stack of rasters.
#' @return Returns a single raster with the same spatial dimensions as the input rasters.
#' @examples
#' library(raster)
#' r1 <- raster(matrix(sample(1:5, size=16, replace=TRUE),
#'                     nrow=4, byrow=TRUE))
#' writeRaster(r1, filename='r1', format='GTiff',
#'             datatype='INT1U', overwrite=TRUE)
#' r2 <- r1 + 10
#' writeRaster(r2, filename='r2', format='GTiff',
#'             datatype='INT1U', overwrite=TRUE)
#' ifpath <- paste0(getwd(), '/')
#' fptrn <- '^r[1,2]\\.tif$'
#' ofname <- 'rCumGain.tif'
#' ovname <- 'ExampleData'
#' accumRas(ifpath=ifpath, ptrn=fptrn,
#'          ofname=ofname, ovname=ovname,
#'          format='GTiff', datatype='INT1U', FUN=cumGain)
#' @author Bjorn J. Brooks
#' @export

###
accumRas <- function(ifpath, ptrn, ofname, ovname, format, datatype, FUN, verbose=TRUE) {

  ofile <- paste0(ifpath, ofname)
  if ( file.exists(ofile)) {
	  print(paste(ofile, 'already exists. Skipping accumulation.'))
  } else {
    ras2pr <- list.files(path=ifpath,                # Raster files to process
			 pattern=ptrn,
			 full.names=TRUE)
    if (verbose == TRUE) {
      print('Accumulating the following rasters:')
      print(ras2pr)
    }

    stck <- stack(ras2pr)                          # Make raster stack
    stckOut <- calc(stck, fun=FUN)                 # Calc acrs stack by pix
    stckOut[Which(stckOut == 0)] <- NA             # If none gained, set to NA
    names(stckOut) <- ovname                       # Update name in metadata
    writeRaster(stckOut, filename=ofile,           # Write output to files
		format=format, overwrite=TRUE)
    if (verbose == TRUE) {
      print(paste('Output raster written to:', ofile))
    }
  }
}
