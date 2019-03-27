#' @name geomSeries
#' @title Produces a geometric series for a specified base.
#' @description Produces a geometric series from 1 to a specified limit, at intervals determined by the specified base.
#' @param base A numeric values indicating the log-base to use in creating the geometric series
#' @param max A numeric values indicating the largest value in the output series, i.e., {1, ..., max}.
#' @return Returns a geometric series of values.
#' @examples
#' geomSeries(base=2, max=400)
#' @author Bjorn J. Brooks
#' @export

###
# Function for creating a geometric series
geomSeries <- function(max, base) {
  base^(0:floor(log(max, base=base)))
}
