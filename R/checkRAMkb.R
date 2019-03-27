#' @name checkRAMkb
#' @title Queries available RAM.
#' @description Queries the operating system and returns the number of kilobytes of memory available.
#' @return Returns a single value.
#' @examples
#' checkRAMkb()
#' @author Bjorn J. Brooks
#' @export

###
checkRAMkb <- function() {
  if (Sys.info()[['sysname']] == 'Linux') {
    output <- as.numeric(system(                   # Detect available RAM
                         "awk '/MemAvailable/ {print $2}' /proc/meminfo",
                         intern=TRUE))
  } else if (Sys.info()[['sysname']] == 'Windows') {
    x <- system2("wmic", args =  "OS get FreePhysicalMemory /Value",
                 stdout = TRUE)                    # Detect available RAM
    x <- x[grepl("FreePhysicalMemory", x)]
    x <- gsub("FreePhysicalMemory=", "", x, fixed = TRUE)
    x <- gsub("\r", "", x, fixed = TRUE)
    output <- as.numeric(x)                        # Convert to numeric
  }
  return(output)
}

