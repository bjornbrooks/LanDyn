#' @name LaTeXtabular
#' @title Translates R data to LaTeX tabular format
#' @description Convert R data or data frame to LaTeX tabular format, suitable for inclusion in an R document.
#' @param df R data frame or tabular data.
#' @return Returns the input data with LaTeX code for column breaks and line feeds inserted.
#' @examples
#' # Convert R data output to LaTeX tablular format
#' cat(LaTeXtabular(mtcars[1:5, 1:5]))
#' @author Bjorn J. Brooks
#' @export

###
LaTeXtabular <- function(df) {
  stopifnot(is.data.frame(df))

  align <- function(x) if (is.numeric(x)) "r" else "l"
  col_align <- vapply(df, align, character(1))

  cols <- lapply(df, format)
  contents <- do.call("paste",
                      c(cols, list(sep = " \\tab ", collapse = "\\cr\n  ")))

  paste("\\tabular{", paste(col_align, collapse = ""), "}{\n  ",
        contents, "\n}\n", sep = "")
}
