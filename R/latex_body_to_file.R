#' Write a data frame to a file in simple LaTeX table row format
#'
#' This function writes the contents of a data frame to a text file where
#' each row is formatted using ampersand separators and a trailing double
#' backslash, suitable for inclusion inside a LaTeX tabular environment.
#' No LaTeX commands are added other than the row terminator string.
#'
#' @details
#' The function converts the input to a data frame, optionally prepends
#' row names, and writes each row to the specified file. Values in each
#' row are separated by "&" and each row ends with "\\".
#'
#' @param datf A data frame or object coercible to a data frame.
#' 
#' @param file A file path where the output will be written.
#' 
#' @param coln Logical. If `TRUE`, column names are written as the first row.
#' 
#' @param rown Logical. If `TRUE`, row names are added as the first column.
#'
#' @return
#' Invisibly returns `NULL`. The primary result is the file written to disk.
#'
#' @examples
#' temp <- tempfile()
#' latex_body_to_file(head(mtcars), temp, coln = TRUE, rown = TRUE)
#'
#' @export

latex_body_to_file <- function(datf, file, coln = FALSE, rown = FALSE) {
  datf <- as.data.frame(datf)
  fcon <- file(file, open = "w")
  # If rownames = TRUE, prepend rownames as a column
  if (rown) {
    datf <- cbind(rownames(datf), datf)
    names(datf)[1] <- ""
  }
  # Optional column names
  if (coln) {
    writeLines(paste(paste(names(datf), collapse = " & "), "\\\\"), fcon)
  }
  # Write rows
  apply(datf, 1, function(rval) {
    writeLines(paste(paste(rval, collapse = " & "), "\\\\"), fcon)
  })
  close(fcon)
}
