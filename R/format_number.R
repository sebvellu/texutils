#' Format numbers with a fixed number of decimal places
#'
#' This function rounds numeric values to a specified number of decimal
#' places and returns them as character strings with trailing zeros retained.
#' Scientific notation is suppressed.
#'
#' @param x A numeric vector, matrix, or array.
#'
#' @param digits A non-negative integer specifying the number of decimal
#'   places to display.
#'
#' @return
#' A character object with the same dimensions as `x`, containing the
#' formatted values.
#'
#' @examples
#' format_number(c(1.23456, 2, 3.1), 2)
#' format_number(matrix(c(1.234, 5.6, 7, 8.901), nrow = 2), 3)
#'
#' @export

format_number <- function(x, digits) {
    return(format(
        round(x, digits),
        nsmall = digits,
        scientific = FALSE
    ))
}
