#' Format small numbers for LaTeX output
#'
#' Formats numeric values to a fixed number of decimal places and represents
#' sufficiently small nonzero values using a compact LaTeX notation in which
#' the number of leading zeros after the decimal point is indicated by a
#' superscript.
#'
#' @details
#' Values whose absolute value is smaller than `threshold` are represented
#' in the form `0.0^{n}d`, where `n` is the number of leading zeros after
#' the decimal point and `d` contains `small_digits` digits following these
#' zeros.
#'
#' For example, `0.00003927` is represented as `0.0^{4}39` when
#' `small_digits = 2`, and as `0.0^{4}3927` when `small_digits = 4`.
#'
#' Values not represented using the compact notation are formatted to
#' `digits` decimal places.
#'
#' @param x A numeric vector, matrix, array, or data frame.
#'
#' @param threshold A positive number specifying the threshold below which
#'   nonzero values are displayed using compact notation.
#'
#' @param digits A non-negative integer specifying the number of decimal
#'   places used for values not displayed using compact notation.
#'
#' @param small_digits A positive integer specifying the number of digits
#'   retained after the leading zeros for values displayed using compact
#'   notation.
#'
#' @param math Logical. If `TRUE`, values displayed using compact notation
#'   are enclosed in `$...$` for use in LaTeX math mode.
#'
#' @return
#' A character object containing the formatted values. For matrices, arrays,
#' and data frames, the dimensions and dimension names of `x` are preserved.
#'
#' @examples
#' format_small_number(
#'     c(0.0123, 0.000412, 0.00003927),
#'     threshold = 0.001,
#'     digits = 4,
#'     small_digits = 2
#' )
#'
#' @export

format_small_number <- function(x, threshold = 0.001, digits = 4,
                                small_digits = digits, math = TRUE) {

    if (is.data.frame(x)) {
        x <- as.matrix(x)
    }

    out <- formatC(x, format = "f", digits = digits)

    idx <- !is.na(x) & x != 0 & abs(x) < threshold

    out[idx] <- vapply(x[idx], function(z) {

        n_zero <- ceiling(-log10(abs(z))) - 1

        rest <- sprintf(
            paste0("%0", small_digits, "d"),
            round(abs(z) * 10^(n_zero + small_digits))
        )

        temp <- paste0(
            if (z < 0) "-" else "",
            "0.0^{", n_zero, "}",
            rest
        )

        if (math) {
            paste0("$", temp, "$")
        } else {
            temp
        }

    }, character(1))

    if (!is.null(dim(x))) {
        dim(out) <- dim(x)
        dimnames(out) <- dimnames(x)
    }

    out
}
