#' Format small numbers for LaTeX output
#'
#' This function formats sufficiently small nonzero numbers using a compact
#' LaTeX notation in which the number of leading zeros after the decimal
#' point is indicated by a superscript.
#'
#' @param x A numeric vector, matrix, or array.
#'
#' @param threshold A positive number specifying the threshold below which
#'   values are displayed using compact notation.
#'
#' @param digits A non-negative integer specifying the number of significant
#'   digits retained in the nonzero part.
#'
#' @return
#' A character object with the same dimensions as `x`, containing the
#' formatted values.
#'
#' @examples
#' format_small_number(c(0.0123, 0.000412, 0.00004),
#'                     threshold = 0.001)
#'
#' @export

format_small_number <- function(x, threshold = 0.001, digits = 4) {

    if (is.data.frame(x)) {
        x <- as.matrix(x)
    }

    out <- formatC(x, format = "f", digits = digits)

    idx <- !is.na(x) & x != 0 & abs(x) < threshold

    out[idx] <- vapply(x[idx], function(z) {

        n_zero <- floor(-log10(abs(z))) - 1
        rest <- signif(abs(z) * 10^(n_zero + 1), digits)

        paste0(
            "$",
            if (z < 0) "-" else "",
            "0.0^{", n_zero, "}",
            format(rest, scientific = FALSE, trim = TRUE),
            "$"
        )
    }, character(1))

    if (!is.null(dim(x))) {
        dim(out) <- dim(x)
        dimnames(out) <- dimnames(x)
    }

    out
}
