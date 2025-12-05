#' Format numbers for APA-style reporting
#'
#' Create "pretty" character representations of numeric values with a fixed
#' number of decimal places, optionally keeping or omitting the leading zero
#' for values between -1 and 1.
#'
#' @param value Numeric input: a single number, vector, matrix, or a data frame
#'   with all-numeric columns. Non-numeric inputs will error.
#' @param decimals A single non-negative integer giving the number of decimal
#'   places to keep in the output.
#' @param leading Logical: `TRUE` to keep leading zeros on decimals (e.g.,
#'   `0.25`), `FALSE` to drop them (e.g., `.25`). Default is `TRUE`.
#' @return A character vector/array (matching the shape of `value`) containing
#'   the formatted numbers.
#' @details
#' This function formats numbers for inclusion in manuscripts and reports.
#' - When `leading = TRUE`, numbers are rounded and padded to `decimals`
#'   places, keeping the leading zero for values with absolute value < 1.
#' - When `leading = FALSE`, the leading zero before the decimal point is
#'   removed for values with absolute value < 1.
#' If `value` is a data frame, all columns must be numeric; otherwise an error
#' is thrown.
#' @export
#' @examples
#' apa(0.54674, decimals = 3, leading = TRUE)   # "0.547"
#' apa(c(0.2, 1.2345, -0.04), decimals = 2)     # "0.20" "1.23" "-0.04"
#' apa(matrix(c(0.12, -0.9, 2.3, 10.5), 2), decimals = 1, leading = FALSE)
#' # returns a character matrix with ".1", "-.9", "2.3", "10.5"

apa <- function(value, decimals = 3, leading = TRUE) {

  if (missing(value)) {
    stop("Be sure to include the numeric values you wish to format.")
  }

  # Allow data frames with all-numeric columns; coerce to matrix
  if (is.data.frame(value)) {
    if (!all(vapply(value, is.numeric, logical(1)))) {
      stop("All columns in 'value' must be numeric.")
    }
  }

  if (!is.numeric(value) && !is.data.frame(value)) {
    stop("'value' must be numeric (vector, matrix) 
      or a data frame with all-numeric columns.")
  }

  # Validate 'decimals'
  if (!is.numeric(decimals) || length(decimals) != 1 ||
        decimals < 0 || decimals != as.integer(decimals)) {
    stop("'decimals' must be a single non-negative integer.")
  }
  decimals <- as.integer(decimals)

  # Validate 'leading'
  if (!is.logical(leading) || length(leading) != 1) {
    stop("'leading' must be TRUE or FALSE.")
  }

  # Base formatted string with fixed decimal places
  base_fmt <- format(round(as.matrix(as.data.frame(value)), decimals),
                     nsmall = decimals, trim = FALSE, scientific = FALSE)

  if (isTRUE(leading)) {
    formnumber <- base_fmt
  } else {
    # Remove leading zero for values with absolute value < 1
    formnumber <- sub("^(-?)0\\.", "\\1.", base_fmt)
  }
  return(formnumber)
}
