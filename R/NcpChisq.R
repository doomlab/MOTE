#' NcpChisq
#'
#' This Function calculates non-central chi-square
#'
#' @param x chisquare value
#' @param q input
#' @param df degrees of freedom
#' @param confirm defaults to FALSE
#' @keywords effect size
#' @export
#' @examples
#' NcpChisq(x, q, df, confirm=FALSE)



NcpChisq <- function(x, q, df, confirm=FALSE) {
  # Function calculates non-central chi-square
  #
  # Args: 
  #   x : chisquare value
  #   q : 
  #   df: degrees of freedom
  #
  # Returns:
  #   calculation of non-central chi-square
  
  .f <- function(ncp, x, df, q)abs(q - pchisq(x, df=df, ncp=ncp))
  if (pchisq(x, df = df) <= q) {
    if (confirm) {
      minimum <- 0 ;
      objective <- pchisq(x, df = df) - q ;
      data.frame(minimum, objective)
    } else {
      0
    }
  } else {
    .n <- 1 ;
    while	(
      (pchisq(x, df = df, ncp = .n) > q / 2)
      &
      (.n < Inf)
    )
      .n <- .n + 1 ;
    if (confirm) {
      optimize(f = .f, x = x, df = df, q = q, interval = c(0,.n))
    } else {
      optimize(f = .f, x = x, df = df, q = q, interval = c(0,.n))$minimum
    }
  }
}
