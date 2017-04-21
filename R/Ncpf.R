#' Ncpf
#'
#' This Function calculates non-central F
#'
#' @param x value
#' @param q value
#' @param df1 degrees of freedom model
#' @param df2 degrees of freedom error
#' @param confirm defaults to FALSE
#' @keywords effect size
#' @export
#' @examples
#' Ncpf(x, q, df1, df2, confirm=FALSE)



Ncpf <- function(x, q, df1, df2, confirm=FALSE) {
  # Function calculates non-central F
  #
  # Args : 
  #   x  : 
  #   q  :
  #   df1: degrees of freedom model
  #   df2: degrees of freedom error
  #
  # Returns:
  #   calculation of non-central F
  
  .f <- function(ncp, x, df1, df2, q)abs(q - pf(x, df1 = df1, df2 = df2, ncp = ncp))
  if (pf(x, df1 = df1, df2 = df2) <= q) {
    if (confirm) {
      minimum <- 0 ;
      objective <- pf(x, df1 = df1, df2 = df2) - q ;
      data.frame(minimum, objective)
    } else {
      0
    }
  } else {
    .n <- 1 ;
    while	(
      (pf(x, df1 = df1, df2 = df2, ncp = .n) > q / 2)
      &
      (.n < Inf)
    )
      .n <- .n + 1 ;
    if (confirm) {
      optimize(f = .f, x = x, df1 = df1, df2 = df2, q = q, interval = c(0,.n))
    } else {
      optimize(f = .f, x = x, df1 = df1, df2 = df2, q = q, interval = c(0,.n))$minimum
    }
  }
}
