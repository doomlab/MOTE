#' Ncpt
#'
#' Calculates non-central t
#'
#' @param x sample mean
#' @param q population mean
#' @param df degrees of freedom
#' @param confirm defaults to FALSE
#' @keywords effect size
#' @export
#' @examples
#' Ncpt(x, q, df, confirm=FALSE)



Ncpt <- function(x, q, df, confirm=FALSE) {
  # Function calculates non-central t
  #
  # Args: 
  #   x : sample mean
  #   q : population mean
  #   df: degrees of freedom
  #
  # Returns:
  #   calculation of non-central t
  
  .f <- function(ncp, x, df, q)abs(q - pt(x, df = df, ncp = ncp))
  .n <- 1;
  while (
    (
      (pt(x, df = df, ncp = -.n) < q + (1 - q) / 2 )
      |
      (pt(x, df = df, ncp = .n) > q / 2)
    )
    &
    (.n < Inf)
  )
    .n <- .n*2 ;
  if (confirm)
    optimize(f = .f, x = x, df = df, q = q, interval = c(-.n,.n))
  else {
    optimize(f = .f, x = x, df = df, q = q, interval = c(-.n,.n))$minimum
  }
}
