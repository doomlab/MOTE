#' Apa
#'
#' This function formats for APA style
#'
#' @param x input one
#' @param k input two
#' @keywords APA
#' @export
#' @examples
#' Apa(x,k)


####format for Apa style####
Apa <- function(x, k) format(round(x, k), nsmall = k)
p.value <- function(p, k) {
  if (k <= 2) { 
    if (p < .01) {
      pout <- "p < .01"
    } else { 
      pout = paste("p = ", Apa(p, k))
    }
  }
  if (k > 2) { 
    if (p < .001) {
      pout <- "p < .001"
    } else { 
      pout <- paste("p = ", Apa(p, k))
    }
  }
  return(pout)
}
