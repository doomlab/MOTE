#' APA
#'
#' A function that formats decimals and leading zeroes 
#'
#' @param value input the number you want to format
#' @param decimals input the number of decimals
#' @param leading input logical TRUE for leading zeroes on decimals and FALSE for no leading zeroes on decimals
#' @keywords APA, decimals, formatting
#' @export
#' @examples
#' Apa(x,k)

apa <- function(value, decimals = 3, leading = T) {
  if (leading == T) {
    format(value, digits = decimals, nsmall = decimals)
    }
  if (leading == F) {
    sub("^(-?)0.", "\\1.", sprintf(paste("%.", decimals, "f", sep = ""), value))
    } 
  }

##get rid of this function after updating all to the list output   
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
