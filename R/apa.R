#' apa
#'
#' A function that formats decimals and leading zeroes 
#'
#' @param value the number you want to format
#' @param decimals the number of decimal points desired
#' @param leading logical TRUE for leading zeroes on decimals and FALSE for no leading zeroes on decimals
#' @keywords APA, decimals, formatting
#' @export
#' @examples
#' Apa(x,k)

apa <- function(value, decimals = 3, leading = T) {
  # Function creates printable results from output
  #
  # Args:
  #   value     : the number you want to format
  #   decimals  : the number of decimal points desired
  #   leading   : T for leading zeroes, F for no leading zeroes
  #
  # Returns:
  #   Character of formatted values
  
  if (leading == T) {
    formnumber <- format(value, digits = decimals, nsmall = decimals)
    }
  if (leading == F) {
    formnumber <- sub("^(-?)0.", "\\1.", sprintf(paste("%.", decimals, "f", sep = ""), value))
    } 
  return(formnumber)
  }
