#' Eta.anova
#'
#' This function Displays eta, r squared, ICCs from ANOVA analyses
#'
#' @param dfm degrees of freedom model
#' @param dfe degrees of freedom error
#' @param f F statistic
#' @param a significance level
#' @param k significant digits to use for formatting
#' @keywords effect size
#' @export
#' @examples
#' Eta.anova(dfm = 2, dfe = 20, f = 5.7, a = .05, k = 2)


Eta.anova <- function (dfm, dfe, f, a = .05, k = 2) {
  # Displays eta, r squared, ICCs from
  # ANOVA analyses
  #
  # Args: 
  #   dfm: degrees of freedom model
  #   dfe: degrees of freedom error
  #   f  : F statistic
  #   a  : significance level
  #   k  : significant digits to use for formatting
  #
  # Returns:
  #   String describing test statistic and 
  #   effect size with confidence intervals.
  
  eta <- (dfm * f) / (dfm * f + dfe)
  ahigh <- a / 2
  alow <- 1 - (a / 2)
  ncplow <- Ncpf(x = f, q = alow, df1 = dfm, df2 = dfe)
  ncphigh <- Ncpf(x = f, q = ahigh, df1 = dfm, df2 = dfe)
  elow <- ncplow / (ncplow + dfm + dfe + 1)
  ehigh <- ncphigh / (ncphigh + dfm + dfe + 1)
  p <- pf(f, dfm, dfe, lower.tail = F)
  # Print the result
  cat("F(", dfm, ", ", dfe, ") = ", 
      Apa(f, k),
      ", ", 
      p.value(p, k), 
      ", n2 = ", 
      Apa(eta, k), 
      ", ", (1 - a) * 100, "%CI[", 
      Apa(elow, k), 
      " - ", 
      Apa(ehigh, k), "] \nNote: use np2 when you have multiple IVs.", sep = "")
}
