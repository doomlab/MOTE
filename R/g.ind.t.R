#' g.ind.t
#'
#' This function Displays Hedge's g and confidence interval for independent t from means.
#'
#' @param m1 mean from first group
#' @param m2 mean from second group
#' @param sd1 standard deviation from first group
#' @param sd2 standard deviation from second group
#' @param n1 sample size from first group
#' @param n2 sample size from second group
#' @param a significance level
#' @param k significant digits to use for formatting
#' @keywords effect size
#' @export
#' @examples
#' g.ind.t(m1=20, m2=17, sd1=4, sd2=5, n1=100, n2=100, a = .05, k = 2)


g.ind.t <- function (m1, m2, sd1, sd2, n1, n2, a = .05, k = 2) {
  # Displays Hedge's g and confidence interval for
  # independent t from means.
  #
  # Args: 
  #   m1 : mean from first group
  #   m2 : mean from second group
  #   sd1: standard deviation from first group
  #   sd2: standard deviation from second group
  #   n1 : sample size from first group
  #   n2 : sample size from second group
  #   a  : significance level
  #   k  : significant digits to use for formatting
  #
  # Returns:
  #   String describing test statistic and 
  #   Hedge's g with confidence intervals.
  
  correction <- 1 - (3 / (4 * (n1 + n2) - 9))
  spooled <- sqrt( ((n1 - 1) * sd1 ^ 2 + (n2 - 1) * sd2 ^ 2) / (n1 + n2 - 2))
  d <- ((m1 - m2) / spooled)*correction
  se1 <- sd1 / sqrt(n1)
  se2 <- sd2 / sqrt(n2)
  sepooled <- sqrt((spooled ^ 2 / n1 + spooled ^ 2 / n2))
  t <- (m1 - m2) / sepooled
  ahigh <- a / 2
  alow <- 1 - (a / 2)
  ncplow <- Ncpt(x = t, q = alow, df = (n1 + n2 - 2))
  ncphigh <- Ncpt(x = t, q = ahigh, df = (n1 + n2 - 2))
  dlow <- correction * (ncplow / sqrt(((n1 * n2) / (n1 + n2))))
  dhigh <- correction * (ncphigh / sqrt(((n1 * n2) / (n1 + n2))))
  M1low <- m1 - se1 * qt(a / 2, n1 - 1, lower.tail = FALSE)
  M1high <- m1 + se1 * qt(a / 2, n1 - 1, lower.tail = FALSE)
  M2low <- m2 - se2 * qt(a / 2, n2 - 1, lower.tail = FALSE)
  M2high <- m2 + se2 * qt(a / 2, n2 - 1, lower.tail = FALSE)
  p <- pt(abs(t), (n1 - 1 + n2 - 1), lower.tail = F) * 2
  # Print the result
  cat("M1 = ", 
      Apa(m1, k),
      ", SD = ", 
      Apa(sd1, k),
      ", SE = ", Apa(se1, k),
      ", ", (1 - a) * 100, "%CI[", 
      Apa(M1low, k),
      " - ",
      Apa(M1high, k),
      "]",
      "\nM2 = ", 
      Apa(m2, k),
      ", SD = ", 
      Apa(sd2, k),
      ", SE = ", Apa(se2, k),
      ", ", (1 - a) * 100, "%CI[", 
      Apa(M2low, k),
      " - ",
      Apa(M2high, k),
      "]",
      "\nt(", (n1 - 1 + n2 - 1), ") = ", 
      Apa(t, k),
      ", ", 
      p.value(p, k), 
      ", d = ",
      Apa(d, k), 
      ", ", (1 - a) * 100, "%CI[", 
      Apa(dlow, k), 
      " - ", 
      Apa(dhigh, k), "]", sep = "")
}
