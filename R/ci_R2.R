#' Confidence interval for R^2 (exported helper)
#'
#' Compute a confidence interval for the coefficient of determination (R^2).
#' This implementation follows MBESS (Ken Kelley) and is exported here to avoid
#' importing many dependencies. It supports cases with random or fixed
#' predictors and can be parameterized via either degrees of freedom or
#' sample size (n) and number of predictors (p/k).
#'
#' @param r2 Numeric. The observed R^2 (may be `NULL` if `f_value` is supplied).
#' @param df1 Integer. Numerator degrees of freedom from F.
#' @param df2 Integer. Denominator degrees of freedom from F.
#' @param conf_level Numeric in (0, 1). Two-sided confidence
#' level for a symmetric confidence interval. Default is `0.95`.
#' Cannot be used with `alpha_lower` or `alpha_upper`.
#' @param random_predictors Logical. If `TRUE` (default), compute limits for
#'   random predictors; if `FALSE`, compute limits for fixed predictors.
#' @param random_regressors Logical. Backwards-compatible alias for
#'   `random_predictors`. If supplied, it overrides `random_predictors`.
#' @param f_value Numeric. The observed F statistic from the study.
#' @param n Integer. Sample size.
#' @param p Integer. Number of predictors.
#' @param k Integer. Alias for `p` (number of predictors).
#' If supplied along with `p`, they must be equal.
#' @param alpha_lower Numeric. Lower-tail noncoverage probability
#' (cannot be used with `conf_level`).
#' @param alpha_upper Numeric. Upper-tail noncoverage probability
#' (cannot be used with `conf_level`).
#' @param tol Numeric. Tolerance for the iterative method determining critical
#' values. Default is `1e-9`.
#'
#' @return
#' A named list with the following elements:
#' \itemize{
#'   \item{\code{lower_conf_limit_r2}}{The lower confidence limit for R^2.}
#'   \item{\code{prob_less_lower}}{Probability associated with values
#' less than the lower limit.}
#'   \item{\code{upper_conf_limit_r2}}{The upper confidence limit for R^2.}
#'   \item{\code{prob_greater_upper}}{Probability associated with values
#' greater than the upper limit.}
#' }
#'
#' @details
#' If `n` and `p` (or `k`) are provided, `df1` and `df2` are derived as
#' `df1 = p` and `df2 = n - p - 1`. Conversely, if `df1` and `df2` are
#' provided, `n = df1 + df2 + 1` and `p = df1`.
#'
#' @references
#' Kelley, K. (2007). Methods for the behavioral, educational, and social
#' sciences: An R package (MBESS).
#'
#' @importFrom stats pf
#' @export

ci_r2 <- function(r2 = NULL,
                  df1 = NULL,
                  df2 = NULL,
                  conf_level = 0.95,
                  random_predictors = TRUE,
                  random_regressors = random_predictors,
                  f_value = NULL,
                  n = NULL,
                  p = NULL,
                  k = NULL,
                  alpha_lower = NULL,
                  alpha_upper = NULL,
                  tol = 1e-9) {

  # Prefer explicit K/p handling (K is alias for p)
  if (!missing(k)) {
    if (!is.null(p) && p != k) {
      stop("Specify 'p' or 'k', but not both (your 'p' and 'k' are different)")
    }
    p <- k
  }

  # Back-compat alias between random_regressors and random_predictors
  if (!missing(random_regressors)) random_predictors <- random_regressors

  # Derive df or n/p, but not both combinations
  if (((!is.null(n) || !is.null(p)) && (!is.null(df1) || !is.null(df2)))) {
    stop("Either specify 'df1' and 'df2' or 'n' and 'p',
    but not both combinations.")
  }

  if (!is.null(n) && !is.null(p) && is.null(df1) && is.null(df2)) {
    df1 <- p
    df2 <- n - p - 1
  }

  if (!is.null(df1) && !is.null(df2) && is.null(n) && is.null(p)) {
    n <- df1 + df2 + 1
    p <- df1
  }

  # Confidence level / alpha handling
  if (!is.null(conf_level) && is.null(alpha_lower) && is.null(alpha_upper)) {
    if (conf_level <= 0 || conf_level >= 1) {
      stop("'conf_level' must be between 0 and 1.")
    }
    if (!is.null(alpha_lower) || !is.null(alpha_upper)) {
      stop("Since conf_level has been specified (the default),
      you cannot specify 'alpha_lower' and 'alpha_upper'. To specify
      alpha directly, set 'conf_level = NULL'.")
    }
    alpha_lower <- alpha_upper <- (1 - conf_level) / 2
    conf_level <- NULL
  }

  # If needed, derive F from r2 and vice versa
  if (is.null(f_value)) {
    f_value <- r_square_f(r2 = r2, df1 = df1, df2 = df2, p = p, n = n)
  }
  if (is.null(r2)) {
    r2 <- f_r_square(f_value = f_value, df1 = df1, df2 = df2)
  }

  if (isFALSE(random_predictors)) {
    limits <- noncentral_f(f_value = f_value, df1 = df1, df2 = df2,
                           conf_level = NULL, tol = tol,
                           alpha_lower = alpha_lower, alpha_upper = alpha_upper)

    if (length(limits) == 4) {
      lower_limit_r2    <- lambda_to_r2(lambda = limits$lower_limit,
                                        n = n)
      prob_less_lower   <- limits$prob_less_lower
      upper_limit_r2    <- lambda_to_r2(lambda = limits$upper_limit,
                                        n = n)
      prob_greater_upper <- limits$prob_greater_upper

      if (is.na(limits$lower_limit)) {
        lower_limit_r2  <- 0
        prob_less_lower <- 0
      }

      if (is.na(limits$upper_limit)) {
        upper_limit_r2     <- 1
        prob_greater_upper <- 0
      }

      return(list(lower_conf_limit_r2 = lower_limit_r2,
                  prob_less_lower     = prob_less_lower,
                  upper_conf_limit_r2 = upper_limit_r2,
                  prob_greater_upper  = prob_greater_upper))
    }

    if (length(limits) == 2 && is.null(limits$upper_limit)) {
      return(list(lower_conf_limit_r2 =
                    lambda_to_r2(lambda = limits$lower_limit, n = n),
                  prob_less_lower = limits$prob_less_lower,
                  upper_conf_limit_r2 = 1, prob_greater_upper = 0))
    }

    if (length(limits) == 2 && is.null(limits$lower_limit)) {
      return(list(lower_conf_limit_r2 = 0, prob_less_lower = 0,
                  upper_conf_limit_r2 =
                    lambda_to_r2(lambda = limits$upper_limit, n = n),
                  prob_greater_upper = limits$prob_greater_upper))
    }
  }

  # Random predictors branch
  if (isTRUE(random_predictors)) {
    pul <- alpha_upper
    pll <- 1 - alpha_lower

    df1_star <- n - 1
    df2_star <- n - p - 1

    r2_tilde <- r2 / (1 - r2)

    x2 <- 1 - 1e-6
    x1 <- 1e-6
    x3 <- 0.5
    diff3 <- 1

    ulrhosq <- 0.5
    llrhosq <- 0.5

    if (pul != 0) {
      while ((abs(diff3) > 1e-5) && (round(ulrhosq, 5) < 1)) {
        x3 <- (x1 + x2) / 2
        yy <- x3 / (1 - x3)
        gamma_val <- sqrt(1 + yy)
        phi1 <- df1_star * (gamma_val^2 - 1) + p
        phi2 <- df1_star * (gamma_val^4 - 1) + p
        phi3 <- df1_star * (gamma_val^6 - 1) + p
        g <- (phi2 - sqrt(phi2^2 - phi1 * phi3)) / phi1
        nu <- (phi2 - 2 * yy * gamma_val * (sqrt(df1_star * df2_star))) / (g^2)
        lambda_u <- yy * gamma_val * (sqrt(df1_star * df2_star)) / (g^2)
        limit <- df2_star * r2_tilde / (nu * g)
        diff3 <- pf(limit, nu, df2_star, ncp = lambda_u) - pul
        yy <- x1 / (1 - x1)
        gamma_val <- sqrt(1 + yy)
        phi1 <- df1_star * (gamma_val^2 - 1) + p
        phi2 <- df1_star * (gamma_val^4 - 1) + p
        phi3 <- df1_star * (gamma_val^6 - 1) + p
        g <- (phi2 - sqrt(phi2^2 - phi1 * phi3)) / phi1
        nu <- (phi2 - 2 * yy * gamma_val * (sqrt(df1_star * df2_star))) / (g^2)
        lambda_u <- yy * gamma_val * (sqrt(df1_star * df2_star)) / (g^2)
        limit <- df2_star * r2_tilde / (nu * g)
        diff1 <- pf(limit, nu, df2_star, ncp = lambda_u) - pul
        if ((diff1 * diff3) < 0) x2 <- x3 else x1 <- x3
        ulrhosq <- x3
      }
    }

    # Lower limit
    x2 <- 1 - 1e-6
    x1 <- 1e-6
    x3 <- 0.5

    if (pll != 1) {
      yy <- x3 / (1 - x3)
      gamma_val <- sqrt(1 + yy)
      phi1 <- df1_star * (gamma_val^2 - 1) + p
      phi2 <- df1_star * (gamma_val^4 - 1) + p
      phi3 <- df1_star * (gamma_val^6 - 1) + p
      g <- (phi2 - sqrt(phi2^2 - phi1 * phi3)) / phi1
      nu <- (phi2 - 2 * yy * gamma_val * (sqrt(df1_star * df2_star))) / (g^2)
      lambda_u <- yy * gamma_val * (sqrt(df1_star * df2_star)) / (g^2)
      limit <- df2_star * r2_tilde / (nu * g)
      diff3 <- pf(limit, nu, df2_star, ncp = lambda_u) - pll

      while ((abs(diff3) > 1e-5) && (round(llrhosq, 5) < 1)) {
        x3 <- (x1 + x2) / 2
        yy <- x3 / (1 - x3)
        gamma_val <- sqrt(1 + yy)
        phi1 <- df1_star * (gamma_val^2 - 1) + p
        phi2 <- df1_star * (gamma_val^4 - 1) + p
        phi3 <- df1_star * (gamma_val^6 - 1) + p
        g <- (phi2 - sqrt(phi2^2 - phi1 * phi3)) / phi1
        nu <- (phi2 - 2 * yy * gamma_val * (sqrt(df1_star * df2_star))) / (g^2)
        lambda_u <- yy * gamma_val * (sqrt(df1_star * df2_star)) / (g^2)
        limit <- df2_star * r2_tilde / (nu * g)
        diff3 <- pf(limit, nu, df2_star, ncp = lambda_u) - pll
        yy <- x1 / (1 - x1)
        gamma_val <- sqrt(1 + yy)
        phi1 <- df1_star * (gamma_val^2 - 1) + p
        phi2 <- df1_star * (gamma_val^4 - 1) + p
        phi3 <- df1_star * (gamma_val^6 - 1) + p
        g <- (phi2 - sqrt(phi2^2 - phi1 * phi3)) / phi1
        nu <- (phi2 - 2 * yy * gamma_val * (sqrt(df1_star * df2_star))) / (g^2)
        lambda_u <- yy * gamma_val * (sqrt(df1_star * df2_star)) / (g^2)
        limit <- df2_star * r2_tilde / (nu * g)
        diff1 <- pf(limit, nu, df2_star, ncp = lambda_u) - pll
        if ((diff1 * diff3) < 0) x2 <- x3 else x1 <- x3
        llrhosq <- x3
      }
    }

    if (round(llrhosq, 5) == 1 && llrhosq > r2) llrhosq <- 0

    if (pll == 1) llrhosq <- 0
    if (pul == 0) ulrhosq <- 1
    if (llrhosq > ulrhosq) warning("There is a problem; the lower
    limit is greater than the upper limit (Are you at one of the
    boundaries of Rho^2 or is alpha very large?).")

    if (pll == 1) return(list(lower_conf_limit_r2 = 0, prob_less_lower = 0,
                              upper_conf_limit_r2 = ulrhosq,
                              prob_greater_upper = pul))
    if (pul == 0) return(list(lower_conf_limit_r2 = llrhosq,
                              prob_less_lower = 1 - pll,
                              upper_conf_limit_r2 = 1, prob_greater_upper = 0))
    return(list(lower_conf_limit_r2 = llrhosq, prob_less_lower = 1 - pll,
                upper_conf_limit_r2 = ulrhosq, prob_greater_upper = pul))
  }
}
