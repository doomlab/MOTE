#' Confidence interval for R^2 (exported helper)
#'
#' Compute a confidence interval for the coefficient of determination (R^2).
#' This implementation follows MBESS (Ken Kelley) and is exported here to avoid
#' importing many dependencies. It supports cases with random or fixed
#' predictors and can be parameterized via either degrees of freedom or
#' sample size (N) and number of predictors (p/K).
#'
#' @param R2 Numeric. The observed R^2 (may be `NULL` if `F.value` is supplied).
#' @param df.1 Integer. Numerator degrees of freedom from F.
#' @param df.2 Integer. Denominator degrees of freedom from F.
#' @param conf.level Numeric in (0, 1). Two-sided confidence level for a symmetric
#'   confidence interval. Default is `0.95`. Cannot be used with `alpha.lower` or
#'   `alpha.upper`.
#' @param Random.Predictors Logical. If `TRUE` (default), compute limits for
#'   random predictors; if `FALSE`, compute limits for fixed predictors.
#' @param Random.Regressors Logical. Backwards-compatible alias for
#'   `Random.Predictors`. If supplied, it overrides `Random.Predictors`.
#' @param F.value Numeric. The observed F statistic from the study.
#' @param N Integer. Sample size.
#' @param p Integer. Number of predictors.
#' @param K Integer. Alias for `p` (number of predictors). If supplied along with
#'   `p`, they must be equal.
#' @param alpha.lower Numeric. Lower-tail noncoverage probability (cannot be used
#'   with `conf.level`).
#' @param alpha.upper Numeric. Upper-tail noncoverage probability (cannot be used
#'   with `conf.level`).
#' @param tol Numeric. Tolerance for the iterative method determining critical
#'   values. Default is `1e-9`.
#'
#' @return A list with elements:
#'   * `Lower.Conf.Limit.R2`
#'   * `Prob.Less.Lower`
#'   * `Upper.Conf.Limit.R2`
#'   * `Prob.Greater.Upper`
#'
#' @details
#' If `N` and `p` (or `K`) are provided, `df.1` and `df.2` are derived as
#' `df.1 = p` and `df.2 = N - p - 1`. Conversely, if `df.1` and `df.2` are
#' provided, `N = df.1 + df.2 + 1` and `p = df.1`.
#'
#' @seealso [Rsquare2F()], [F2Rsquare()], [noncentral_f()], [Lambda2Rsquare()].
#'
#' @references
#' Kelley, K. (2007). Methods for the behavioral, educational, and social
#' sciences: An R package (MBESS).
#'
#' @importFrom stats pf
#' @export
ci.R2 <- function(R2 = NULL, df.1 = NULL, df.2 = NULL,
                  conf.level = 0.95, Random.Predictors = TRUE,
                  Random.Regressors, F.value = NULL, N = NULL,
                  p = NULL, K = NULL, alpha.lower = NULL, alpha.upper = NULL,
                  tol = 1e-9) {

  # Prefer explicit K/p handling (K is alias for p)
  if (!missing(K)) {
    if (!is.null(p) && p != K) {
      stop("Specify 'p' or 'K', but not both (your 'p' and 'K' are different)")
    }
    p <- K
  }

  # Back-compat alias
  if (!missing(Random.Regressors)) Random.Predictors <- Random.Regressors

  # Derive df or N/p, but not both combinations
  if (((!is.null(N) || !is.null(p)) && (!is.null(df.1) || !is.null(df.2)))) {
    stop("Either specify 'df.1' and 'df.2' or 'N' and 'p', but not both combinations.")
  }

  if (!is.null(N) && !is.null(p) && is.null(df.1) && is.null(df.2)) {
    df.1 <- p
    df.2 <- N - p - 1
  }

  if (!is.null(df.1) && !is.null(df.2) && is.null(N) && is.null(p)) {
    N <- df.1 + df.2 + 1
    p <- df.1
  }

  # Confidence level / alpha handling
  if (!is.null(conf.level) && is.null(alpha.lower) && is.null(alpha.upper)) {
    if (conf.level <= 0 || conf.level >= 1) {
      stop("'conf.level' must be between 0 and 1.")
    }
    if (!is.null(alpha.lower) || !is.null(alpha.upper)) {
      stop("Since conf.level has been specified (the default), you cannot specify 'alpha.lower' and 'alpha.upper'. To specify alpha directly, set 'conf.level = NULL'.")
    }
    alpha.lower <- alpha.upper <- (1 - conf.level) / 2
    conf.level <- NULL
  }

  # If needed, derive F from R2 and vice versa
  if (is.null(F.value)) {
    F.value <- Rsquare2F(R2 = R2, df.1 = df.1, df.2 = df.2, p = p, N = N)
  }
  if (is.null(R2)) {
    R2 <- F2Rsquare(F.value = F.value, df.1 = df.1, df.2 = df.2)
  }

  if (isFALSE(Random.Predictors)) {
    Limits <- noncentral_f(F.value = F.value, df.1 = df.1, df.2 = df.2,
                           conf.level = NULL, tol = tol,
                           alpha.lower = alpha.lower, alpha.upper = alpha.upper)

    if (length(Limits) == 4) {
      LL <- Lambda2Rsquare(Limits$Lower.Limit, N = N)
      Prob.LL <- Limits$Prob.Less.Lower
      UL <- Lambda2Rsquare(Limits$Upper.Limit, N = N)
      Prob.UL <- Limits$Prob.Greater.Upper

      if (is.na(Limits$Lower.Limit)) {
        LL <- 0
        Prob.LL <- 0
      }

      if (is.na(Limits$Upper.Limit)) {
        UL <- 1
        Prob.UL <- 0
      }

      return(list(Lower.Conf.Limit.R2 = LL, Prob.Less.Lower = Prob.LL,
                  Upper.Conf.Limit.R2 = UL, Prob.Greater.Upper = Prob.UL))
    }

    if (length(Limits) == 2 && is.null(Limits$Upper.Limit)) {
      return(list(Lower.Conf.Limit.R2 = Lambda2Rsquare(Limits$Lower.Limit, N = N),
                  Prob.Less.Lower = Limits$Prob.Less.Lower,
                  Upper.Conf.Limit.R2 = 1, Prob.Greater.Upper = 0))
    }

    if (length(Limits) == 2 && is.null(Limits$Lower.Limit)) {
      return(list(Lower.Conf.Limit.R2 = 0, Prob.Less.Lower = 0,
                  Upper.Conf.Limit.R2 = Lambda2Rsquare(Limits$Upper.Limit, N = N),
                  Prob.Greater.Upper = Limits$Prob.Greater.Upper))
    }
  }

  # Random predictors branch
  if (isTRUE(Random.Predictors)) {
    pul <- alpha.upper
    pll <- 1 - alpha.lower

    df1 <- N - 1
    df2 <- N - p - 1

    R2.Tilda <- R2 / (1 - R2)

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
        GAMMA <- sqrt(1 + yy)
        PHI.1 <- df1 * (GAMMA^2 - 1) + p
        PHI.2 <- df1 * (GAMMA^4 - 1) + p
        PHI.3 <- df1 * (GAMMA^6 - 1) + p
        g <- (PHI.2 - sqrt(PHI.2^2 - PHI.1 * PHI.3)) / PHI.1
        nu <- (PHI.2 - 2 * yy * GAMMA * (sqrt(df1 * df2))) / (g^2)
        LAMBDA.U <- yy * GAMMA * (sqrt(df1 * df2)) / (g^2)
        limit <- df2 * R2.Tilda / (nu * g)
        diff3 <- pf(limit, nu, df2, ncp = LAMBDA.U) - pul
        yy <- x1 / (1 - x1)
        GAMMA <- sqrt(1 + yy)
        PHI.1 <- df1 * (GAMMA^2 - 1) + p
        PHI.2 <- df1 * (GAMMA^4 - 1) + p
        PHI.3 <- df1 * (GAMMA^6 - 1) + p
        g <- (PHI.2 - sqrt(PHI.2^2 - PHI.1 * PHI.3)) / PHI.1
        nu <- (PHI.2 - 2 * yy * GAMMA * (sqrt(df1 * df2))) / (g^2)
        LAMBDA.U <- yy * GAMMA * (sqrt(df1 * df2)) / (g^2)
        limit <- df2 * R2.Tilda / (nu * g)
        diff1 <- pf(limit, nu, df2, ncp = LAMBDA.U) - pul
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
      GAMMA <- sqrt(1 + yy)
      PHI.1 <- df1 * (GAMMA^2 - 1) + p
      PHI.2 <- df1 * (GAMMA^4 - 1) + p
      PHI.3 <- df1 * (GAMMA^6 - 1) + p
      g <- (PHI.2 - sqrt(PHI.2^2 - PHI.1 * PHI.3)) / PHI.1
      nu <- (PHI.2 - 2 * yy * GAMMA * (sqrt(df1 * df2))) / (g^2)
      LAMBDA.U <- yy * GAMMA * (sqrt(df1 * df2)) / (g^2)
      limit <- df2 * R2.Tilda / (nu * g)
      diff3 <- pf(limit, nu, df2, ncp = LAMBDA.U) - pll

      while ((abs(diff3) > 1e-5) && (round(llrhosq, 5) < 1)) {
        x3 <- (x1 + x2) / 2
        yy <- x3 / (1 - x3)
        GAMMA <- sqrt(1 + yy)
        PHI.1 <- df1 * (GAMMA^2 - 1) + p
        PHI.2 <- df1 * (GAMMA^4 - 1) + p
        PHI.3 <- df1 * (GAMMA^6 - 1) + p
        g <- (PHI.2 - sqrt(PHI.2^2 - PHI.1 * PHI.3)) / PHI.1
        nu <- (PHI.2 - 2 * yy * GAMMA * (sqrt(df1 * df2))) / (g^2)
        LAMBDA.U <- yy * GAMMA * (sqrt(df1 * df2)) / (g^2)
        limit <- df2 * R2.Tilda / (nu * g)
        diff3 <- pf(limit, nu, df2, ncp = LAMBDA.U) - pll
        yy <- x1 / (1 - x1)
        GAMMA <- sqrt(1 + yy)
        PHI.1 <- df1 * (GAMMA^2 - 1) + p
        PHI.2 <- df1 * (GAMMA^4 - 1) + p
        PHI.3 <- df1 * (GAMMA^6 - 1) + p
        g <- (PHI.2 - sqrt(PHI.2^2 - PHI.1 * PHI.3)) / PHI.1
        nu <- (PHI.2 - 2 * yy * GAMMA * (sqrt(df1 * df2))) / (g^2)
        LAMBDA.U <- yy * GAMMA * (sqrt(df1 * df2)) / (g^2)
        limit <- df2 * R2.Tilda / (nu * g)
        diff1 <- pf(limit, nu, df2, ncp = LAMBDA.U) - pll
        if ((diff1 * diff3) < 0) x2 <- x3 else x1 <- x3
        llrhosq <- x3
      }
    }

    if (round(llrhosq, 5) == 1 && llrhosq > R2) llrhosq <- 0

    if (pll == 1) llrhosq <- 0
    if (pul == 0) ulrhosq <- 1
    if (llrhosq > ulrhosq) warning("There is a problem; the lower limit is greater than the upper limit (Are you at one of the boundaries of Rho^2 or is alpha very large?).")

    if (pll == 1) return(list(Lower.Conf.Limit.R2 = 0, Prob.Less.Lower = 0,
                              Upper.Conf.Limit.R2 = ulrhosq, Prob.Greater.Upper = pul))
    if (pul == 0) return(list(Lower.Conf.Limit.R2 = llrhosq, Prob.Less.Lower = 1 - pll,
                              Upper.Conf.Limit.R2 = 1, Prob.Greater.Upper = 0))
    return(list(Lower.Conf.Limit.R2 = llrhosq, Prob.Less.Lower = 1 - pll,
                Upper.Conf.Limit.R2 = ulrhosq, Prob.Greater.Upper = pul))
  }
}