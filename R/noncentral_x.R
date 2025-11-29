#' Calculate the noncentral t confidence interval for d-values
#' Taken from MBESS by Ken Kelley
#' Exported here to avoid importing a zillion package dependencies
#'
#' @param chi_square the chi-square value found in the study
#' @param df the degrees of freedom.
#' @param conf_level the level of confidence for a symmetric confidence
#' interval.
#' @param alpha_lower the proportion of values beyond the lower limit of
#' the confidence interval (cannot be used with \code{conf_level}).
#' @param alpha_upper the proportion of values beyond the upper limit of
#' the confidence interval (cannot be used with \code{conf_level}).
#' @param tol is the tolerance of the iterative method for determining
#' the critical values.
#' @param jumping_prop helps move up and down to find
#' the right noncentrality limit
#' @noRd

noncentral_x <- function(chi_square = NULL, conf_level = .95,
                         df = NULL, alpha_lower = NULL, alpha_upper = NULL,
                         tol = 1e-9, jumping_prop = .10) {
  if (jumping_prop <= 0 || jumping_prop >= 1) {
    stop("The jumping_prop value must be between zero and one.")
  }

  if (is.null(chi_square)) {
    stop("Your 'chi_square' is not correctly specified.")
  }

  if (chi_square < 0) {
    stop("Your 'chi_square' is not correctly specified.")
  }

  if (is.null(df)) {
    stop("You must specify the degrees of freedom ('df').")
  }

  if (is.null(alpha_lower) && is.null(alpha_upper) && is.null(conf_level)) {
    stop("You need to specify the confidence interval parameters.")
  }

  if ((!is.null(alpha_lower) || !is.null(alpha_upper)) &&
        !is.null(conf_level)) {
    stop("You must specify only one method of defining the confidence limits.")
  }

  if (is.null(conf_level)) {
    if (alpha_lower < 0 || alpha_upper < 0) {
      stop("The upper and lower confidence limits must be larger than 0.")
    }
  }

  if (!is.null(conf_level)) {
    if (conf_level >= 1 || conf_level <= 0) {
      stop("Your confidence level ('conf_level') must be between 0 and 1.")
    }
    alpha_lower <- alpha_upper <- (1 - conf_level) / 2
  }

  if (alpha_lower == 0) ll <- 0
  if (alpha_upper == 0) ul <- Inf

  # Critical value for lower tail.
  failed_lower <- NULL

  if (alpha_lower > 0) {
    # Start with a lower value using the central chi-square distribution
    ll0 <- .01
    diff_val <- pchisq(q = chi_square, df = df, ncp = ll0) - (1 - alpha_lower)

    if (pchisq(q = chi_square, df = df, ncp = ll0) < (1 - alpha_lower)) {
      failed_lower <-
        if (pchisq(q = chi_square, df = df, ncp = 0) < 1 - alpha_lower) {
          ll0 <- .00000001
        }

      if (pchisq(q = chi_square, df = df, ncp = ll0) < 1 - alpha_lower) {
        failed_lower <- TRUE
      }

      if (failed_lower == TRUE) {
        warning(
          "The size of the effect combined with the degrees of 
          freedom is too small ",
          "to determine a lower confidence limit for 'alpha_lower' (or the ",
          "(1/2)(1 - 'conf_level') symmetric) value specified (set to zero).",
          call. = FALSE
        )
      }
    }

    if (is.null(failed_lower)) {
      # Define starting bounds; ll2 is overwritten later in the search
      ll1 <- ll2 <- ll0

      # Find values that bracket the solution
      while (diff_val > tol) {
        ll2 <- ll1 * (1 + jumping_prop)
        diff_val <-
          pchisq(q = chi_square, df = df, ncp = ll2) - (1 - alpha_lower)
        ll1 <- ll2
      }

      # Value directly before failure (too small)
      ll1 <- ll2 / (1 + jumping_prop)

      # Create bounding values (lower, mid, upper)
      ll_bounds <- c(ll1, (ll1 + ll2) / 2, ll2)

      diff_val <-
        pchisq(q = chi_square, df = df, ncp = ll_bounds[2]) -
        (1 - alpha_lower)

      # Refine bounds to find lower confidence limit
      while (abs(diff_val) > tol) {
        diff1 <-
          pchisq(q = chi_square, df = df, ncp = ll_bounds[1]) -
          (1 - alpha_lower) > tol
        diff2 <-
          pchisq(q = chi_square, df = df, ncp = ll_bounds[2]) -
          (1 - alpha_lower) > tol
        diff3 <-
          pchisq(q = chi_square, df = df, ncp = ll_bounds[3]) -
          (1 - alpha_lower) > tol

        if (diff1 == TRUE && diff2 == TRUE && diff3 == FALSE) {
          ll_bounds <-
            c(ll_bounds[2], (ll_bounds[2] + ll_bounds[3]) / 2, ll_bounds[3])
        }

        if (diff1 == TRUE && diff2 == FALSE && diff3 == FALSE) {
          ll_bounds <-
            c(ll_bounds[1], (ll_bounds[1] + ll_bounds[2]) / 2, ll_bounds[2])
        }

        diff_val <-
          pchisq(q = chi_square, df = df, ncp = ll_bounds[2]) -
          (1 - alpha_lower)
      }

      # Final lower confidence limit
      ll <- ll_bounds[2]
    }
  }

  if (!is.null(failed_lower)) {
    ll <- 0
  }

  # Critical value for upper tail.
  if (alpha_upper > 0) {
    failed_upper <- NULL
    # Starting value for upper lambda (slightly above lower limit)
    ul0 <- ll + .01

    diff_val <- pchisq(q = chi_square, df = df, ncp = ul0) - alpha_upper

    if (diff_val < 0) {
      ul0 <- .00000001
    }

    diff_val <- pchisq(q = chi_square, df = df, ncp = ul0) - alpha_upper
    if (diff_val < 0) {
      failed_upper <- TRUE
      warning(
        "The size of the effect combined with the degrees of 
        freedom is too small ",
        "to determine an upper confidence limit for 'alpha_upper' ",
        "(or (1/2)(1 - 'conf_level') symmetric) value specified.",
        call. = FALSE
      )
    }

    if (is.null(failed_upper)) {
      ul1 <- ul2 <- ul0

      # Find values that bracket the solution
      while (diff_val > tol) {
        ul2 <- ul1 * (1 + jumping_prop)
        diff_val <-
          pchisq(q = chi_square, df = df, ncp = ul2) - alpha_upper
        ul1 <- ul2
      }
      ul1 <- ul2 / (1 + jumping_prop)

      # Create bounding values (lower, mid, upper)
      ul_bounds <- c(ul1, (ul1 + ul2) / 2, ul2)

      diff_val <-
        pchisq(q = chi_square, df = df, ncp = ul_bounds[2]) - alpha_upper

      # Refine bounds to find upper confidence limit
      while (abs(diff_val) > tol) {
        diff1 <-
          pchisq(q = chi_square, df = df, ncp = ul_bounds[1]) - alpha_upper >
          tol
        diff2 <-
          pchisq(q = chi_square, df = df, ncp = ul_bounds[2]) - alpha_upper >
          tol
        diff3 <-
          pchisq(q = chi_square, df = df, ncp = ul_bounds[3]) - alpha_upper >
          tol

        if (diff1 == TRUE && diff2 == TRUE && diff3 == FALSE) {
          ul_bounds <-
            c(ul_bounds[2], (ul_bounds[2] + ul_bounds[3]) / 2, ul_bounds[3])
        }

        if (diff1 == TRUE && diff2 == FALSE && diff3 == FALSE) {
          ul_bounds <-
            c(ul_bounds[1], (ul_bounds[1] + ul_bounds[2]) / 2, ul_bounds[2])
        }

        diff_val <-
          pchisq(q = chi_square, df = df, ncp = ul_bounds[2]) - alpha_upper
      }

      # Final upper confidence limit
      ul <- ul_bounds[2]
    }

    if (!is.null(failed_upper)) {
      ul <- NA
    }
  }

  if (alpha_lower > 0 && alpha_upper > 0) {
    return(list(
      lower_limit        = ll,
      prob_less_lower    = alpha_lower,
      upper_limit        = ul,
      prob_greater_upper = alpha_upper
    ))
  }

  if (alpha_lower == 0 && alpha_upper > 0) {
    return(list(
      conf_interval_type = "one-sided",
      lower_limit        = 0,
      upper_limit        = ul,
      prob_greater_upper = alpha_upper
    ))
  }

  if (alpha_lower > 0 && alpha_upper == 0) {
    return(list(
      conf_interval_type = "one-sided",
      lower_limit        = ll,
      prob_less_lower    = alpha_lower,
      upper_limit        = Inf
    ))
  }
}
