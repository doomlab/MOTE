#' Calculate the noncentral F confidence interval for effect sizes
#' Taken from MBESS by Ken Kelley
#' Exported here to avoid importing a zillion package dependencies
#'
#' @param f_value the F statistic found in the study
#' @param conf_level the level of confidence for a symmetric confidence
#' interval.
#' @param df1 the first degrees of freedom from F
#' @param df2 the second degrees of freedom from F
#' @param alpha_lower the proportion of values beyond the lower limit of
#' the confidence interval (cannot be used with \code{conf_level}).
#' @param alpha_upper the proportion of values beyond the upper limit of
#' the confidence interval (cannot be used with \code{conf_level}).
#' @param tol the tolerance of the iterative method for determining
#' the critical values.
#' @param jumping_prop helps move up and down to find the
#' right noncentrality limit
#'
#' @noRd

noncentral_f <-
  function(f_value = NULL, conf_level = .95, df1 = NULL,
           df2 = NULL, alpha_lower = NULL, alpha_upper = NULL,
           tol = 1e-9, jumping_prop = .10) {
    if (jumping_prop <= 0 || jumping_prop >= 1) {
      stop("The jumping_prop value must be between zero and one.")
    }

    if (is.null(f_value)) {
      stop("Your 'f_value' is not correctly specified.")
    }

    if (f_value < 0) {
      stop("Your 'f_value' is not correctly specified.")
    }

    if (is.null(df1) || is.null(df2)) {
      stop("You must specify the degrees of freedom ('df1' and 'df2').")
    }

    if (is.null(alpha_lower) && is.null(alpha_upper) && is.null(conf_level)) {
      stop("You need to specify the confidence interval parameters.")
    }

    if ((!is.null(alpha_lower) || !is.null(alpha_upper)) &&
          !is.null(conf_level)) {
      stop("You must specify only one method of 
      defining the confidence limits.")
    }

    if (!is.null(conf_level)) {
      if (conf_level >= 1 || conf_level <= 0) {
        stop(
          "Your confidence level ('conf_level') must be between 0 and 1."
        )
      }
      alpha_lower <- alpha_upper <- (1 - conf_level) / 2
    }

    if (alpha_lower == 0) alpha_lower <- NULL
    if (alpha_upper == 0) alpha_upper <- NULL

    # Critical value for lower tail.
    failed_lower <- NULL
    if (!is.null(alpha_lower)) {
      # Obtain a lower value by using the central F distribution
      ll0 <- qf(p = alpha_lower * .0005, df1 = df1, df2 = df2)
      diff_val <- pf(q = f_value, df1 = df1,
                     df2 = df2, ncp = ll0) - (1 - alpha_lower)

      if (
        pf(q = f_value, df1 = df1, df2 = df2, ncp = ll0) <
          (1 - alpha_lower)
      ) {
        failed_lower <-
          if (
            pf(q = f_value, df1 = df1, df2 = df2, ncp = 0) <
            (1 - alpha_lower)
          ) {
            ll0 <- .00000001
          }

        if (
          pf(q = f_value, df1 = df1, df2 = df2, ncp = ll0) <
            (1 - alpha_lower)
        ) {
          failed_lower <- TRUE
        }
      }

      if (is.null(failed_lower)) {
        # Start with the initial lambda estimate
        ll1 <- ll2 <- ll0

        # Find values that bracket the solution
        while (diff_val > tol) {
          ll2 <- ll1 * (1 + jumping_prop)
          diff_val <-
            pf(q = f_value, df1 = df1, df2 = df2, ncp = ll2) -
            (1 - alpha_lower)
          ll1 <- ll2
        }

        # Value directly before failure (too small)
        ll1 <- ll2 / (1 + jumping_prop)

        # Create bounding values (lower, mid, upper)
        ll_bounds <- c(ll1, (ll1 + ll2) / 2, ll2)

        diff_val <-
          pf(q = f_value, df1 = df1, df2 = df2, ncp = ll_bounds[2]) -
          (1 - alpha_lower)

        # Refine bounds to find lower confidence limit
        while (abs(diff_val) > tol) {
          diff1 <-
            pf(q = f_value, df1 = df1, df2 = df2, ncp = ll_bounds[1]) -
            (1 - alpha_lower) > tol
          diff2 <-
            pf(q = f_value, df1 = df1, df2 = df2, ncp = ll_bounds[2]) -
            (1 - alpha_lower) > tol
          diff3 <-
            pf(q = f_value, df1 = df1, df2 = df2, ncp = ll_bounds[3]) -
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
            pf(q = f_value, df1 = df1, df2 = df2, ncp = ll_bounds[2]) -
            (1 - alpha_lower)
        }

        # Final lower confidence limit
        ll <- ll_bounds[2]
      }
    }
    if (!is.null(failed_lower)) ll <- NA

    # Critical value for upper tail.
    if (!is.null(alpha_upper)) {
      failed_upper <- NULL
      ul0 <- qf(p = 1 - alpha_upper * .0005, df1 = df1, df2 = df2)
      diff_val <- pf(q = f_value, df1 = df1, df2 = df2, ncp = ul0) - alpha_upper

      if (diff_val < 0) ul0 <- .00000001

      diff_val <- pf(q = f_value, df1 = df1, df2 = df2, ncp = ul0) - alpha_upper
      if (diff_val < 0) {
        failed_upper <- TRUE
      }

      if (is.null(failed_upper)) {
        ul1 <- ul2 <- ul0
        while (diff_val > tol) {
          ul2 <- ul1 * (1 + jumping_prop)
          diff_val <- pf(q = f_value, df1 = df1,
                         df2 = df2, ncp = ul2) - alpha_upper
          ul1 <- ul2
        }
        ul1 <- ul2 / (1 + jumping_prop)

        ul_bounds <- c(ul1, (ul1 + ul2) / 2, ul2)

        diff_val <-
          pf(q = f_value, df1 = df1, df2 = df2, ncp = ul_bounds[2]) -
          alpha_upper
        while (abs(diff_val) > tol) {
          diff1 <-
            pf(q = f_value, df1 = df1, df2 = df2, ncp = ul_bounds[1]) -
            alpha_upper > tol
          diff2 <-
            pf(q = f_value, df1 = df1, df2 = df2, ncp = ul_bounds[2]) -
            alpha_upper > tol
          diff3 <-
            pf(q = f_value, df1 = df1, df2 = df2, ncp = ul_bounds[3]) -
            alpha_upper > tol

          if (diff1 == TRUE && diff2 == TRUE && diff3 == FALSE) {
            ul_bounds <- c(ul_bounds[2],
                           (ul_bounds[2] + ul_bounds[3]) / 2, ul_bounds[3])
          }

          if (diff1 == TRUE && diff2 == FALSE && diff3 == FALSE) {
            ul_bounds <- c(ul_bounds[1],
                           (ul_bounds[1] + ul_bounds[2]) / 2, ul_bounds[2])
          }

          diff_val <-
            pf(q = f_value, df1 = df1, df2 = df2, ncp = ul_bounds[2]) -
            alpha_upper

        }
        ul <- ul_bounds[2] # Confidence limit.
      }
      if (!is.null(failed_upper)) ul <- NA
    }
    if (!is.null(alpha_lower) && !is.null(alpha_upper)) {
      return(list(
        lower_limit       = ll,
        prob_less_lower   =
          1 - pf(q = f_value, df1 = df1, df2 = df2, ncp = ll),
        upper_limit       = ul,
        prob_greater_upper =
          pf(q = f_value, df1 = df1, df2 = df2, ncp = ul)
      ))
    }
    if (is.null(alpha_lower) && !is.null(alpha_upper)) {
      return(list(
        upper_limit       = ul,
        prob_greater_upper =
          pf(q = f_value, df1 = df1, df2 = df2, ncp = ul)
      ))
    }
    if (!is.null(alpha_lower) && is.null(alpha_upper)) {
      return(list(
        lower_limit     = ll,
        prob_less_lower =
          1 - pf(q = f_value, df1 = df1, df2 = df2, ncp = ll)
      ))
    }
  }
