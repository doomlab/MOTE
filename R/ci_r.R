#' Calculate the confidence interval of R
#' Taken from MBESS by Ken Kelley
#' Exported here to avoid importing a zillion package dependencies
#'
#' @param r the multiple correlation (R) found in the study
#' @param df1 the first degrees of freedom from F
#' @param df2 the second degrees of freedom from F
#' @param conf_level the level of confidence for a symmetric confidence
#' interval.
#' @param random_predictors logical; whether predictors are considered random
#' @param random_regressors logical; whether regressors are considered random
#' @param f_value the F value from the study
#' @param n sample size
#' @param k the number of predictors
#' @param alpha_lower the proportion of values beyond the lower limit of
#' the confidence interval (cannot be used with \code{conf_level}).
#' @param alpha_upper the proportion of values beyond the upper limit of
#' the confidence interval (cannot be used with \code{conf_level}).
#' @param \dots allows one to potentially include other arguments
#'
#' @noRd
#'
ci_r <- function(r = NULL,
                 df1 = NULL,
                 df2 = NULL,
                 conf_level = 0.95,
                 random_predictors = TRUE,
                 random_regressors = random_predictors,
                 f_value = NULL,
                 n = NULL,
                 k = NULL,
                 alpha_lower = NULL,
                 alpha_upper = NULL,
                 ...) {

  # Basic checks on r if supplied
  if (!is.null(r)) {
    if (r < 0) {
      stop("Your multiple correlation coefficient 
      ('r') cannot be less than zero.")
    }
    if (r > 1) {
      stop("Your multiple correlation coefficient 
      ('r') cannot be greater than one.")
    }
  }

  # If r is not supplied, derive df1 from k when possible
  if (is.null(r)) {
    if (is.null(df1)) {
      if (is.null(k)) {
        stop("You need to specify 'k' or 'df1'.")
      }
      df1 <- k
    }
  }

  # If df2 is missing, derive it from n and k
  if (is.null(df2)) {
    if (is.null(n)) {
      stop("You need to specify 'n' or 'df2'.")
    }
    if (is.null(k)) {
      stop("You need to specify 'k' or 'df2'.")
    }
    df2 <- n - k - 1
  }

  # Derive r from F if available; mirrors
  # original behavior which relied on F2Rsquare
  if (!is.null(f_value)) {
    r <- sqrt(f_r_square(f_value = f_value, df1 = df1, df2 = df2))
  }

  limits <- ci_r2(
    r2                = r^2,
    df1              = df1,
    df2              = df2,
    conf_level        = conf_level,
    random_predictors = random_predictors,
    random_regressors = random_regressors,
    f_value           = NULL,
    n                 = NULL,
    p                 = k,
    alpha_lower       = alpha_lower,
    alpha_upper       = alpha_upper,
    ...
  )

  list(
    lower_conf_limit_r   = sqrt(limits$lower_conf_limit_r2),
    prob_less_lower      = limits$prob_less_lower,
    upper_conf_limit_r   = sqrt(limits$upper_conf_limit_r2),
    prob_greater_upper   = limits$prob_greater_upper
  )
}
