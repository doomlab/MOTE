#' Calculate the noncentral t confidence interval for d-values
#' Taken from MBESS by Ken Kelley
#' Exported here to avoid importing a zillion package dependencies
#'
#' @param ncp the noncentrality parameter (e.g., observed *t*-value)
#' of interest.
#' @param df the degrees of freedom.
#' @param conf_level the level of confidence for a symmetric confidence
#' interval.
#' @param alpha_lower the proportion of values beyond the lower limit of
#' the confidence interval (cannot be used with \code{conf_level}).
#' @param alpha_upper the proportion of values beyond the upper limit of
#' the confidence interval (cannot be used with \code{conf_level}).
#' @param t_value alias for \code{ncp}
#' @param tol is the tolerance of the iterative method for determining
#' the critical values.
#' @param sup_int_warns Suppress internal warnings (from internal functions):
#' \code{TRUE} or \code{FALSE}
#' @param \dots allows one to potentially include parameter
#' values for inner functions
#' @noRd
noncentral_t <- function(ncp, df, conf_level = .95, alpha_lower = NULL,
                         alpha_upper = NULL, t_value, tol = 1e-9,
                         sup_int_warns = TRUE, ...) {

  if (missing(ncp)) {
    if (missing(t_value)) stop("You need to specify either 
    'ncp' or its alias, 't.value,' you have not specified either")
    ncp <- t_value
  }

  # General stop checks.
  if (df <= 0) {
    stop("The degrees of freedom must be some positive value.", call. = FALSE)
  }

  if (abs(ncp) > 37.62) {
    print(
      paste(
        "The observed noncentrality parameter of the noncentral t-distribution",
        "has exceeded 37.62 in magnitude (R's limitation for accurate",
        "probabilities from the noncentral t-distribution) in the function's",
        "iterative search for the appropriate value(s). 
        The results may be fine,",
        "but they might be inaccurate; use caution."
      )
    )
  }

  if (sup_int_warns == TRUE) {
    orig_warn <- options()$warn  # nolint: object_usage_linter
    options(warn = -1)
  }

  if (!is.null(conf_level) && is.null(alpha_lower) && !is.null(alpha_upper)) {
    stop(
      "You must choose either to use 'conf_level' or define the 'lower.alpha' ",
      "and 'upper.alpha' values; here, 'upper.alpha' is specified but ",
      "'lower.alpha' is not",
      call. = FALSE
    )
  }
  if (!is.null(conf_level) && !is.null(alpha_lower) && is.null(alpha_upper)) {
    stop(
      "You must choose either to use 'conf_level' or define the 'lower.alpha' ",
      "and 'upper.alpha' values; here, 'lower.alpha' is specified but ",
      "'upper.alpha' is not",
      call. = FALSE
    )
  }

  if (!is.null(conf_level) && is.null(alpha_lower) && is.null(alpha_upper)) {
    alpha_lower <- (1 - conf_level) / 2
    alpha_upper <- (1 - conf_level) / 2
  }


  conf_limits_nct_m1 <- function(ncp, df, conf_level = NULL,
                                 alpha_lower, alpha_upper,
                                 tol = 1e-9, sup_int_warns = TRUE, ...) {

    if (sup_int_warns == TRUE) {
      orig_warn <- options()$warn
      options(warn = -1)
    }

    min_ncp <- min(-150, -5 * ncp)
    max_ncp <- max(150, 5 * ncp)

    # Internal function for lower limit (for upper CI bound).
    # Uses the upper tail (alpha_lower) of the noncentral t.
    ci_nct_lower <- function(val_of_interest, ...) {
      (
        qt(
          p = alpha_lower,
          df = df,
          ncp = val_of_interest,
          lower.tail = FALSE,
          log.p = FALSE
        ) - ncp
      )^2
    }

    # Internal function for upper limit (for lower CI bound).
    # Uses the lower tail (alpha_upper) of the noncentral t.
    ci_nct_upper <- function(val_of_interest, ...) {
      (
        qt(
          p = alpha_upper,
          df = df,
          ncp = val_of_interest,
          lower.tail = TRUE,
          log.p = FALSE
        ) - ncp
      )^2
    }

    if (alpha_lower != 0) {
      if (sup_int_warns == TRUE) {
        low_lim <- suppressWarnings(
          optimize(
            f = ci_nct_lower,
            interval = c(min_ncp, max_ncp),
            alpha_lower = alpha_lower,
            df = df,
            ncp = ncp,
            maximum = FALSE,
            tol = tol
          )
        )
      }

      if (sup_int_warns == FALSE) {
        low_lim <- optimize(
          f = ci_nct_lower,
          interval = c(min_ncp, max_ncp),
          alpha_lower = alpha_lower,
          df = df,
          ncp = ncp,
          maximum = FALSE,
          tol = tol
        )
      }
    }

    if (alpha_upper != 0) {
      if (sup_int_warns == TRUE) {
        up_lim <- suppressWarnings(
          optimize(
            f = ci_nct_upper,
            interval = c(min_ncp, max_ncp),
            alpha_upper = alpha_upper,
            df = df,
            ncp = ncp,
            maximum = FALSE,
            tol = tol
          )
        )
      }

      if (sup_int_warns == FALSE) {
        up_lim <- optimize(
          f = ci_nct_upper,
          interval = c(min_ncp, max_ncp),
          alpha_upper = alpha_upper,
          df = df,
          ncp = ncp,
          maximum = FALSE,
          tol = tol
        )
      }
    }

    if (alpha_lower == 0) {
      result <- list(
        lower_limit        = -Inf,
        Prob.Less.Lower    = 0,
        upper_limit        = up_lim$minimum,
        prob_greater_upper = pt(q = ncp, ncp = up_lim$minimum, df = df)
      )
    }

    if (alpha_upper == 0) {
      result <- list(
        lower_limit        = low_lim$minimum,
        Prob.Less.Lower    = pt(
          q = ncp, ncp = low_lim$minimum, df = df, lower.tail = FALSE
        ),
        upper_limit        = Inf,
        prob_greater_upper = 0
      )
    }

    if (alpha_lower != 0 && alpha_upper != 0) {
      result <- list(
        lower_limit        = low_lim$minimum,
        Prob.Less.Lower    = pt(
          q = ncp, ncp = low_lim$minimum, df = df, lower.tail = FALSE
        ),
        upper_limit        = up_lim$minimum,
        prob_greater_upper = pt(q = ncp, ncp = up_lim$minimum, df = df)
      )
    }

    if (sup_int_warns == TRUE) {
      options(warn = orig_warn)
    }

    return(result)
  }
  conf_limits_nct_m2 <- function(ncp, df, conf_level = NULL,
                                 alpha_lower, alpha_upper,
                                 tol = 1e-9, sup_int_warns = TRUE, ...) {

    # Internal function for lower limit (for upper CI bound).
    ci_nct_lower <- function(val_of_interest, ...) {
      (
        qt(
          p = alpha_lower,
          df = df,
          ncp = val_of_interest,
          lower.tail = FALSE,
          log.p = FALSE
        ) - ncp
      )^2
    }

    # Internal function for upper limit (for lower CI bound).
    ci_nct_upper <- function(val_of_interest, ...) {
      (
        qt(
          p = alpha_upper,
          df = df,
          ncp = val_of_interest,
          lower.tail = TRUE,
          log.p = FALSE
        ) - ncp
      )^2
    }

    if (sup_int_warns == TRUE) {
      low_lim <- suppressWarnings(nlm(f = ci_nct_lower, p = ncp, ...))
      up_lim  <- suppressWarnings(nlm(f = ci_nct_upper, p = ncp, ...))
    }

    if (sup_int_warns == FALSE) {
      low_lim <- nlm(f = ci_nct_lower, p = ncp, ...)
      up_lim  <- nlm(f = ci_nct_upper, p = ncp, ...)
    }

    if (alpha_lower == 0) {
      result <- list(
        lower_limit        = -Inf,
        Prob.Less.Lower    = 0,
        upper_limit        = up_lim$estimate,
        prob_greater_upper = pt(q = ncp, ncp = up_lim$estimate, df = df)
      )
    }

    if (alpha_upper == 0) {
      result <- list(
        lower_limit        = low_lim$estimate,
        Prob.Less.Lower    = pt(
          q = ncp, ncp = low_lim$estimate, df = df, lower.tail = FALSE
        ),
        upper_limit        = Inf,
        prob_greater_upper = 0
      )
    }

    if (alpha_lower != 0 && alpha_upper != 0) {
      result <- list(
        lower_limit        = low_lim$estimate,
        Prob.Less.Lower    = pt(
          q = ncp, ncp = low_lim$estimate, df = df, lower.tail = FALSE
        ),
        upper_limit        = up_lim$estimate,
        prob_greater_upper = pt(q = ncp, ncp = up_lim$estimate, df = df)
      )
    }

    return(result)
  }

  # Now, use the each of the two methods.
  res_m1 <- res_m2 <- NULL

  try(
    res_m1 <- conf_limits_nct_m1(
      ncp = ncp,
      df = df,
      conf_level = NULL,
      alpha_lower = alpha_lower,
      alpha_upper = alpha_upper,
      tol = tol,
      sup_int_warns = sup_int_warns
    ),
    silent = TRUE
  )
  if (length(res_m1) != 4) res_m1 <- NULL

  try(
    res_m2 <- conf_limits_nct_m2(
      ncp = ncp,
      df = df,
      conf_level = NULL,
      alpha_lower = alpha_lower,
      alpha_upper = alpha_upper,
      tol = tol,
      sup_int_warns = sup_int_warns
    ),
    silent = TRUE
  )
  if (length(res_m2) != 4) res_m2 <- NULL

  # Now, set-up the test to find the best method.
  low_m1 <- res_m1$lower_limit
  prob_low_m1 <- res_m1$Prob.Less.Lower
  upper_m1 <- res_m1$upper_limit
  prob_upper_m1 <- res_m1$prob_greater_upper

  low_m2 <- res_m2$lower_limit
  prob_low_m2 <- res_m2$Prob.Less.Lower
  upper_m2 <- res_m2$upper_limit
  prob_upper_m2 <- res_m2$prob_greater_upper

  # Choose the best interval limits:
  ##Here low
  min_for_best_low <- min((c(prob_low_m1, prob_low_m2) - alpha_lower)^2)

  if (!is.null(res_m1)) {
    if (min_for_best_low == (prob_low_m1 - alpha_lower)^2) best_low <- 1
  }
  if (!is.null(res_m2)) {
    if (min_for_best_low == (prob_low_m2 - alpha_lower)^2) best_low <- 2
  }

  ##Here high
  min_for_best_up <- min((c(prob_upper_m1, prob_upper_m2) - alpha_upper)^2)

  if (!is.null(res_m1)) {
    if (min_for_best_up == (prob_upper_m1 - alpha_upper)^2) best_up <- 1
  }
  if (!is.null(res_m2)) {
    if (min_for_best_up == (prob_upper_m2 - alpha_upper)^2) best_up <- 2
  }
  #####################################

  if (is.null(res_m1)) {
    low_m1 <- NA
    prob_low_m1 <- NA
    upper_m1 <- NA
    prob_upper_m1 <- NA
  }
  if (is.null(res_m2)) {
    low_m2 <- NA
    prob_low_m2 <- NA
    upper_m2 <- NA
    prob_upper_m2 <- NA
  }

  result <- list(
    lower_limit = c(low_m1, low_m2)[best_low],
    Prob.Less.Lower = c(prob_low_m1, prob_low_m2)[best_low],
    upper_limit = c(upper_m1, upper_m2)[best_up],
    prob_greater_upper = c(prob_upper_m1, prob_upper_m2)[best_up]
  )

  return(result)
}
