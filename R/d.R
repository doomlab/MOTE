#' General interface for Cohen's d
#'
#' @description
#' `d()` is a convenience wrapper that will eventually route to the appropriate
#' Cohen's *d* helper function based on the arguments supplied. This allows
#' users to call a single function for different study designs while
#' maintaining backward compatibility with the more specific helpers.
#'
#'
#' @section Supported designs:
#'
#' - `"dep_t_avg"` — paired/dependent t-test with average SD denominator.
#'   Supply `m1`, `m2`, `sd1`, `sd2`, and `n`. In this case, `d()` will call
#'   [d_dep_t_avg()] with the same arguments.
#'
#' - `"dep_t_diff"` — paired/dependent t-test using the
#' **SD of the difference scores**.
#'   Supply `mdiff`, `sddiff`, and `n`. In this case, `d()` will call
#'   [d_dep_t_diff()] with the same arguments.
#'
#'
#' @param m1 Means of the two conditions or measurements.
#' @param m2 Means of the two conditions or measurements.
#' @param sd1 Standard deviations for the two conditions or measurements.
#' @param sd2 Standard deviations for the two conditions or measurements.
#' @param mdiff Mean difference between paired observations.
#' @param sddiff Standard deviation of the difference scores.
#' @param n Sample size (number of paired observations).
#' @param a Significance level used when computing confidence intervals.
#'   Defaults to `0.05`.
#' @param design Character string specifying the study design.
#' @param ... Reserved for future arguments and passed on to the underlying
#'   helper functions when appropriate.
#'
#' @return
#' A list with the same structure as returned by the underlying helper
#' function. For the current paired-means case, this is the output of
#' [d_dep_t_avg()], which includes:
#' \itemize{
#'   \item `d` – Cohen's d using the average SD denominator.
#'   \item `dlow`, `dhigh` – lower and upper confidence limits for `d`.
#'   \item Snake_case aliases such as `d_lower_limit` and `d_upper_limit`.
#'   \item Descriptive statistics (means, SDs, SEs, and their confidence
#'         limits) for each group.
#' }
#'
#' @examples
#' # Paired/dependent t-test using average SD denominator
#' # These arguments will route d() to d_dep_t_avg()
#' d(
#'   m1 = 5.57, m2 = 4.43,
#'   sd1 = 1.99, sd2 = 2.88,
#'   n = 7, a = .05,
#'   design = "dep_t_avg"
#' )
#'
#' # You can also call the helper directly
#' d_dep_t_avg(
#'   m1 = 5.57, m2 = 4.43,
#'   sd1 = 1.99, sd2 = 2.88,
#'   n = 7, a = .05
#' )
#'
#' @export
d <- function(m1 = NULL,
              m2 = NULL,
              sd1 = NULL,
              sd2 = NULL,
              mdiff = NULL,
              sddiff = NULL,
              n = NULL,
              a = 0.05,
              design,
              ...) {

  design <- match.arg(design, choices = c("dep_t_avg", "dep_t_diff"))

  if (design == "dep_t_avg") {
    if (is.null(m1) || is.null(m2) ||
          is.null(sd1) || is.null(sd2) ||
          is.null(n)) {
      stop(
        "For design = 'dep_t_avg', you must supply m1, m2, sd1, sd2, and n."
      )
    }

    return(
      d_dep_t_avg(
        m1 = m1,
        m2 = m2,
        sd1 = sd1,
        sd2 = sd2,
        n = n,
        a = a
      )
    )
  }

  if (design == "dep_t_diff") {
    if (is.null(m1) && is.null(m2) &&
        is.null(sd1) && is.null(sd2)) {
      # This design uses mdiff and sddiff instead of m1/m2/sd1/sd2
      # Ensure required arguments exist
      if (is.null(mdiff) || is.null(sddiff) || is.null(n)) {
        stop(
          "For design = 'dep_t_diff', you must supply mdiff, sddiff, and n."
        )
      }
    }

    # Call the helper
    return(
      d_dep_t_diff(
        mdiff  = mdiff,
        sddiff = sddiff,
        n      = n,
        a      = a
      )
    )
  }
}