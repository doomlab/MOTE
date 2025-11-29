test_that("ci_r returns correctly structured list", {
  res <- ci_r(
    r                 = 0.5,
    df1               = 3,
    df2               = 96,
    conf_level        = 0.95,
    random_predictors = FALSE
  )

  expect_type(res, "list")
  expect_named(
    res,
    c(
      "lower_conf_limit_r",
      "prob_less_lower",
      "upper_conf_limit_r",
      "prob_greater_upper"
    )
  )

  expect_true(res$lower_conf_limit_r >= 0)
  expect_true(res$upper_conf_limit_r <= 1)
  expect_true(res$lower_conf_limit_r <= res$upper_conf_limit_r)
})

test_that("ci_r matches MBESS::ci.R2-based values for r", {
  res <- ci_r(
    r                 = 0.5,
    df1               = 3,
    df2               = 96,
    conf_level        = 0.95,
    random_predictors = FALSE
  )

  # From MBESS::ci.R2(R2 = .25, df.1 = 3, df.2 = 96,
  #                   conf.level = .95, Random.Predictors = FALSE)
  # R^2 CI: [0.0976086, 0.3668434]
  # r CI:   [sqrt(0.0976086), sqrt(0.3668434)]
  expect_equal(res$lower_conf_limit_r, sqrt(0.0976086), tolerance = 1e-6)
  expect_equal(res$upper_conf_limit_r, sqrt(0.3668434), tolerance = 1e-6)

  expect_equal(res$prob_less_lower, 0.025, tolerance = 1e-6)
  expect_equal(res$prob_greater_upper, 0.025, tolerance = 1e-6)
})

test_that("ci_r gives same result when r is derived from F", {
  # For R^2 = .25, df1 = 3, df2 = 96, the corresponding F is:
  # F = (R^2 / df1) / ((1 - R^2) / df2) = 10.6666667 # nolint
  f_value <- (0.25 / 3) / ((1 - 0.25) / 96)

  direct <- ci_r(
    r                 = 0.5,
    df1               = 3,
    df2               = 96,
    conf_level        = 0.95,
    random_predictors = FALSE
  )

  via_f <- ci_r(
    r                 = NULL,
    df1               = NULL,
    df2               = NULL,
    f_value           = f_value,
    n                 = 100,
    k                 = 3,
    conf_level        = 0.95,
    random_predictors = FALSE
  )

  expect_equal(
    via_f$lower_conf_limit_r,
    direct$lower_conf_limit_r,
    tolerance = 1e-6
  )
  expect_equal(
    via_f$upper_conf_limit_r,
    direct$upper_conf_limit_r,
    tolerance = 1e-6
  )
  expect_equal(
    via_f$prob_less_lower,
    direct$prob_less_lower,
    tolerance = 1e-6
  )
  expect_equal(
    via_f$prob_greater_upper,
    direct$prob_greater_upper,
    tolerance = 1e-6
  )
})

test_that("ci_r errors on missing required df info", {
  # missing both k and df1 when r is NULL
  expect_error(
    ci_r(r = NULL, df1 = NULL, df2 = 96),
    "You need to specify 'k' or 'df1'"
  )

  # missing both n and df2
  expect_error(
    ci_r(r = 0.5, df1 = 3, df2 = NULL, n = NULL),
    "You have not specified 'df1', 'df2', 'n', and/or 'p' correctly"
  )
})
