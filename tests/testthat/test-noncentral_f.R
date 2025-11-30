test_that("noncentral_f returns correctly structured list", {
  res <- noncentral_f(
    f_value    = 4,
    df1        = 3,
    df2        = 120,
    conf_level = 0.95
  )

  expect_type(res, "list")
  expect_named(
    res,
    c("lower_limit", "prob_less_lower", "upper_limit", "prob_greater_upper")
  )

  expect_true(res$lower_limit >= 0)
  expect_true(res$upper_limit >= res$lower_limit)

  expect_true(res$prob_less_lower >= 0 && res$prob_less_lower <= 1)
  expect_true(res$prob_greater_upper >= 0 && res$prob_greater_upper <= 1)
})

test_that("noncentral_f limits give the correct tail probabilities", {
  f_value <- 4
  df1 <- 3
  df2 <- 120
  conf_level <- 0.95
  alpha <- (1 - conf_level) / 2

  res <- noncentral_f(
    f_value    = f_value,
    df1        = df1,
    df2        = df2,
    conf_level = conf_level
  )

  expect_equal(
    pf(q = f_value, df1 = df1, df2 = df2, ncp = res$lower_limit),
    1 - alpha,
    tolerance = 1e-4
  )

  expect_equal(
    pf(q = f_value, df1 = df1, df2 = df2, ncp = res$upper_limit),
    alpha,
    tolerance = 1e-4
  )
})

test_that("noncentral_f one-sided lower CI behaves as expected", {
  res <- noncentral_f(
    f_value     = 4,
    df1         = 3,
    df2         = 120,
    conf_level  = NULL,
    alpha_lower = 0,
    alpha_upper = 0.05
  )

  expect_false("lower_limit" %in% names(res))
  expect_false("prob_less_lower" %in% names(res))

  expect_true(is.finite(res$upper_limit))
  expect_equal(
    pf(q = 4, df1 = 3, df2 = 120, ncp = res$upper_limit),
    0.05,
    tolerance = 1e-4
  )
})

test_that("noncentral_f one-sided upper CI behaves as expected", {
  res <- noncentral_f(
    f_value     = 4,
    df1         = 3,
    df2         = 120,
    conf_level  = NULL,
    alpha_lower = 0.05,
    alpha_upper = 0
  )

  expect_true(is.finite(res$lower_limit))
  expect_true(is.null(res$upper_limit))
  expect_true(is.null(res$prob_greater_upper))
})

test_that("noncentral_f catches bad jumping_prop and f_value", {
  expect_error(
    noncentral_f(
      f_value     = 4,
      df1         = 3,
      df2         = 120,
      jumping_prop = 0
    ),
    "jumping_prop value",
    ignore.case = TRUE
  )

  expect_error(
    noncentral_f(
      f_value     = -1,
      df1         = 3,
      df2         = 120
    ),
    "not correctly specified",
    ignore.case = TRUE
  )
})
