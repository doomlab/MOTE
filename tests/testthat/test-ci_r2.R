test_that("ci_r2 returns correctly structured list", {
  res <- ci_r2(
    r2                = 0.25,
    df1               = 3,
    df2               = 96,
    conf_level        = 0.95,
    random_predictors = FALSE
  )

  expect_type(res, "list")
  expect_named(
    res,
    c(
      "lower_conf_limit_r2",
      "prob_less_lower",
      "upper_conf_limit_r2",
      "prob_greater_upper"
    )
  )

  expect_true(res$lower_conf_limit_r2 >= 0)
  expect_true(res$upper_conf_limit_r2 <= 1)
  expect_true(res$lower_conf_limit_r2 <= res$upper_conf_limit_r2)
})

test_that("ci_r2 matches known value within tolerance", {
  res <- ci_r2(
    r2                = 0.25,
    df1               = 3,
    df2               = 96,
    conf_level        = 0.95,
    random_predictors = FALSE
  )

  # Reference values from MBESS::ci.R2(R2 = .25, df.1 = 3, df.2 = 96,
  #                       conf.level = .95, Random.Predictors = FALSE)
  expect_equal(res$lower_conf_limit_r2, 0.0976086, tolerance = 1e-6)
  expect_equal(res$upper_conf_limit_r2, 0.3668434, tolerance = 1e-6)

  expect_equal(res$prob_less_lower, 0.025, tolerance = 1e-6)
  expect_equal(res$prob_greater_upper, 0.025, tolerance = 1e-6)
})