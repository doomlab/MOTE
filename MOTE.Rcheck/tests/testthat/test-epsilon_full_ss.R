test_that("epsilon_full_ss returns correctly structured list", {
  res <- epsilon_full_ss(
    dfm = 2,
    dfe = 8,
    msm = 12.621,
    mse = 2.458,
    sst = 25.24 + 19.67,
    a   = 0.05
  )

  expect_type(res, "list")

  # Legacy names
  expect_true(all(c(
    "epsilon",
    "epsilonlow",
    "epsilonhigh",
    "dfm",
    "dfe",
    "F",
    "p",
    "estimate",
    "statistic"
  ) %in% names(res)))

  # Snake_case aliases
  expect_true(all(c(
    "epsilon_value",
    "epsilon_lower_limit",
    "epsilon_upper_limit",
    "df_model",
    "df_error",
    "f_value",
    "p_value"
  ) %in% names(res)))
})


test_that("epsilon_full_ss matches known example values", {
  res <- epsilon_full_ss(
    dfm = 2,
    dfe = 8,
    msm = 12.621,
    mse = 2.458,
    sst = 25.24 + 19.67,
    a   = 0.05
  )

  # Known epsilon and CI from documented example
  expect_equal(res$epsilon,    0.4525941, tolerance = 1e-6)
  expect_equal(res$epsilonlow, 0.0000000, tolerance = 1e-6)
  expect_equal(res$epsilonhigh, 0.770019, tolerance = 1e-6)

  # Aliases should match legacy names
  expect_equal(res$epsilon_value,       res$epsilon)
  expect_equal(res$epsilon_lower_limit, res$epsilonlow)
  expect_equal(res$epsilon_upper_limit, res$epsilonhigh)

  # df aliases
  expect_equal(res$df_model, res$dfm)
  expect_equal(res$df_error, res$dfe)

  # F and p should correspond and be consistent between legacy and snake_case
  expect_equal(res$F,       res$f_value)
  expect_equal(res$p,       res$p_value)
  expect_true(res$p >= 0 && res$p <= 1)
})


test_that("epsilon_full_ss APA-style strings are correctly formatted", {
  res <- epsilon_full_ss(
    dfm = 2,
    dfe = 8,
    msm = 12.621,
    mse = 2.458,
    sst = 25.24 + 19.67,
    a   = 0.05
  )

  expect_type(res$estimate, "character")
  expect_true(grepl("\\\\epsilon\\^2", res$estimate) ||
                grepl("\\$\\\\epsilon\\^2\\$", res$estimate))
  expect_true(grepl("CI", res$estimate))

  expect_type(res$statistic, "character")
  expect_true(grepl("F", res$statistic))
})


test_that("dotted wrapper epsilon.full.SS matches epsilon_full_ss", {
  res1 <- epsilon_full_ss(
    dfm = 2,
    dfe = 8,
    msm = 12.621,
    mse = 2.458,
    sst = 25.24 + 19.67,
    a   = 0.05
  )

  res2 <- epsilon.full.SS(
    dfm = 2,
    dfe = 8,
    msm = 12.621,
    mse = 2.458,
    sst = 25.24 + 19.67,
    a   = 0.05
  )

  expect_equal(res1$epsilonlow, res2$epsilonlow)
})