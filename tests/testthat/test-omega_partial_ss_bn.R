test_that("omega_partial_ss_bn returns correctly structured list", {
  res <- omega_partial_ss_bn(
    dfm = 4,
    dfe = 990,
    msm = 338057.9 / 4,
    mse = 32833499 / 990,
    ssm = 338057.9,
    n   = 1000,
    a   = 0.05
  )

  expect_type(res, "list")

  # Legacy names
  expect_true(all(c(
    "omega",
    "omegalow",
    "omegahigh",
    "dfm",
    "dfe",
    "F",
    "p",
    "estimate",
    "statistic"
  ) %in% names(res)))

  # Snake_case aliases
  expect_true(all(c(
    "omega_value",
    "omega_lower_limit",
    "omega_upper_limit",
    "df_model",
    "df_error",
    "f_value",
    "p_value"
  ) %in% names(res)))
})

test_that("omega_partial_ss_bn matches known example values", {
  res <- omega_partial_ss_bn(
    dfm = 4,
    dfe = 990,
    msm = 338057.9 / 4,
    mse = 32833499 / 990,
    ssm = 338057.9,
    n   = 1000,
    a   = 0.05
  )

  # Known omega^2_p and CI from example
  expect_equal(res$omega,     0.006155047, tolerance = 1e-9)
  expect_equal(res$omegalow,  0.000000000, tolerance = 1e-9)
  expect_equal(res$omegahigh, 0.015549670, tolerance = 1e-9)

  # Aliases should match legacy values
  expect_equal(res$omega_value,       res$omega)
  expect_equal(res$omega_lower_limit, res$omegalow)
  expect_equal(res$omega_upper_limit, res$omegahigh)

  # df aliases
  expect_equal(res$df_model, res$dfm)
  expect_equal(res$df_error, res$dfe)

  # F and p consistency
  expect_equal(res$F,      res$f_value)
  expect_equal(res$p,      res$p_value)
  expect_true(res$p >= 0 && res$p <= 1)
})

test_that("omega_partial_ss_bn APA-style strings are correctly formatted", {
  res <- omega_partial_ss_bn(
    dfm = 4,
    dfe = 990,
    msm = 338057.9 / 4,
    mse = 32833499 / 990,
    ssm = 338057.9,
    n   = 1000,
    a   = 0.05
  )

  expect_type(res$estimate, "character")
  expect_true(grepl("omega", res$estimate, ignore.case = TRUE))
  expect_true(grepl("CI", res$estimate))

  expect_type(res$statistic, "character")
  expect_true(grepl("F", res$statistic))
})

test_that("dotted wrapper omega.partial.SS.bn matches omega_partial_ss_bn", {
  res1 <- omega_partial_ss_bn(
    dfm = 4,
    dfe = 990,
    msm = 338057.9 / 4,
    mse = 32833499 / 990,
    ssm = 338057.9,
    n   = 1000,
    a   = 0.05
  )

  res2 <- omega.partial.SS.bn(
    dfm = 4,
    dfe = 990,
    msm = 338057.9 / 4,
    mse = 32833499 / 990,
    ssm = 338057.9,
    n   = 1000,
    a   = 0.05
  )

  expect_equal(res1$omega, res2$omega)
})