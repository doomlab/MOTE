test_that("omega_partial_ss_rm returns correctly structured list", {
  res <- omega_partial_ss_rm(
    dfm = 1,
    dfe = 157,
    msm = 2442.948 / 1,
    mse = 5402.567 / 157,
    mss = 76988.130 / 157,
    ssm = 2442.948,
    sss = 76988.13,
    sse = 5402.567,
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

test_that("omega_partial_ss_rm matches known example values", {
  res <- omega_partial_ss_rm(
    dfm = 1,
    dfe = 157,
    msm = 2442.948 / 1,
    mse = 5402.567 / 157,
    mss = 76988.130 / 157,
    ssm = 2442.948,
    sss = 76988.13,
    sse = 5402.567,
    a   = 0.05
  )

  # Known omega^2_p and CI from example
  expect_equal(res$omega,     0.02822812,  tolerance = 1e-8)
  expect_equal(res$omegalow,  0.00000000,  tolerance = 1e-8)
  expect_equal(res$omegahigh, 0.09893879,  tolerance = 1e-8)

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

test_that("omega_partial_ss_rm APA-style strings are correctly formatted", {
  res <- omega_partial_ss_rm(
    dfm = 1,
    dfe = 157,
    msm = 2442.948 / 1,
    mse = 5402.567 / 157,
    mss = 76988.130 / 157,
    ssm = 2442.948,
    sss = 76988.13,
    sse = 5402.567,
    a   = 0.05
  )

  expect_type(res$estimate, "character")
  expect_true(grepl("omega", res$estimate, ignore.case = TRUE))
  expect_true(grepl("CI", res$estimate))

  expect_type(res$statistic, "character")
  expect_true(grepl("F", res$statistic))
})

test_that("dotted wrapper omega.partial.SS.rm matches omega_partial_ss_rm", {
  res1 <- omega_partial_ss_rm(
    dfm = 1,
    dfe = 157,
    msm = 2442.948 / 1,
    mse = 5402.567 / 157,
    mss = 76988.130 / 157,
    ssm = 2442.948,
    sss = 76988.13,
    sse = 5402.567,
    a   = 0.05
  )

  res2 <- omega.partial.SS.rm(
    dfm = 1,
    dfe = 157,
    msm = 2442.948 / 1,
    mse = 5402.567 / 157,
    mss = 76988.130 / 157,
    ssm = 2442.948,
    sss = 76988.13,
    sse = 5402.567,
    a   = 0.05
  )

  expect_equal(res1$omegalow, res2$omegalow)
})