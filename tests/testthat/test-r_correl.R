test_that("r_correl returns correctly structured list", {
  res <- r_correl(
    r = -0.8676594,
    n = 32,
    a = 0.05
  )

  expect_type(res, "list")

  # Legacy names
  expect_true(all(c(
    "r",
    "rlow",
    "rhigh",
    "R2",
    "R2low",
    "R2high",
    "se",
    "n",
    "dfm",
    "dfe",
    "t",
    "F",
    "p",
    "estimate",
    "estimateR2",
    "statistic"
  ) %in% names(res)))

  # Snake_case aliases
  expect_true(all(c(
    "r_value",
    "r_lower_limit",
    "r_upper_limit",
    "r2_value",
    "r2_lower_limit",
    "r2_upper_limit",
    "standard_error",
    "sample_size",
    "df_model",
    "df_error",
    "t_value",
    "f_value",
    "p_value"
  ) %in% names(res)))
})

test_that("r_correl matches known example values", {
  res <- r_correl(
    r = -0.8676594,
    n = 32,
    a = 0.05
  )

  # Known r and CI from example
  expect_equal(res$r,     -0.8676594, tolerance = 1e-7)
  expect_equal(res$rlow,  -0.7387667, tolerance = 1e-7)
  expect_equal(res$rhigh, -0.9314170, tolerance = 1e-7)

  # Known R2 and CI from example
  expect_equal(res$R2,    0.7528328, tolerance = 1e-7)
  expect_equal(res$R2low, 0.5457763, tolerance = 1e-7)
  expect_equal(res$R2high, 0.8675377, tolerance = 1e-7)

  # Aliases should match legacy values
  expect_equal(res$r_value,        res$r)
  expect_equal(res$r_lower_limit,  res$rlow)
  expect_equal(res$r_upper_limit,  res$rhigh)

  expect_equal(res$r2_value,       res$R2)
  expect_equal(res$r2_lower_limit, res$R2low)
  expect_equal(res$r2_upper_limit, res$R2high)

  # Sample size and degrees of freedom
  expect_equal(res$sample_size, res$n)
  expect_equal(res$df_model,    res$dfm)
  expect_equal(res$df_error,    res$dfe)

  # Test statistics and p-value consistency
  expect_equal(res$t,      res$t_value)
  expect_equal(res$F,      res$f_value)
  expect_equal(res$p,      res$p_value)
  expect_true(res$p >= 0 && res$p <= 1)
})

test_that("r_correl APA-style strings are correctly formatted", {
  res <- r_correl(
    r = -0.8676594,
    n = 32,
    a = 0.05
  )

  expect_type(res$estimate, "character")
  expect_true(grepl("\\\
r$", res$estimate) || grepl("r", res$estimate))
  expect_true(grepl("CI", res$estimate))

  expect_type(res$estimateR2, "character")
  expect_true(grepl("R\\^2", res$estimateR2))
  expect_true(grepl("CI", res$estimateR2))

  expect_type(res$statistic, "character")
  expect_true(grepl("t", res$statistic))
})

test_that("dotted wrapper r.correl matches r_correl", {
  res1 <- r_correl(
    r = -0.8676594,
    n = 32,
    a = 0.05
  )

  res2 <- r.correl(
    r = -0.8676594,
    n = 32,
    a = 0.05
  )

  expect_equal(res1$rlow, res2$rlow)
})
