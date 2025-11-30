test_that("eta_partial_ss returns correctly structured list", {
  res <- eta_partial_ss(
    dfm    = 4,
    dfe    = 990,
    ssm    = 338057.9,
    sse    = 32833499,
    f_value = 2.548,
    a      = 0.05
  )

  expect_type(res, "list")

  # Legacy names
  expect_true(all(c(
    "eta",
    "etalow",
    "etahigh",
    "dfm",
    "dfe",
    "F",
    "p",
    "estimate",
    "statistic"
  ) %in% names(res)))

  # Snake_case aliases
  expect_true(all(c(
    "eta_value",
    "eta_lower_limit",
    "eta_upper_limit",
    "df_model",
    "df_error",
    "f_value",
    "p_value"
  ) %in% names(res)))
})


test_that("eta_partial_ss matches known example values", {
  res <- eta_partial_ss(
    dfm    = 4,
    dfe    = 990,
    ssm    = 338057.9,
    sse    = 32833499,
    f_value = 2.548,
    a      = 0.05
  )

  # Known partial eta^2 and CI from example
  expect_equal(res$eta,     0.0101912,  tolerance = 1e-7)
  expect_equal(res$etalow,  0.0000000,  tolerance = 1e-7)
  expect_equal(res$etahigh, 0.02249241, tolerance = 1e-7)

  # Aliases should match legacy values
  expect_equal(res$eta_value,       res$eta)
  expect_equal(res$eta_lower_limit, res$etalow)
  expect_equal(res$eta_upper_limit, res$etahigh)

  # df aliases
  expect_equal(res$df_model, res$dfm)
  expect_equal(res$df_error, res$dfe)

  # F and p consistency
  expect_equal(res$F,      res$f_value)
  expect_equal(res$p,      res$p_value)
  expect_true(res$p >= 0 && res$p <= 1)
})


test_that("eta_partial_ss APA-style strings are correctly formatted", {
  res <- eta_partial_ss(
    dfm    = 4,
    dfe    = 990,
    ssm    = 338057.9,
    sse    = 32833499,
    f_value = 2.548,
    a      = 0.05
  )

  expect_type(res$estimate, "character")
  expect_true(grepl("eta", res$estimate))
  expect_true(grepl("CI", res$estimate))

  expect_type(res$statistic, "character")
  expect_true(grepl("F", res$statistic))
})


test_that("dotted wrapper eta.partial.SS matches eta_partial_ss", {
  res1 <- eta_partial_ss(
    dfm    = 4,
    dfe    = 990,
    ssm    = 338057.9,
    sse    = 32833499,
    f_value = 2.548,
    a      = 0.05
  )

  res2 <- eta.partial.SS(
    dfm    = 4,
    dfe    = 990,
    ssm    = 338057.9,
    sse    = 32833499,
    Fvalue = 2.548,
    a      = 0.05
  )

  expect_equal(res1$etalow, res2$etalow)
})
