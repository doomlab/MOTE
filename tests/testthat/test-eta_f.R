test_that("eta_f returns correctly structured list", {
  res <- eta_f(
    dfm     = 2,
    dfe     = 8,
    Fvalue  = 5.134,
    a       = 0.05
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

test_that("eta_f matches known example values", {
  res <- eta_f(
    dfm     = 2,
    dfe     = 8,
    Fvalue  = 5.134,
    a       = 0.05
  )

  # Known eta and CI
  expect_equal(res$eta,     0.5620758, tolerance = 1e-6)
  expect_equal(res$etalow,  0.0000000, tolerance = 1e-6)
  expect_equal(res$etahigh, 0.8273919, tolerance = 1e-6)

  # Aliases match
  expect_equal(res$eta_value,       res$eta)
  expect_equal(res$eta_lower_limit, res$etalow)
  expect_equal(res$eta_upper_limit, res$etahigh)

  expect_equal(res$df_model, res$dfm)
  expect_equal(res$df_error, res$dfe)

  expect_equal(res$F,      res$f_value)
  expect_equal(res$p,      res$p_value)
  expect_true(res$p >= 0 && res$p <= 1)
})

test_that("eta_f APA-style strings are formatted correctly", {
  res <- eta_f(
    dfm     = 2,
    dfe     = 8,
    Fvalue  = 5.134,
    a       = 0.05
  )

  expect_type(res$estimate, "character")
  expect_true(grepl("eta", res$estimate))
  expect_true(grepl("CI", res$estimate))

  expect_type(res$statistic, "character")
  expect_true(grepl("F", res$statistic))
})

test_that("eta.F wrapper matches eta_f", {
  res1 <- eta_f(
    dfm     = 2,
    dfe     = 8,
    Fvalue  = 5.134,
    a       = 0.05
  )

  res2 <- eta.F(
    dfm    = 2,
    dfe    = 8,
    Fvalue = 5.134,
    a      = 0.05
  )

  expect_equal(res1$etalow, res2$etahigh)
})