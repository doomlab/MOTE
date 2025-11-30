test_that("ges_partial_ss_rm returns correctly structured list", {
  res <- ges_partial_ss_rm(
    dfm     = 1,
    dfe     = 157,
    ssm     = 2442.948,
    sss     = 76988.13,
    sse1    = 5402.567,
    sse2    = 8318.75,
    sse3    = 6074.417,
    f_value = 70.9927,
    a       = 0.05
  )

  expect_type(res, "list")

  # Legacy names
  expect_true(all(c(
    "ges",
    "geslow",
    "geshigh",
    "dfm",
    "dfe",
    "F",
    "p",
    "estimate",
    "statistic"
  ) %in% names(res)))

  # Snake_case aliases
  expect_true(all(c(
    "ges_value",
    "ges_lower_limit",
    "ges_upper_limit",
    "df_model",
    "df_error",
    "f_value",
    "p_value"
  ) %in% names(res)))
})


test_that("ges_partial_ss_rm matches known example values", {
  res <- ges_partial_ss_rm(
    dfm     = 1,
    dfe     = 157,
    ssm     = 2442.948,
    sss     = 76988.13,
    sse1    = 5402.567,
    sse2    = 8318.75,
    sse3    = 6074.417,
    f_value = 70.9927,
    a       = 0.05
  )

  # Known generalized eta-squared and CI from example
  expect_equal(res$ges,     0.02461984,  tolerance = 1e-6)
  expect_equal(res$geslow,  0.00000000,  tolerance = 1e-6)
  expect_equal(res$geshigh, 0.09257589,  tolerance = 1e-6)

  # Aliases should match legacy values
  expect_equal(res$ges_value,       res$ges)
  expect_equal(res$ges_lower_limit, res$geslow)
  expect_equal(res$ges_upper_limit, res$geshigh)

  # df aliases
  expect_equal(res$df_model, res$dfm)
  expect_equal(res$df_error, res$dfe)

  # F and p consistency
  expect_equal(res$F,      res$f_value)
  expect_equal(res$p,      res$p_value)
  expect_true(res$p >= 0 && res$p <= 1)
})


test_that("ges_partial_ss_rm APA-style strings are correctly formatted", {
  res <- ges_partial_ss_rm(
    dfm     = 1,
    dfe     = 157,
    ssm     = 2442.948,
    sss     = 76988.13,
    sse1    = 5402.567,
    sse2    = 8318.75,
    sse3    = 6074.417,
    f_value = 70.9927,
    a       = 0.05
  )

  expect_type(res$estimate, "character")
  expect_true(grepl("eta", res$estimate, ignore.case = TRUE))
  expect_true(grepl("CI", res$estimate))

  expect_type(res$statistic, "character")
  expect_true(grepl("F", res$statistic))
})


test_that("dotted wrapper ges.partial.SS.rm matches ges_partial_ss_rm", {
  res1 <- ges_partial_ss_rm(
    dfm     = 1,
    dfe     = 157,
    ssm     = 2442.948,
    sss     = 76988.13,
    sse1    = 5402.567,
    sse2    = 8318.75,
    sse3    = 6074.417,
    f_value = 70.9927,
    a       = 0.05
  )

  res2 <- ges.partial.SS.rm(
    dfm    = 1,
    dfe    = 157,
    ssm    = 2442.948,
    sss    = 76988.13,
    sse1   = 5402.567,
    sse2   = 8318.75,
    sse3   = 6074.417,
    Fvalue = 70.9927,
    a      = 0.05
  )

  expect_equal(res1$geslow, res2$geslow)
})
