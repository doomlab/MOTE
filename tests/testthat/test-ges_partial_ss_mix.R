test_that("ges_partial_ss_mix returns correctly structured list", {
  res <- ges_partial_ss_mix(
    dfm    = 1,
    dfe    = 156,
    ssm    = 71.07608,
    sss    = 30936.498,
    sse    = 8657.094,
    f_value = 1.280784,
    a      = 0.05
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

test_that("ges_partial_ss_mix matches known example values", {
  res <- ges_partial_ss_mix(
    dfm    = 1,
    dfe    = 156,
    ssm    = 71.07608,
    sss    = 30936.498,
    sse    = 8657.094,
    f_value = 1.280784,
    a      = 0.05
  )

  # Known generalized eta-squared and CI from example
  expect_equal(res$ges,     0.001791924,  tolerance = 1e-9)
  expect_equal(res$geslow,  0.000000000,  tolerance = 1e-9)
  expect_equal(res$geshigh, 0.03792664,   tolerance = 1e-8)

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

test_that("ges_partial_ss_mix APA-style strings are correctly formatted", {
  res <- ges_partial_ss_mix(
    dfm    = 1,
    dfe    = 156,
    ssm    = 71.07608,
    sss    = 30936.498,
    sse    = 8657.094,
    f_value = 1.280784,
    a      = 0.05
  )

  expect_type(res$estimate, "character")
  expect_true(grepl("eta", res$estimate, ignore.case = TRUE))
  expect_true(grepl("CI", res$estimate))

  expect_type(res$statistic, "character")
  expect_true(grepl("F", res$statistic))
})

test_that("dotted wrapper ges.partial.SS.mix matches ges_partial_ss_mix", {
  res1 <- ges_partial_ss_mix(
    dfm    = 1,
    dfe    = 156,
    ssm    = 71.07608,
    sss    = 30936.498,
    sse    = 8657.094,
    f_value = 1.280784,
    a      = 0.05
  )

  res2 <- ges.partial.SS.mix(
    dfm    = 1,
    dfe    = 156,
    ssm    = 71.07608,
    sss    = 30936.498,
    sse    = 8657.094,
    Fvalue = 1.280784,
    a      = 0.05
  )

  expect_equal(res1$geslow, res2$geslow)
})
