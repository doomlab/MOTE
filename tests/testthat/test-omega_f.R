test_that("omega_f returns correctly structured list", {
  res <- omega_f(
    dfm     = 2,
    dfe     = 8,
    f_value = 5.134,
    n       = 11,
    a       = 0.05
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

test_that("omega_f matches known example values", {
  res <- omega_f(
    dfm     = 2,
    dfe     = 8,
    f_value = 5.134,
    n       = 11,
    a       = 0.05
  )

  # Known omega^2 and CI from example
  expect_equal(res$omega,     0.4291053,  tolerance = 1e-7)
  expect_equal(res$omegalow,  0.0000000,  tolerance = 1e-7)
  expect_equal(res$omegahigh, 0.7565302,  tolerance = 1e-7)

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

test_that("omega_f APA-style strings are correctly formatted", {
  res <- omega_f(
    dfm     = 2,
    dfe     = 8,
    f_value = 5.134,
    n       = 11,
    a       = 0.05
  )

  expect_type(res$estimate, "character")
  expect_true(grepl("omega", res$estimate, ignore.case = TRUE))
  expect_true(grepl("CI", res$estimate))

  expect_type(res$statistic, "character")
  expect_true(grepl("F", res$statistic))
})

test_that("dotted wrapper omega.F matches omega_f", {
  res1 <- omega_f(
    dfm     = 2,
    dfe     = 8,
    f_value = 5.134,
    n       = 11,
    a       = 0.05
  )

  res2 <- omega.F(
    dfm    = 2,
    dfe    = 8,
    Fvalue = 5.134,
    n      = 11,
    a      = 0.05
  )

  expect_equal(res1$omega, res2$omega)
})
