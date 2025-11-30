test_that("ges_ss_rm returns correctly structured list", {
  res <- omega_g_ss_rm(
    dfm     = 1,
    dfe     = 156,
    ssm     = 6842.46829,
    ssm2    = 14336.07886,
    sst     = sum(c(30936.498, 6842.46829,
                    14336.07886, 8657.094, 71.07608)),
    mss     = 30936.498 / 156,
    j       = 2,
    f_value = 34.503746,
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

test_that("ges_ss_rm matches known example values", {
  res <- omega_g_ss_rm(
    dfm     = 1,
    dfe     = 156,
    ssm     = 6842.46829,
    ssm2    = 14336.07886,
    sst     = sum(c(30936.498, 6842.46829,
                    14336.07886, 8657.094, 71.07608)),
    mss     = 30936.498 / 156,
    j       = 2,
    f_value = 34.503746,
    a       = 0.05
  )

  # Known omega^2_G and CI from example
  expect_equal(res$omega,     0.08791369, tolerance = 1e-8)
  expect_equal(res$omegalow,  0.02150822, tolerance = 1e-8)
  expect_equal(res$omegahigh, 0.18618840, tolerance = 1e-8)

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

test_that("ges_ss_rm APA-style strings are correctly formatted", {
  res <- omega_g_ss_rm(
    dfm     = 1,
    dfe     = 156,
    ssm     = 6842.46829,
    ssm2    = 14336.07886,
    sst     = sum(c(30936.498, 6842.46829,
                    14336.07886, 8657.094, 71.07608)),
    mss     = 30936.498 / 156,
    j       = 2,
    f_value = 34.503746,
    a       = 0.05
  )

  expect_type(res$estimate, "character")
  expect_true(grepl("omega", res$estimate, ignore.case = TRUE))
  expect_true(grepl("CI", res$estimate))

  expect_type(res$statistic, "character")
  expect_true(grepl("F", res$statistic))
})

test_that("dotted wrapper omega.gen.SS.rm matches ges_ss_rm", {
  res1 <- omega_g_ss_rm(
    dfm     = 1,
    dfe     = 156,
    ssm     = 6842.46829,
    ssm2    = 14336.07886,
    sst     = sum(c(30936.498, 6842.46829,
                    14336.07886, 8657.094, 71.07608)),
    mss     = 30936.498 / 156,
    j       = 2,
    f_value = 34.503746,
    a       = 0.05
  )

  res2 <- omega.gen.SS.rm(
    dfm    = 1,
    dfe    = 156,
    ssm    = 6842.46829,
    ssm2   = 14336.07886,
    sst    = sum(c(30936.498, 6842.46829,
                   14336.07886, 8657.094, 71.07608)),
    mss    = 30936.498 / 156,
    j      = 2,
    Fvalue = 34.503746,
    a      = 0.05
  )

  expect_equal(res1$omega, res2)
})