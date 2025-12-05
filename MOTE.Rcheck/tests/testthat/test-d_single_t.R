test_that("d_single_t returns correctly structured list", {
  res <- d_single_t(
    m  = 1370,
    u  = 1080,
    sd = 112.7,
    n  = 14,
    a  = 0.05
  )

  expect_type(res, "list")

  # Legacy names
  expect_true(all(c(
    "d", "dlow", "dhigh",
    "m", "sd", "se",
    "Mlow", "Mhigh",
    "u", "n", "df",
    "t", "p",
    "estimate", "statistic"
  ) %in% names(res)))

  # Snake_case aliases
  expect_true(all(c(
    "d_lower_limit", "d_upper_limit",
    "mean_value", "sd_value", "se_value",
    "mean_lower_limit", "mean_upper_limit",
    "population_mean",
    "sample_size", "degrees_freedom",
    "t_value", "p_value"
  ) %in% names(res)))
})

test_that("d_single_t computes correct d and sensible CI", {
  m  <- 1370
  u  <- 1080
  sd <- 112.7
  n  <- 14
  a  <- 0.05

  res <- d_single_t(m = m, u = u, sd = sd, n = n, a = a)

  # d should equal (m - u) / sd for one-sample designs
  d_expected <- (m - u) / sd
  expect_equal(res$d, d_expected, tolerance = 1e-6)

  # CI should bracket the point estimate
  expect_true(res$dlow <= res$d)
  expect_true(res$dhigh >= res$d)

  # df should be n - 1
  expect_equal(res$df, n - 1)
  expect_equal(res$degrees_freedom, n - 1)

  # p should be between 0 and 1
  expect_true(res$p >= 0 && res$p <= 1)
  expect_equal(res$p, res$p_value)
})

test_that("d_single_t APA-style strings are correctly formed", {
  res <- d_single_t(
    m  = 1370,
    u  = 1080,
    sd = 112.7,
    n  = 14,
    a  = 0.05
  )

  expect_type(res$estimate, "character")
  expect_true(grepl("\\$d\\$", res$estimate))
  expect_true(grepl("CI", res$estimate))

  expect_type(res$statistic, "character")
  expect_true(grepl("t", res$statistic))
})

test_that("dotted wrapper d.single.t matches d_single_t", {
  res1 <- d_single_t(
    m  = 1370,
    u  = 1080,
    sd = 112.7,
    n  = 14,
    a  = 0.05
  )

  res2 <- d.single.t(
    1370, 1080,
    112.7,
    14,
    0.05
  )

  expect_equal(res1$dlow, res2$dlow)
})

test_that("d_single_t matches known example values", {
  res <- d_single_t(
    m  = 1370,
    u  = 1080,
    sd = 112.7,
    n  = 14,
    a  = 0.05
  )

  expect_equal(res$d,     2.573203,  tolerance = 1e-6)
  expect_equal(res$dlow,  1.453498,  tolerance = 1e-6)
  expect_equal(res$dhigh, 3.671227,  tolerance = 1e-6)
})
