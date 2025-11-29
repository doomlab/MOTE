test_that("d_z_mean returns correctly structured list", {
  res <- d_z_mean(
    m1  = 10,
    mu  = 8,
    sd1 = 2.5,
    sig = 2,
    n   = 25,
    a   = 0.05
  )

  expect_type(res, "list")

  # Legacy names
  expect_true(all(c(
    "d", "dlow", "dhigh",
    "M1", "sd1", "se1",
    "M1low", "M1high",
    "Mu", "Sigma", "se2",
    "z", "p", "n",
    "estimate", "statistic"
  ) %in% names(res)))

  # Snake_case aliases (without estimate_d / statistic_z)
  expect_true(all(c(
    "d_lower_limit", "d_upper_limit",
    "mean_value", "mean_lower_limit", "mean_upper_limit",
    "sample_sd", "sample_se",
    "population_mean", "population_sd", "population_se",
    "z_value", "p_value", "sample_size"
  ) %in% names(res)))
})


test_that("d_z_mean computes correct d, CI, and z for a known example", {
  m1  <- 10
  mu  <- 8
  sd1 <- 2.5
  sig <- 2
  n   <- 25
  a   <- 0.05

  res <- d_z_mean(m1 = m1, mu = mu, sd1 = sd1, sig = sig, n = n, a = a)

  # Expected d using population sigma
  d_expected <- (m1 - mu) / sig
  expect_equal(res$d, d_expected, tolerance = 1e-6)

  # SE(d) = 1/sqrt(n) under known sigma
  se_d  <- 1 / sqrt(n)
  crit  <- qnorm(a / 2, lower.tail = FALSE)
  dlow_expected  <- d_expected - crit * se_d
  dhigh_expected <- d_expected + crit * se_d

  expect_equal(res$dlow,  dlow_expected,  tolerance = 1e-6)
  expect_equal(res$dhigh, dhigh_expected, tolerance = 1e-6)

  # CI should bracket the point estimate
  expect_true(res$dlow <= res$d)
  expect_true(res$dhigh >= res$d)

  # z based on population SE
  se_pop <- sig / sqrt(n)
  z_expected <- (m1 - mu) / se_pop
  expect_equal(res$z, z_expected, tolerance = 1e-6)

  # p in [0, 1]
  expect_true(res$p >= 0 && res$p <= 1)

  # Mean CI based on sample SE
  se_sample      <- sd1 / sqrt(n)
  m1low_expected <- m1 - crit * se_sample
  m1high_expected <- m1 + crit * se_sample

  expect_equal(res$M1low, m1low_expected, tolerance = 1e-6)
  expect_equal(res$M1high, m1high_expected, tolerance = 1e-6)

  # Check se1/se2 / aliases are mapped correctly
  expect_equal(res$se1, se_sample, tolerance = 1e-6)
  expect_equal(res$se2, se_pop,    tolerance = 1e-6)
  expect_equal(res$sample_se,     se_sample, tolerance = 1e-6)
  expect_equal(res$population_se, se_pop,    tolerance = 1e-6)
})


test_that("d_z_mean APA-style strings are correctly formed", {
  res <- d_z_mean(
    m1  = 10,
    mu  = 8,
    sd1 = 2.5,
    sig = 2,
    n   = 25,
    a   = 0.05
  )

  expect_type(res$estimate, "character")
  expect_true(grepl("\\$d\\$", res$estimate))
  expect_true(grepl("CI", res$estimate))

  expect_type(res$statistic, "character")
  expect_true(grepl("Z", res$statistic))
}
)