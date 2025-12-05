test_that("d_z_z returns correctly structured list", {
  res <- d_z_z(
    z   = 2.5,
    n   = 100,
    a   = 0.05,
    sig = 4
  )

  expect_type(res, "list")

  # Legacy names
  expect_true(all(c(
    "d", "dlow", "dhigh",
    "sigma",
    "z", "p",
    "n",
    "estimate", "statistic"
  ) %in% names(res)))

  # Snake_case aliases
  expect_true(all(c(
    "d_lower_limit", "d_upper_limit",
    "sigma_value", "population_sd",
    "z_value", "p_value",
    "sample_size"
  ) %in% names(res)))
})

test_that("d_z_z computes correct d, CI, and p for a known example", {
  z   <- 2.5
  n   <- 100
  a   <- 0.05
  sig <- 4

  res <- d_z_z(z = z, n = n, a = a, sig = sig)

  # d = z / sqrt(n)
  d_expected <- z / sqrt(n)
  expect_equal(res$d, d_expected, tolerance = 1e-6)

  # SE(d) = 1/sqrt(n) under d = z/sqrt(n)
  se_d <- 1 / sqrt(n)
  crit <- qnorm(a / 2, lower.tail = FALSE)

  dlow_expected  <- d_expected - crit * se_d
  dhigh_expected <- d_expected + crit * se_d

  expect_equal(res$dlow,  dlow_expected,  tolerance = 1e-6)
  expect_equal(res$dhigh, dhigh_expected, tolerance = 1e-6)

  # CI should bracket the point estimate
  expect_true(res$dlow <= res$d)
  expect_true(res$dhigh >= res$d)

  # z and p should match the inputs/normal theory
  expect_equal(res$z, z, tolerance = 1e-6)
  expect_true(res$p >= 0 && res$p <= 1)

  # Sample size and sigma aliases
  expect_equal(res$n,           n)
  expect_equal(res$sample_size, n)
  expect_equal(res$sigma,       sig)
  expect_equal(res$sigma_value, sig)
  expect_equal(res$population_sd, sig)
})

test_that("d_z_z APA-style strings are correctly formed", {
  res <- d_z_z(
    z   = 2.5,
    n   = 100,
    a   = 0.05,
    sig = 4
  )

  expect_type(res$estimate, "character")
  expect_true(grepl("\\$d\\$", res$estimate))
  expect_true(grepl("CI", res$estimate))

  expect_type(res$statistic, "character")
  expect_true(grepl("Z", res$statistic))
})

test_that("dotted wrapper d.z.z matches d_z_z", {
  res1 <- d_z_z(
    z   = 2.5,
    n   = 100,
    a   = 0.05,
    sig = 4
  )

  res2 <- d.z.z(
    2.5,
    4,
    100,
    0.05
  )

  expect_equal(res1$dlow, res2$dlow)
})