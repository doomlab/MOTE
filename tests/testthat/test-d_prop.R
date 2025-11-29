test_that("d_prop returns correctly structured list", {
  res <- d_prop(
    p1 = 0.25,
    p2 = 0.35,
    n1 = 100,
    n2 = 100,
    a  = 0.05
  )

  expect_type(res, "list")

  # Core effect size fields
  expect_true(all(c("d", "dlow", "dhigh") %in% names(res)))

  # Proportion-level fields
  expect_true(all(c(
    "p1", "se1", "p2", "se2",
    "n1", "n2",
    "z", "p",
    "estimate", "statistic"
  ) %in% names(res)))

  # Snake_case aliases
  expect_true(all(c(
    "d_lower_limit", "d_upper_limit",
    "p1_value", "se1_value",
    "p2_value", "se2_value",
    "sample_size_1", "sample_size_2",
    "z_value", "p_value"
  ) %in% names(res)))
})


test_that("d_prop computes SMD consistent with Bernoulli pooled SD", {
  p1 <- 0.25
  p2 <- 0.35
  n1 <- 100
  n2 <- 100
  a  <- 0.05

  res <- d_prop(p1 = p1, p2 = p2, n1 = n1, n2 = n2, a = a)

  # Recompute expected Cohen's d-style SMD for binary outcomes
  var1 <- p1 * (1 - p1)
  var2 <- p2 * (1 - p2)

  s_pooled <- sqrt(
    ((n1 - 1) * var1 + (n2 - 1) * var2) /
      (n1 + n2 - 2)
  )

  d_expected <- (p1 - p2) / s_pooled

  expect_equal(res$d, d_expected, tolerance = 1e-6)

  # CI should bracket the point estimate
  expect_true(res$dlow <= res$d)
  expect_true(res$dhigh >= res$d)
})


test_that("d_prop APA-style strings are correctly formed", {
  res <- d_prop(
    p1 = 0.25,
    p2 = 0.35,
    n1 = 100,
    n2 = 100,
    a  = 0.05
  )

  expect_type(res$estimate, "character")
  expect_true(grepl("d_{prop}", res$estimate))
  expect_true(grepl("CI", res$estimate))

  expect_true(grepl("Z", res$statistic))
  
})


test_that("dotted wrapper d.prop matches d_prop", {
  res1 <- d_prop(
    p1 = 0.25,
    p2 = 0.35,
    n1 = 100,
    n2 = 100,
    a  = 0.05
  )

  res2 <- d.prop(
    0.25, 0.35,
    100, 100,
    0.05
  )

  expect_equal(res1$dlow, res2$dlow)
})