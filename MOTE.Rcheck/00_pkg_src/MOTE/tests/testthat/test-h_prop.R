test_that("h_prop returns correctly structured list", {
  res <- h_prop(
    p1 = 0.25,
    p2 = 0.35,
    n1 = 100,
    n2 = 100,
    a  = 0.05
  )

  expect_type(res, "list")

  # Core effect size fields
  expect_true(all(c("h", "hlow", "hhigh") %in% names(res)))

  # Proportion-level and sample-size fields
  expect_true(all(c(
    "p1", "p2",
    "n1", "n2",
    "z", "p",
    "estimate", "statistic"
  ) %in% names(res)))

  # Snake_case aliases
  expect_true(all(c(
    "h_lower_limit", "h_upper_limit",
    "sample_size_1", "sample_size_2",
    "z_value", "p_value"
  ) %in% names(res)))
})

test_that("h_prop computes Cohen's h and CI behaves sensibly", {
  p1 <- 0.25
  p2 <- 0.35
  n1 <- 100
  n2 <- 100
  a  <- 0.05

  res <- h_prop(p1 = p1, p2 = p2, n1 = n1, n2 = n2, a = a)

  # Recompute expected Cohen's h
  h_expected <- 2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))

  expect_equal(res$h, h_expected, tolerance = 1e-6)

  # CI should bracket the point estimate
  expect_true(res$hlow <= res$h)
  expect_true(res$hhigh >= res$h)
})

test_that("h_prop APA-style strings are correctly formed", {
  res <- h_prop(
    p1 = 0.25,
    p2 = 0.35,
    n1 = 100,
    n2 = 100,
    a  = 0.05
  )

  expect_type(res$estimate, "character")
  expect_true(grepl("\\$h\\$", res$estimate))
  expect_true(grepl("CI", res$estimate))

  expect_type(res$statistic, "character")
  expect_true(grepl("Z", res$statistic))
})

test_that("dotted wrapper h.prop matches h_prop", {
  res1 <- h_prop(
    p1 = 0.25,
    p2 = 0.35,
    n1 = 100,
    n2 = 100,
    a  = 0.05
  )

  res2 <- h.prop(
    0.25, 0.35,
    100, 100,
    0.05
  )

  expect_equal(res1$dlow, res2$dlow)
})