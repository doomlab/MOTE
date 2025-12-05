test_that("d_single_t_t returns correctly structured list", {
  res <- d_single_t_t(
    t = 9.968,
    n = 15,
    a = 0.05
  )

  expect_type(res, "list")

  # Legacy names
  expect_true(all(c(
    "d", "dlow", "dhigh",
    "n", "df",
    "t", "p",
    "estimate", "statistic"
  ) %in% names(res)))

  # Snake_case aliases
  expect_true(all(c(
    "d_lower_limit", "d_upper_limit",
    "sample_size", "degrees_freedom",
    "t_value", "p_value"
  ) %in% names(res)))
})


test_that("d_single_t_t computes correct d and CI for known example", {
  res <- d_single_t_t(
    t = 9.968,
    n = 15,
    a = 0.05
  )

  expect_equal(res$d,     2.573727, tolerance = 1e-6)
  expect_equal(res$dlow,  1.493432, tolerance = 1e-6)
  expect_equal(res$dhigh, 3.633535, tolerance = 1e-6)

  # CI should bracket point estimate
  expect_true(res$dlow <= res$d)
  expect_true(res$dhigh >= res$d)

  # df should be n - 1
  expect_equal(res$df, 15 - 1)
  expect_equal(res$degrees_freedom, 15 - 1)

  # p should be between 0 and 1 and match p_value
  expect_true(res$p >= 0 && res$p <= 1)
  expect_equal(res$p, res$p_value)
})


test_that("d_single_t_t APA-style strings are correctly formed", {
  res <- d_single_t_t(
    t = 9.968,
    n = 15,
    a = 0.05
  )

  expect_type(res$estimate, "character")
  expect_true(grepl("\\$d\\$", res$estimate))
  expect_true(grepl("CI", res$estimate))

  expect_type(res$statistic, "character")
  expect_true(grepl("t", res$statistic))
})


test_that("dotted wrapper d.single.t.t matches d_single_t_t", {
  res1 <- d_single_t_t(
    t = 9.968,
    n = 15,
    a = 0.05
  )

  res2 <- d.single.t.t(
    9.968,
    15,
    0.05
  )

  expect_equal(res1$dlow, res2$dlow)
})
