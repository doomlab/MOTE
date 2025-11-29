test_that("d_dep_t_diff_t returns correctly structured list", {
  res <- d_dep_t_diff_t(
    t_value = 1.43,
    n       = 7,
    a       = .05
  )

  expect_type(res, "list")

  # Original names
  expect_true(all(c(
    "d", "dlow", "dhigh",
    "n", "df",
    "t", "p",
    "estimate", "statistic"
  ) %in% names(res)))

  # Snake_case aliases
  expect_true(all(c(
    "d_lower_limit", "d_upper_limit",
    "t_value", "p_value",
    "sample_size", "degrees_freedom"
  ) %in% names(res)))
})


test_that("d_dep_t_diff_t returns correct numerical values", {
  res <- d_dep_t_diff_t(
    t_value = 1.43,
    n       = 7,
    a       = .05
  )

  expect_equal(res$d,     0.5404892, tolerance = 1e-6)
  expect_equal(res$dlow, -0.2770679, tolerance = 1e-6)
  expect_equal(res$dhigh, 1.320576,  tolerance = 1e-6)
})


test_that("APA strings are formatted correctly for d_dep_t_diff_t", {
  res <- d_dep_t_diff_t(
    t_value = 1.43,
    n       = 7,
    a       = .05
  )

  expect_type(res$estimate, "character")
  expect_true(grepl("d_z", res$estimate))
  expect_true(grepl("CI", res$estimate))

  expect_type(res$statistic, "character")
  expect_true(grepl("t", res$statistic))
})


test_that("dotted version matches snake_case version", {
  res1 <- d_dep_t_diff_t(
    t_value = 1.43,
    n       = 7,
    a       = .05
  )

  res2 <- d.dep.t.diff.t(
    t = 1.43,
    n = 7,
    a = .05
  )

  expect_equal(res1$dlow, res2$dlow)
})
