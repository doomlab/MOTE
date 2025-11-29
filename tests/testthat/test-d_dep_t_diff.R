test_that("d_dep_t_diff returns correctly structured list", {
  res <- d_dep_t_diff(
    mdiff  = 1.14,
    sddiff = 2.12,
    n      = 7,
    a      = .05
  )

  expect_type(res, "list")

  # Required original names
  expect_true(all(c(
    "d", "dlow", "dhigh",
    "mdiff", "Mlow", "Mhigh",
    "sddiff", "se", "n", "df",
    "t", "p", "estimate", "statistic"
  ) %in% names(res)))

  # Required snake_case aliases
  expect_true(all(c(
    "d_lower_limit", "d_upper_limit",
    "m_diff", "m_diff_lower_limit", "m_diff_upper_limit",
    "sd_diff", "se_diff",
    "t_value", "p_value",
    "sample_size", "degrees_freedom"
  ) %in% names(res)))
})


test_that("d_dep_t_diff returns correct numerical values", {
  res <- d_dep_t_diff(
    mdiff  = 1.14,
    sddiff = 2.12,
    n      = 7,
    a      = .05
  )

  expect_equal(res$d, 0.5377358, tolerance = 1e-6)
  expect_equal(res$dlow, -0.2791857, tolerance = 1e-6)
  expect_equal(res$dhigh, 1.317321, tolerance = 1e-6)
})


test_that("APA string is created correctly for d_dep_t_diff", {
  res <- d_dep_t_diff(
    mdiff  = 1.14,
    sddiff = 2.12,
    n      = 7,
    a      = .05
  )

  expect_type(res$estimate, "character")
  expect_true(grepl("d_z", res$estimate))
  expect_true(grepl("CI", res$estimate))
})


test_that("dotted version d.dep.t.diff matches snake_case version", {
  res1 <- d_dep_t_diff(
    mdiff  = 1.14,
    sddiff = 2.12,
    n      = 7,
    a      = .05
  )

  res2 <- d.dep.t.diff(
    mdiff  = 1.14,
    sddiff = 2.12,
    n      = 7,
    a      = .05
  )

  expect_equal(res1$dlow, res2$dlow)
})
