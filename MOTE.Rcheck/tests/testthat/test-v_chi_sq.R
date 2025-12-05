test_that("v_chi_sq returns correctly structured list", {
  res <- v_chi_sq(
    x2 = 2.0496,
    n  = 60,
    r  = 3,
    c  = 3,
    a  = 0.05
  )

  expect_type(res, "list")

  # Legacy names
  expect_true(all(c(
    "v",
    "vlow",
    "vhigh",
    "n",
    "df",
    "x2",
    "p",
    "estimate",
    "statistic"
  ) %in% names(res)))

  # Snake_case aliases
  expect_true(all(c(
    "v_value",
    "v_lower_limit",
    "v_upper_limit",
    "sample_size",
    "df_total",
    "df_small",
    "chi_square",
    "p_value"
  ) %in% names(res)))
})


test_that("v_chi_sq matches known example values", {
  res <- v_chi_sq(
    x2 = 2.0496,
    n  = 60,
    r  = 3,
    c  = 3,
    a  = 0.05
  )

  # Known V and CI from example
  expect_equal(res$v,    0.1306905, tolerance = 1e-6)
  expect_equal(res$vlow, 0.1825742, tolerance = 1e-6)
  expect_equal(res$vhigh, 0.2980689, tolerance = 1e-6)

  # Aliases should match legacy values
  expect_equal(res$v_value,       res$v)
  expect_equal(res$v_lower_limit, res$vlow)
  expect_equal(res$v_upper_limit, res$vhigh)

  # Sample size and df aliases
  expect_equal(res$sample_size, res$n)
  expect_equal(res$df_total,    res$df)

  # Chi-square and p-value consistency
  expect_equal(res$chi_square, res$x2)
  expect_equal(res$p_value,    res$p)
  expect_true(res$p >= 0 && res$p <= 1)
})


test_that("v_chi_sq APA-style strings are correctly formatted", {
  res <- v_chi_sq(
    x2 = 2.0496,
    n  = 60,
    r  = 3,
    c  = 3,
    a  = 0.05
  )

  expect_type(res$estimate, "character")
  expect_true(grepl("V", res$estimate))
  expect_true(grepl("CI", res$estimate))

  expect_type(res$statistic, "character")
  expect_true(grepl("chi", tolower(res$statistic)))
})


test_that("dotted wrapper v.chi.sq matches v_chi_sq", {
  res1 <- v_chi_sq(
    x2 = 2.0496,
    n  = 60,
    r  = 3,
    c  = 3,
    a  = 0.05
  )

  res2 <- v.chi.sq(
    x2 = 2.0496,
    n  = 60,
    r  = 3,
    c  = 3,
    a  = 0.05
  )

  expect_equal(res1$vlow, res2$vlow)
})