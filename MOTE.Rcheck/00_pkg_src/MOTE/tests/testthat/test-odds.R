test_that("odds_ratio returns correctly structured list", {
  res <- odds_ratio(n11 = 10, n12 = 50, n21 = 20, n22 = 15, a = 0.05)

  expect_type(res, "list")

  # Legacy names
  expect_true(all(c("odds", "olow", "ohigh", "se") %in% names(res)))

  # Snake_case aliases
  expect_true(all(c(
    "odds_value",
    "odds_lower_limit",
    "odds_upper_limit",
    "standard_error"
  ) %in% names(res)))
})


test_that("odds_ratio matches known example values", {
  res <- odds_ratio(n11 = 10, n12 = 50, n21 = 20, n22 = 15, a = 0.05)

  expect_equal(res$odds, 0.15, tolerance = 1e-6)
  expect_equal(res$olow, 0.05780898, tolerance = 1e-6)
  expect_equal(res$ohigh, 0.3892129, tolerance = 1e-6)

  # Aliases should match legacy values
  expect_equal(res$odds_value,       res$odds)
  expect_equal(res$odds_lower_limit, res$olow)
  expect_equal(res$odds_upper_limit, res$ohigh)

  # SE alias match
  expect_equal(res$standard_error, res$se)
})


test_that("dotted wrapper odds() matches odds_ratio", {
  res1 <- odds_ratio(n11 = 10, n12 = 50, n21 = 20, n22 = 15, a = 0.05)
  res2 <- odds(n11 = 10, n12 = 50, n21 = 20, n22 = 15, a = 0.05)

  expect_equal(res1$odds, res2$odds)
})
