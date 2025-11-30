test_that("d_to_r returns correctly structured list", {
  res <- d_to_r(
    d  = -1.88,
    n1 = 4,
    n2 = 4,
    a  = 0.05
  )

  expect_type(res, "list")

  # Legacy names
  expect_true(all(c(
    "r", "rlow", "rhigh",
    "R2", "R2low", "R2high",
    "se", "n",
    "dfm", "dfe",
    "t", "F", "p",
    "estimate", "estimateR2", "statistic"
  ) %in% names(res)))

  # Snake_case aliases
  expect_true(all(c(
    "r_lower_limit", "r_upper_limit",
    "r2_value", "r2_lower_limit", "r2_upper_limit",
    "se_value", "sample_size",
    "degrees_freedom_model", "degrees_freedom_error",
    "t_value", "f_value", "p_value",
    "estimate_r", "estimate_r2"
  ) %in% names(res)))
})


test_that("d_to_r matches known example values for r", {
  res <- d_to_r(
    d  = -1.88,
    n1 = 4,
    n2 = 4,
    a  = 0.05
  )

  expect_equal(res$r,    -0.68491,   tolerance = 1e-6)
  expect_equal(res$rlow,  0.00000,   tolerance = 1e-6)
  expect_equal(res$rhigh, -0.9176276, tolerance = 1e-6)

  # Aliases should match
  expect_equal(res$r_lower_limit, res$rlow)
  expect_equal(res$r_upper_limit, res$rhigh)
})


test_that("d_to_r gives R2 consistent with r^2 and sensible CI", {
  res <- d_to_r(
    d  = -1.88,
    n1 = 4,
    n2 = 4,
    a  = 0.05
  )

  # R2 should be r^2
  r2_expected <- res$r^2
  expect_equal(res$R2, r2_expected, tolerance = 1e-6)
  expect_equal(res$r2_value, res$R2, tolerance = 1e-6)

  # CI should fall between 0 and 1 and bracket R2
  expect_gte(res$R2low, 0)
  expect_lte(res$R2high, 1)
  expect_true(res$R2low <= res$R2)
  expect_true(res$R2high >= res$R2)

  # Aliases should match legacy names
  expect_equal(res$r2_lower_limit, res$R2low)
  expect_equal(res$r2_upper_limit, res$R2high)
})


test_that("d_to_r APA-style strings are correctly formed", {
  res <- d_to_r(
    d  = -1.88,
    n1 = 4,
    n2 = 4,
    a  = 0.05
  )

  expect_type(res$estimate, "character")
  expect_true(grepl("\\$r\\$", res$estimate))
  expect_true(grepl("CI", res$estimate))

  expect_type(res$estimateR2, "character")
  expect_true(grepl("R\\^2", res$estimateR2))

  expect_type(res$estimate_r, "character")
  expect_type(res$estimate_r2, "character")

  expect_type(res$statistic, "character")
  expect_true(grepl("t", res$statistic))
})


test_that("dotted wrapper d.to.r matches d_to_r", {
  res1 <- d_to_r(
    d  = -1.88,
    n1 = 4,
    n2 = 4,
    a  = 0.05
  )

  res2 <- d.to.r(
    -1.88,
    4,
    4,
    0.05
  )

  expect_equal(res1$rlow, res2$rlow)
})

