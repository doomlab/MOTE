
test_that("d_dep_t_rm returns correctly structured list", {
  res <- d_dep_t_rm(
    m1  = 5.57,
    m2  = 4.43,
    sd1 = 1.99,
    sd2 = 2.88,
    r   = 0.68,
    n   = 7,
    a   = 0.05
  )

  expect_type(res, "list")

  # Original names (backward compatible)
  expect_true(all(c(
    "d", "dlow", "dhigh",
    "M1", "sd1", "se1", "M1low", "M1high",
    "M2", "sd2", "se2", "M2low", "M2high",
    "r", "n", "df",
    "estimate"
  ) %in% names(res)))

  # Snake_case aliases (new preferred names)
  expect_true(all(c(
    "d_lower_limit", "d_upper_limit",
    "m1_value", "sd1_value", "se1_value",
    "m1_lower_limit", "m1_upper_limit",
    "m2_value", "sd2_value", "se2_value",
    "m2_lower_limit", "m2_upper_limit",
    "correlation",
    "sample_size", "degrees_freedom",
    "estimate_string"
  ) %in% names(res)))
})

test_that("d_dep_t_rm returns correct numerical values", {
  res <- d_dep_t_rm(
    m1  = 5.57,
    m2  = 4.43,
    sd1 = 1.99,
    sd2 = 2.88,
    r   = 0.68,
    n   = 7,
    a   = 0.05
  )

  expect_equal(res$d,     0.4318412,  tolerance = 1e-6)
  expect_equal(res$dlow, -0.3623148, tolerance = 1e-6)
  expect_equal(res$dhigh, 1.194367,  tolerance = 1e-6)
})

test_that("APA-style estimate string is created for d_dep_t_rm", {
  res <- d_dep_t_rm(
    m1  = 5.57,
    m2  = 4.43,
    sd1 = 1.99,
    sd2 = 2.88,
    r   = 0.68,
    n   = 7,
    a   = 0.05
  )

  expect_type(res$estimate, "character")
  expect_true(grepl("CI", res$estimate))
})

test_that("dotted version d.dep.t.rm matches snake_case d_dep_t_rm", {
  res1 <- d_dep_t_rm(
    m1  = 5.57,
    m2  = 4.43,
    sd1 = 1.99,
    sd2 = 2.88,
    r   = 0.68,
    n   = 7,
    a   = 0.05
  )

  res2 <- d.dep.t.rm(
    5.57, 4.43,
    1.99, 2.88,
    0.68,
    7,
    0.05
  )

  expect_equal(res1$dlow, res2$dlow)
})
