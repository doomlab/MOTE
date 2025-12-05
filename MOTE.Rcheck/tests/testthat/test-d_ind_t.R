test_that("d_ind_t returns correctly structured list", {
  res <- d_ind_t(
    m1  = 17.75,
    m2  = 23,
    sd1 = 3.30,
    sd2 = 2.16,
    n1  = 4,
    n2  = 4,
    a   = 0.05
  )

  expect_type(res, "list")

  # Original names (backward compatible)
  expect_true(all(c(
    "d", "dlow", "dhigh",
    "M1", "sd1", "se1", "M1low", "M1high",
    "M2", "sd2", "se2", "M2low", "M2high",
    "spooled", "sepooled",
    "n1", "n2", "df",
    "t", "p",
    "estimate", "statistic"
  ) %in% names(res)))

  # Snake_case aliases (new preferred names)
  expect_true(all(c(
    "d_lower_limit", "d_upper_limit",
    "m1_value", "sd1_value", "se1_value",
    "m1_lower_limit", "m1_upper_limit",
    "m2_value", "sd2_value", "se2_value",
    "m2_lower_limit", "m2_upper_limit",
    "pooled_sd", "pooled_se",
    "sample_size_1", "sample_size_2",
    "degrees_freedom",
    "t_value", "p_value"
  ) %in% names(res)))
})


test_that("d_ind_t returns correct numerical values", {
  res <- d_ind_t(
    m1  = 17.75,
    m2  = 23,
    sd1 = 3.30,
    sd2 = 2.16,
    n1  = 4,
    n2  = 4,
    a   = 0.05
  )

  expect_equal(res$d,     -1.882482,  tolerance = 1e-6)
  expect_equal(res$dlow,  -3.56801,   tolerance = 1e-5)
  expect_equal(res$dhigh, -0.1021864, tolerance = 1e-6)
})


test_that("APA-style strings are created for d_ind_t", {
  res <- d_ind_t(
    m1  = 17.75,
    m2  = 23,
    sd1 = 3.30,
    sd2 = 2.16,
    n1  = 4,
    n2  = 4,
    a   = 0.05
  )

  expect_type(res$estimate, "character")
  expect_true(grepl("d_s", res$estimate))
  expect_true(grepl("CI", res$estimate))

  expect_type(res$statistic, "character")
  expect_true(grepl("t", res$statistic))
})

test_that("dotted version d.ind.t matches snake_case d_ind_t", {
  res1 <- d_ind_t(
    m1  = 17.75,
    m2  = 23,
    sd1 = 3.30,
    sd2 = 2.16,
    n1  = 4,
    n2  = 4,
    a   = 0.05
  )

  res2 <- d.ind.t(
    17.75, 23,
    3.30, 2.16,
    4, 4,
    0.05
  )

  expect_equal(res1$dlow, res2$dlow)
})
