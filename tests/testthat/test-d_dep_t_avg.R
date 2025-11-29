test_that("d_dep_t_avg returns correct structure", {
  res <- d_dep_t_avg(
    m1 = 5.57, m2 = 4.43,
    sd1 = 1.99, sd2 = 2.88,
    n = 7, a = .05
  )

  expect_type(res, "list")

  # Check expected names exist
  expect_true(all(c(
    "d", "dlow", "dhigh",
    "d_lower_limit", "d_upper_limit",
    "M1", "m1", "sd1", "se1",
    "M1low", "m1_lower_limit",
    "M1high", "m1_upper_limit",
    "M2", "m2", "sd2", "se2",
    "M2low", "m2_lower_limit",
    "M2high", "m2_upper_limit",
    "n", "df", "estimate"
  ) %in% names(res)))
})

test_that("d_dep_t_avg returns correct numerical results (known example)", {
  res <- d_dep_t_avg(
    m1 = 5.57, m2 = 4.43,
    sd1 = 1.99, sd2 = 2.88,
    n = 7, a = .05
  )

  expect_equal(res$d, 0.4681725, tolerance = 1e-6)
  expect_equal(res$dlow, -0.3334172, tolerance = 1e-6)
  expect_equal(res$dhigh, 1.2360580, tolerance = 1e-6)
})

test_that("d_dep_t_avg APA estimate string is formatted", {
  res <- d_dep_t_avg(
    m1 = 5.57, m2 = 4.43,
    sd1 = 1.99, sd2 = 2.88,
    n = 7, a = .05
  )

  expect_type(res$estimate, "character")
  expect_true(grepl("d_{av}", res$estimate))
  expect_true(grepl("CI", res$estimate))
})

test_that("dotted version d.dep.t.avg matches snake_case version", {
  res1 <- d_dep_t_avg(
    m1 = 5.57, m2 = 4.43,
    sd1 = 1.99, sd2 = 2.88,
    n = 7, a = .05
  )

  res2 <- d.dep.t.avg(
    m1 = 5.57, m2 = 4.43,
    sd1 = 1.99, sd2 = 2.88,
    n = 7, a = .05
  )

  expect_equal(res1$dlow, res2$dlow)
})
