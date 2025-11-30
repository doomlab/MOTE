test_that("d_ind_t_t returns correctly structured list", {
  res <- d_ind_t_t(
    t_value = -2.6599,
    n1      = 4,
    n2      = 4,
    a       = 0.05
  )

  expect_type(res, "list")

  # Original names (backward compatible)
  expect_true(all(c(
    "d", "dlow", "dhigh",
    "n1", "n2", "df",
    "t", "p",
    "estimate", "statistic"
  ) %in% names(res)))

  # Snake_case aliases (new preferred names)
  expect_true(all(c(
    "d_lower_limit", "d_upper_limit",
    "sample_size_1", "sample_size_2",
    "degrees_freedom",
    "t_value", "p_value"
  ) %in% names(res)))
})

test_that("d_ind_t_t yields plausible values", {
  res <- d_ind_t_t(
    t_value = -2.6599,
    n1      = 4,
    n2      = 4,
    a       = 0.05
  )

  # Effect size should have the same sign as t
  expect_true(sign(res$d) == sign(-2.6599))

  # CI should bracket d
  expect_true(res$dlow <= res$d)
  expect_true(res$dhigh >= res$d)

  # df should equal n1 + n2 - 2
  expect_equal(res$df, 4 + 4 - 2)
  expect_equal(res$degrees_freedom, 4 + 4 - 2)

  # p should be between 0 and 1
  expect_true(res$p >= 0 && res$p <= 1)
  expect_equal(res$p, res$p_value)
})

test_that("APA-style strings are created for d_ind_t_t", {
  res <- d_ind_t_t(
    t_value = -2.6599,
    n1      = 4,
    n2      = 4,
    a       = 0.05
  )

  expect_type(res$estimate, "character")
  expect_true(grepl("d_s", res$estimate))
  expect_true(grepl("CI", res$estimate))

  expect_type(res$statistic, "character")
  expect_true(grepl("t", res$statistic))
})

test_that("dotted version d.ind.t.t matches snake_case d_ind_t_t", {
  res1 <- d_ind_t_t(
    t_value = -2.6599,
    n1      = 4,
    n2      = 4,
    a       = 0.05
  )

  res2 <- d.ind.t.t(
    t  = -2.6599,
    n1 = 4,
    n2 = 4,
    a  = 0.05
  )

  expect_equal(res1$dlow, res2$dlow)
})

