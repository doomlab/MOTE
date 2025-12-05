test_that("g_ind_t returns correctly structured list", {
  res <- g_ind_t(
    m1  = 17.75,
    m2  = 23,
    sd1 = 3.30,
    sd2 = 2.16,
    n1  = 4,
    n2  = 4,
    a   = 0.05
  )

  expect_type(res, "list")

  # Legacy names
  expect_true(all(c(
    "d", "dlow", "dhigh",
    "M1", "sd1", "se1", "M1low", "M1high",
    "M2", "sd2", "se2", "M2low", "M2high",
    "spooled", "sepooled", "correction",
    "n1", "n2", "df",
    "t", "p",
    "estimate", "statistic"
  ) %in% names(res)))

  # Snake_case aliases
  expect_true(all(c(
    "d_value", "d_lower_limit", "d_upper_limit",
    "mean1_value", "mean1_lower", "mean1_upper",
    "mean2_value", "mean2_lower", "mean2_upper",
    "sample_sd1", "sample_sd2",
    "sample_se1", "sample_se2",
    "pooled_sd", "pooled_se",
    "correction_value",
    "sample_size1", "sample_size2",
    "degrees_freedom",
    "t_value", "p_value"
  ) %in% names(res)))
})


test_that("g_ind_t computes correct g and CI for known example", {
  res <- g_ind_t(17.75, 23, 3.30, 2.16, 4, 4, .05)

  expect_equal(res$d,     -1.636941,   tolerance = 1e-6)
  expect_equal(res$dlow,  -3.102618,   tolerance = 1e-6)
  expect_equal(res$dhigh, -0.08885774, tolerance = 1e-6)

  # CI should bracket the estimate
  expect_true(res$dlow <= res$d)
  expect_true(res$dhigh >= res$d)

  # p-value valid
  expect_true(res$p >= 0 && res$p <= 1)
})


test_that("g_ind_t matches g.ind.t wrapper", {
  res1 <- g.ind.t(17.75, 23, 3.30, 2.16, 4, 4, .05)
  res2 <- g_ind_t(17.75, 23, 3.30, 2.16, 4, 4, .05)

  expect_equal(res1$dlow, res2$dlow)
})


test_that("APA strings are correctly produced", {
  res <- g_ind_t(17.75, 23, 3.30, 2.16, 4, 4, .05)

  expect_type(res$estimate, "character")
  expect_true(grepl("d", res$estimate))
  expect_true(grepl("CI", res$estimate))

  expect_type(res$statistic, "character")
  expect_true(grepl("t", res$statistic))
})