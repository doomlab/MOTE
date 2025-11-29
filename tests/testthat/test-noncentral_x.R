test_that("noncentral_x returns correctly structured list", {
  res <- noncentral_x(
    chi_square = 10,
    df         = 5,
    conf_level = 0.95
  )

  expect_type(res, "list")
  expect_named(
    res,
    c("lower_limit", "prob_less_lower", "upper_limit", "prob_greater_upper")
  )

  expect_true(is.numeric(res$lower_limit))
  expect_true(is.numeric(res$upper_limit))
  expect_true(res$lower_limit >= 0)
  expect_true(res$upper_limit >= res$lower_limit)

  expect_true(res$prob_less_lower >= 0 && res$prob_less_lower <= 1)
  expect_true(res$prob_greater_upper >= 0 && res$prob_greater_upper <= 1)
})

test_that("noncentral_x limits give the correct tail probabilities", {
  chi_square <- 10
  df <- 5
  conf_level <- 0.95
  alpha <- (1 - conf_level) / 2

  res <- noncentral_x(
    chi_square = chi_square,
    df         = df,
    conf_level = conf_level
  )

  # The function stores the target tail probabilities directly
  expect_true(res$prob_less_lower >= 0 && res$prob_less_lower <= 1)
  expect_true(res$prob_greater_upper >= 0 && res$prob_greater_upper <= 1)

  expect_equal(res$prob_less_lower, alpha, tolerance = 1e-6)
  expect_equal(res$prob_greater_upper, alpha, tolerance = 1e-6)
})

test_that("noncentral_x one-sided lower CI behaves as expected", {
  res <- noncentral_x(
    chi_square  = 10,
    df          = 5,
    conf_level  = NULL,
    alpha_lower = 0,
    alpha_upper = 0.05
  )

  # Lower limit and lower-tail probability should reflect one-sided CI
  expect_equal(res$lower_limit, 0)
  expect_equal(res$Prob.Less.Lower, 0)

  expect_true(is.finite(res$upper_limit))
  expect_equal(
    pchisq(q = 10, df = 5, ncp = res$upper_limit),
    0.05,
    tolerance = 1e-4
  )
})

test_that("noncentral_x one-sided upper CI behaves as expected", {
  res <- noncentral_x(
    chi_square  = 10,
    df          = 5,
    conf_level  = NULL,
    alpha_lower = 0.05,
    alpha_upper = 0
  )

  # Upper limit and upper-tail probability are undefined in this mode
  expect_true(is.infinite(res$upper_limit) || is.na(res$upper_limit))
  expect_true(is.null(res$prob_greater_upper) || res$prob_greater_upper == 0)

  expect_true(is.finite(res$lower_limit))
  expect_true(res$lower_limit <= 10)
})

test_that("noncentral_x catches bad jumping_prop and chi_square", {
  expect_error(
    noncentral_x(
      chi_square  = 10,
      df          = 5,
      conf_level  = 0.95,
      jumping_prop = 0
    ),
    "jumping_prop value",
    ignore.case = TRUE
  )

  expect_error(
    noncentral_x(
      chi_square  = -1,
      df          = 5,
      conf_level  = 0.95
    ),
    "not correctly specified",
    ignore.case = TRUE
  )
})
