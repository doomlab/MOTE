test_that("noncentral_t returns correctly structured list", {
  res <- noncentral_t(
    ncp          = 2,
    df           = 20,
    conf_level   = 0.95,
    alpha_lower  = NULL,
    alpha_upper  = NULL,
    t_value      = 2
  )

  expect_type(res, "list")
  expect_named(
    res,
    c("lower_limit", "Prob.Less.Lower", "upper_limit", "prob_greater_upper")
  )

  expect_true(is.numeric(res$lower_limit))
  expect_true(is.numeric(res$upper_limit))
  expect_true(is.numeric(res$Prob.Less.Lower))
  expect_true(is.numeric(res$prob_greater_upper))

  expect_true(res$lower_limit <= 2)
  expect_true(res$upper_limit >= 2)
})

test_that("noncentral_t limits give consistent tail probabilities", {
  ncp <- 2
  df <- 20
  conf_level <- 0.95
  alpha <- (1 - conf_level) / 2

  res <- noncentral_t(
    ncp          = ncp,
    df           = df,
    conf_level   = conf_level,
    alpha_lower  = NULL,
    alpha_upper  = NULL,
    t_value      = ncp
  )

  # By construction in the function, Prob.Less.Lower and prob_greater_upper
  # are defined via pt(q = ncp, ncp = limit, df = df, ...)
  expect_equal(
    res$Prob.Less.Lower,
    pt(q = ncp, ncp = res$lower_limit, df = df, lower.tail = FALSE),
    tolerance = 1e-4
  )

  expect_equal(
    res$prob_greater_upper,
    pt(q = ncp, ncp = res$upper_limit, df = df),
    tolerance = 1e-4
  )

  # These probabilities should be close to alpha for a symmetric CI
  expect_equal(res$Prob.Less.Lower, alpha, tolerance = 5e-2)
  expect_equal(res$prob_greater_upper, alpha, tolerance = 5e-2)
})

test_that("noncentral_t one-sided upper CI behaves as expected", {
  ncp <- 2
  df <- 20

  res <- noncentral_t(
    ncp          = ncp,
    df           = df,
    conf_level   = NULL,
    alpha_lower  = 0.05,
    alpha_upper  = 0,
    t_value      = ncp
  )

  # lower_limit should be finite and below ncp
  expect_true(is.finite(res$lower_limit))
  expect_lt(res$lower_limit, ncp)

  # upper_limit and its probability are one-sided/undefined in this mode
  expect_true(is.infinite(res$upper_limit) || is.na(res$upper_limit))
  expect_true(is.na(res$prob_greater_upper) || res$prob_greater_upper == 0)
})

test_that("noncentral_t one-sided lower CI behaves as expected", {
  ncp <- 2
  df <- 20

  res <- noncentral_t(
    ncp          = ncp,
    df           = df,
    conf_level   = NULL,
    alpha_lower  = 0,
    alpha_upper  = 0.05,
    t_value      = ncp
  )

  # lower_limit and its probability are one-sided/undefined in this mode
  expect_true(is.infinite(res$lower_limit) || is.na(res$lower_limit))
  expect_true(is.na(res$Prob.Less.Lower) || res$Prob.Less.Lower == 0)

  # upper_limit should be finite and above ncp
  expect_true(is.finite(res$upper_limit))
  expect_gt(res$upper_limit, ncp)
})

test_that("noncentral_t errors on invalid input", {
  expect_error(
    noncentral_t(
      ncp        = 2,
      df         = -1,
      conf_level = 0.95
    ),
    "degrees of freedom",
    ignore.case = TRUE
  )

  expect_error(
    noncentral_t(
      df         = 20,
      conf_level = 0.95
    ),
    "You need to specify either 
    'ncp' or its alias, 't.value,' you have not specified either",
    ignore.case = TRUE
  )
})
