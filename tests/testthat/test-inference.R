test_that("power.t", {
  res <- power_per_var_helper(
    vars.delta = 2.5,
    vars.sigma.sq = 4,
    vars.names = "v",
    n.patients = make_n_patients(per.group = 10),
    alpha = 0.07,
    distr = "t"
  )
  expect_equal(
    res$vars$power,
    power.t.test(
      n = 10, delta = 2.5, sd = 2,
      sig.level = 0.07, type = "two.sample",
      alternative = "two.sided",
      strict = FALSE
    )$power
  )
})

test_that("power.t.neg", {
  res <- power_per_var_helper(
    vars.delta = -2.5,
    vars.sigma.sq = 4,
    vars.names = "v",
    n.patients = make_n_patients(per.group = 10),
    alpha = 0.07,
    distr = "t"
  )
  expect_equal(
    res$vars$power,
    power.t.test(
      n = 10, delta = 2.5, sd = 2,
      sig.level = 0.07, type = "two.sample",
      alternative = "two.sided",
      strict = FALSE
    )$power
  )
})

test_that("power.norm", {
  res <- power_per_var_helper(
    vars.delta = 1.5,
    vars.sigma.sq = pi,
    vars.names = "v",
    n.patients = make_n_patients(per.group = 10),
    alpha = 0.07,
    distr = "norm"
  )
  expect_equal(
    res$vars$power,
    asbio::power.z.test(
      n = 10 / 2, effect = 1.5, sigma = sqrt(pi),
      alpha = 0.07,
      test = "two.tail", strict = FALSE
    )$power
  )
})

test_that("power.norm.net", {
  res <- power_per_var_helper(
    vars.delta = -1.5,
    vars.sigma.sq = pi,
    vars.names = "v",
    n.patients = make_n_patients(per.group = 10),
    alpha = 0.07,
    distr = "norm"
  )
  expect_equal(
    res$vars$power,
    asbio::power.z.test(
      n = 10 / 2, effect = 1.5, sigma = sqrt(pi),
      alpha = 0.07,
      test = "two.tail", strict = FALSE
    )$power
  )
})
