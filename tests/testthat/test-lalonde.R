test_that("lalonde.per.var.1.noeffect", {
  set.seed(202111091144 %% .Machine$integer.max)
  responses.1 <- responses.default
  structure.1 <- make_endpoints(
    n.domains = 4,
    domain.n.vars = 1,
    corr.inter = 0.5,
    corr.intra = 1,
    vars.lrv = responses.1$LRV,
    vars.tv = responses.1$TV
  )
  r <- run_simulated_study(make_trial(
    endpoints = structure.1,
    vars.effect = "no.effect",
    n.patients = make_n_patients(per.group = 100)
  ))
  res <- lalonde_per_var(r)
  expect_equal(res$total.vars$n.go, 0)
  expect_equal(res$total.vars$n.stop, 4)
  expect_equal(res$total.vars$n.discuss, 0)
})

test_that("lalonde.per.var.1.tv", {
  set.seed(202111091144 %% .Machine$integer.max)
  responses.1 <- responses.default
  structure.1 <- make_endpoints(
    n.domains = 4,
    domain.n.vars = 1,
    corr.inter = 0.5,
    corr.intra = 1,
    vars.lrv = responses.1$LRV,
    vars.tv = responses.1$TV
  )
  r <- run_trial(make_trial(
    endpoints = structure.1,
    vars.effect = "tv",
    make_n_patients(per.group = 200)
  ))
  res <- lalonde_per_var(r)
  expect_equal(res$total.vars$n.go, 4)
  expect_equal(res$total.vars$n.stop, 0)
  expect_equal(res$total.vars$n.discuss, 0)
})

test_that("lalonde.per.var.1.lrv", {
  set.seed(202111091144 %% .Machine$integer.max)
  responses.1 <- responses.default
  structure.1 <- make_endpoints(
    n.domains = 4,
    domain.n.vars = 1,
    corr.inter = 0,
    corr.intra = 1,
    vars.lrv = responses.1$LRV,
    vars.tv = responses.1$TV
  )
  r <- run_trial(make_trial(
    endpoints = structure.1,
    vars.effect = "lrv",
    make_n_patients(per.group = 10)
  ))
  res <- lalonde_per_var(r)
  expect_equal(res$total.vars$n.go, 1)
  expect_equal(res$total.vars$n.discuss, 1)
  expect_equal(res$total.vars$n.stop, 2)
})

test_that("run.trial", {
  responses.1 <- responses.default
  eff <- list("tv", "tv", "lrv", 0)
  res <- lapply(seq(200), function(i) {
    r <- run_trial(make_trial(
      endpoints = make_endpoints(
        n.domains = 4,
        domain.n.vars = 1,
        corr.inter = 0,
        corr.intra = 1,
        vars.lrv = responses.1$LRV,
        vars.tv = responses.1$TV
      ),
      vars.effect = eff,
      n.patients = make_n_patients(per.group = 5)
    ))$summary
  })
  eff <- with(responses.1, c(TV, TV, LRV, 0))
  expect_equal(unname(rowMeans(sapply(res, function(x) {
    x$vars$control.hat
  }))),
  0 * eff,
  tolerance = 5 / sqrt(5 * 200)
  )
  expect_equal(unname(rowMeans(sapply(res, function(x) {
    x$vars$active.hat
  }))),
  1 * eff,
  tolerance = 5 / sqrt(5 * 200)
  )
  expect_equal(unname(rowMeans(sapply(res, function(x) {
    x$vars$delta.hat
  }))),
  (1 - 0) * eff,
  tolerance = 5 / sqrt(5 * 200)
  )
})
