test_that("recycle.to.vars", {
  trial <-
    make_endpoints(n.domains = 1) %>%
    with_n_patients(per.group = 17) %>%
    with_true_effect("tv")
  expect_equal(
    criteria_fun(
      p_univariate_has_stop,
      trial$endpoints,
      make_exact_study(trial)$summary
    )$stop.p,
    0.5
  )
})
