test_that("recycle.to.vars", {
  expect_equal(
    recycle.to.vars(seq(3), domain.n.vars = seq(3)),
    c(1, 2, 2, 3, 3, 3)
  )
  expect_equal(
    recycle.to.vars(5, domain.n.vars = c(2, 2)),
    c(5, 5, 5, 5)
  )
})
