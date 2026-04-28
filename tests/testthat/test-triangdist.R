test_that("dtriang works correctly", {
  expect_equal(dtriang(0.5, 0, 1, 0.5), 2)
  expect_equal(dtriang(-1, 0, 1, 0.5), 0)
  expect_equal(dtriang(2, 0, 1, 0.5), 0)
})

test_that("ptriang works correctly", {
  expect_equal(ptriang(0, 0, 1, 0.5), 0)
  expect_equal(ptriang(1, 0, 1, 0.5), 1)
  expect_equal(ptriang(0.5, 0, 1, 0.5), 0.5)
})

test_that("qtriang inverts ptriang", {
  p <- seq(0.1, 0.9, by = 0.1)
  q <- qtriang(p, 0, 1, 0.5)
  expect_equal(ptriang(q, 0, 1, 0.5), p)
})

test_that("rtriang generates values in range", {
  x <- rtriang(1000, 0, 1, 0.5)
  expect_length(x, 1000)
  expect_true(all(x >= 0 & x <= 1))
})

test_that("functions stop on invalid parameters", {
  expect_error(dtriang(0.5, 1, 0, 0.5))
  expect_error(ptriang(0.5, 1, 0, 0.5))
  expect_error(qtriang(0.5, 1, 0, 0.5))

  expect_error(dtriang(0.5, 0, 1, 2))
  expect_error(ptriang(0.5, 0, 1, 2))
  expect_error(qtriang(0.5, 0, 1, 2))
})


test_that("boundary values are handled correctly", {
  expect_equal(dtriang(0, 0, 1, 0.5), 0)
  expect_equal(dtriang(1, 0, 1, 0.5), 0)

  expect_equal(ptriang(0, 0, 1, 0.5), 0)
  expect_equal(ptriang(1, 0, 1, 0.5), 1)

  expect_equal(qtriang(0, 0, 1, 0.5), 0)
  expect_equal(qtriang(1, 0, 1, 0.5), 1)
})


