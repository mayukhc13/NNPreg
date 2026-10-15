test_that("nnp rejects invalid lambda", {

  X <- matrix(rnorm(50 * 5), 50, 5)
  Y <- matrix(rnorm(50 * 3), 50, 3)

  expect_error(
    nnp(X, Y, lambda = -1)
  )

  expect_error(
    nnp(X, Y, lambda = 0)
  )
})


test_that("nnp rejects incompatible dimensions", {

  X <- matrix(rnorm(50 * 5), 50, 5)
  Y <- matrix(rnorm(40 * 3), 40, 3)

  expect_error(
    nnp(X, Y, lambda = 10)
  )
})


test_that("nnp rejects invalid CV rule", {

  X <- matrix(rnorm(50 * 5), 50, 5)
  Y <- matrix(rnorm(50 * 3), 50, 3)

  expect_error(
    nnp(
      X = X,
      Y = Y,
      nfold = 5,
      nlambda = 10,
      rule = "invalid"
    )
  )
})


test_that("predict requires newx", {

  X <- matrix(rnorm(50 * 5), 50, 5)
  Y <- matrix(rnorm(50 * 3), 50, 3)

  fit <- nnp(
    X = X,
    Y = Y,
    lambda = 10
  )

  expect_error(
    predict(fit)
  )
})


test_that("predict detects dimension mismatch", {

  X <- matrix(rnorm(50 * 5), 50, 5)
  Y <- matrix(rnorm(50 * 3), 50, 3)

  fit <- nnp(
    X = X,
    Y = Y,
    lambda = 10
  )

  newx_bad <- matrix(
    rnorm(20 * 4),
    20,
    4
  )

  expect_error(
    predict(
      fit,
      newx = newx_bad
    )
  )
})
