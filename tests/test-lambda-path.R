test_that("NNP lambda sequence is valid", {

  set.seed(123)

  n <- 50
  p <- 10
  q <- 4

  X <- matrix(rnorm(n * p), n, p)
  Y <- matrix(rnorm(n * q), n, q)

  lambda <- nnp_lambda_seq(
    X = X,
    Y = Y,
    nlambda = 20
  )

  expect_length(lambda, 20)
  expect_true(is.numeric(lambda))
  expect_true(all(is.finite(lambda)))
  expect_true(all(lambda > 0))
  expect_true(all(diff(lambda) < 0))
})


test_that("NNP solution path works", {

  set.seed(123)

  n <- 50
  p <- 10
  q <- 4

  X <- matrix(rnorm(n * p), n, p)
  Y <- matrix(rnorm(n * q), n, q)

  lambda <- nnp_lambda_seq(
    X = X,
    Y = Y,
    nlambda = 5
  )

  path <- nnp_solver_path(
    X = X,
    Y = Y,
    lambdas = lambda
  )

  expect_s3_class(path, "nnp.path")
  expect_length(path, 5)

  for (fit in path) {
    expect_s3_class(fit, "nnp.fit")
    expect_true(is.matrix(fit$coefficients))
    expect_equal(
      dim(fit$coefficients),
      c(p, q)
    )
    expect_true(is.numeric(fit$lambda))
    expect_true(isTRUE(fit$converged))
  }
})
