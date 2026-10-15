test_that("NNP fixed-lambda fit works", {

  set.seed(123)

  n <- 50
  p <- 10
  q <- 4

  X <- matrix(rnorm(n * p), n, p)
  B <- matrix(rnorm(p * q), p, q)
  Y <- X %*% B + matrix(rnorm(n * q), n, q)

  fit <- nnp(
    X = X,
    Y = Y,
    lambda = 10
  )

  expect_s3_class(fit, "nnp")
  expect_true(is.matrix(fit$coefficients))
  expect_equal(dim(fit$coefficients), c(p, q))
  expect_true(isTRUE(fit$converged))
  expect_true(is.numeric(fit$objective))
})


test_that("NNP cross-validation works", {

  set.seed(123)

  n <- 50
  p <- 10
  q <- 4

  X <- matrix(rnorm(n * p), n, p)
  B <- matrix(rnorm(p * q), p, q)
  Y <- X %*% B + matrix(rnorm(n * q), n, q)

  fit <- nnp(
    X = X,
    Y = Y,
    nfold = 5,
    nlambda = 10,
    rule = "1se",
    seed = 123
  )

  expect_s3_class(fit, "nnp")

  expect_true(is.numeric(fit$lambda_min))
  expect_true(is.numeric(fit$lambda_1se))
  expect_true(is.numeric(fit$lambda_opt))

  expect_equal(fit$lambda_opt, fit$lambda_1se)

  expect_equal(
    dim(fit$coefficients),
    c(p, q)
  )
})


test_that("NNP prediction has correct dimensions", {

  set.seed(123)

  n <- 50
  p <- 10
  q <- 4

  X <- matrix(rnorm(n * p), n, p)
  Y <- matrix(rnorm(n * q), n, q)

  fit <- nnp(
    X = X,
    Y = Y,
    lambda = 10
  )

  pred <- predict(
    fit,
    newx = X
  )

  expect_equal(dim(pred), dim(Y))

  expect_equal(
    pred,
    X %*% fit$coefficients
  )
})
