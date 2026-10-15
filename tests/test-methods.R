test_that("NNP print and summary methods work", {

  set.seed(123)

  X <- matrix(rnorm(50 * 10), 50, 10)
  Y <- matrix(rnorm(50 * 5), 50, 5)

  fit <- nnp(
    X = X,
    Y = Y,
    lambda = 1
  )

  expect_s3_class(fit, "nnp")

  expect_output(print(fit))
  expect_output(summary(fit))
})


test_that("NNP coef method works", {

  set.seed(123)

  X <- matrix(rnorm(50 * 10), 50, 10)
  Y <- matrix(rnorm(50 * 5), 50, 5)

  fit <- nnp(
    X = X,
    Y = Y,
    lambda = 1
  )

  B <- coef(fit)

  expect_true(is.matrix(B))
  expect_equal(dim(B), c(10, 5))
  expect_identical(B, fit$coefficients)
})


test_that("NNP prediction method works", {

  set.seed(123)

  X <- matrix(rnorm(50 * 10), 50, 10)
  Y <- matrix(rnorm(50 * 4), 50, 4)

  fit <- nnp(
    X = X,
    Y = Y,
    lambda = 10
  )

  newx <- matrix(
    rnorm(20 * 10),
    20,
    10
  )

  pred <- predict(
    fit,
    newx = newx
  )

  expect_equal(
    dim(pred),
    c(20, 4)
  )

  expect_equal(
    pred,
    newx %*% fit$coefficients
  )
})


test_that("NNP plotting methods work", {

  set.seed(123)

  X <- matrix(rnorm(50 * 10), 50, 10)
  Y <- matrix(rnorm(50 * 4), 50, 4)

  fit <- nnp(
    X = X,
    Y = Y,
    nfold = 5,
    nlambda = 10,
    rule = "1se",
    seed = 123
  )

  expect_silent(
    plot(fit, type = "cv")
  )

  expect_silent(
    plot(fit, type = "rank")
  )

  expect_silent(
    plot(fit, type = "coef")
  )

  expect_silent(
    plot(fit, type = "all")
  )
})
