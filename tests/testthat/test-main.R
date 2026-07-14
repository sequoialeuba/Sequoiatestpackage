test_that("power_detect returns correct known values", {
  # p = 0 means no chance of success ever, regardless of n
  expect_equal(power_detect(n = 10, p = 0), 0)
  
  # p = 1 means certain success on first trial
  expect_equal(power_detect(n = 1, p = 1), 1)
  expect_equal(power_detect(n = 100, p = 1), 1)
  
  # n = 0 trials means no chance of detection, regardless of p
  expect_equal(power_detect(n = 0, p = 0.5), 0)
  
  # Manually verified case: 1 - (1-0.1)^10
  expect_equal(power_detect(n = 10, p = 0.1), 1 - 0.9^10)
})

test_that("power_detect increases with n", {
  p <- 0.1
  probs <- sapply(1:10, function(n) power_detect(n, p))
  expect_true(all(diff(probs) > 0))
})

test_that("power_detect increases with p", {
  n <- 10
  probs <- sapply(seq(0.1, 0.9, by = 0.1), function(p) power_detect(n, p))
  expect_true(all(diff(probs) > 0))
})

test_that("power_detect output is bounded between 0 and 1", {
  set.seed(123)
  ns <- sample(1:100, 20)
  ps <- runif(20, 0, 1)
  results <- mapply(power_detect, ns, ps)
  expect_true(all(results >= 0 & results <= 1))
})

test_that("power_detect is vectorized over n and p", {
  result <- power_detect(n = c(5, 10, 20), p = 0.1)
  expected <- 1 - (1 - 0.1)^c(5, 10, 20)
  expect_equal(result, expected)
  
  result2 <- power_detect(n = 10, p = c(0.05, 0.1, 0.2))
  expected2 <- 1 - (1 - c(0.05, 0.1, 0.2))^10
  expect_equal(result2, expected2)
})

test_that("power_detect handles edge case p = 1 with large n", {
  expect_equal(power_detect(n = 1000, p = 1), 1)
})

test_that("power_detect returns numeric type", {
  expect_type(power_detect(10, 0.1), "double")
})

test_that("power detect rejects invalid inputs", {
  expect_error(power_detect(1, -2))
})