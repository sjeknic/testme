# Smoke tests
test_that("Finds existing item", {
  list <- 1:4
  expect_equal(binary_search(list, 1), 1)
})

test_that("Doesn't find non-existing item", {
  list <- 1:4
  expect_equal(binary_search(list, 5), 0)
})


# Boundary tests
# These should fail
test_that("Fails if NA passed", {
  list <- 1:4
  expect_equal(binary_search(list, NA), 0)
})

test_that("Fails if NA close to x", {
  list <- c(NA, 1, 2, 3, 4)
  expect_equal(binary_search(list, 1), 2)
})

# These should pass
test_that("Passes if NA after x", {
  list <- c(1, 2, 3, 4, NA)
  expect_equal(binary_search(list, 4), 4)
})

test_that("Passes if NA before and far from x", {
  list <- c(NA, 1, 2, 3, 4)
  expect_equal(binary_search(list, 4), 5)
})

test_that("Works with long lists", {
  list <- 1:1000000
  expect_equal(binary_search(list, 1000000), 1000000)
})

test_that("Works with short lists", {
  list <- 1:2
  expect_equal(binary_search(list, 2), 2)
})