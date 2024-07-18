test_that("detect_outliers handles different data types", {
  # Test for numeric vector
  vec <- c(1, 2, 3, 4, 100)
  result <- detect_outliers(vec)
  expect_equal(result$outliers, c(FALSE, FALSE, FALSE, FALSE, TRUE))

  # Test for data frame
  df <- data.frame(a = c(1, 2, 3, 4, 100), b = c(5, 6, 7, 8, 200))
  result <- detect_outliers(df)
  expect_equal(result$outliers, c(0, 0, 0, 0, 2))

  # Test for removing outliers in vector
  result <- detect_outliers(vec, remove = TRUE)
  expect_equal(result, c(1, 2, 3, 4))

  # Test for removing outliers in data frame
  result <- detect_outliers(df, remove = TRUE)
  expect_equal(result, df[1:4, ])
})

test_that("detect_outliers handles threshold parameter correctly", {
  vec <- c(1, 2, 3, 4, 100)
  result <- detect_outliers(vec, threshold = 3)
  expect_equal(result$outliers, c(FALSE, FALSE, FALSE, FALSE, TRUE))
})

test_that("detect_outliers handles invalid input", {
  # Test for non-numeric vector
  expect_error(detect_outliers(c("a", "b", "c")), "must be a data frame, tibble, or numeric vector")

  # Test for invalid threshold
  expect_error(detect_outliers(c(1, 2, 3), threshold = "a"), "Threshold must be a single numeric value")

  # Test for non-data frame input
  expect_error(detect_outliers(list(a = 1, b = 2)), "must be a data frame, tibble, or numeric vector")
})

test_that("detect_outliers handles edge cases", {
  # Test for empty numeric vector
  vec <- numeric(0)
  result <- detect_outliers(vec)
  expect_equal(result, data.frame(value = numeric(0), outliers = logical(0)))

  # Test for data frame with NA values
  df <- data.frame(a = c(1, 2, NA, 4, 100))
  result <- detect_outliers(df)
  expect_equal(result$outliers, c(0, 0, 0, 0, 1))

  # Test for numeric vector with NA values
  vec <- c(1, 2, NA, 4, 100)
  result <- detect_outliers(vec)
  expect_equal(result$outliers, c(FALSE, FALSE, FALSE, FALSE, TRUE))
})

test_that("detect_outliers does not add 'outliers' column when remove is TRUE", {
  # Test for data frame
  df <- data.frame(a = c(1, 2, 3, 4, 100), b = c(5, 6, 7, 8, 200))
  result <- detect_outliers(df, remove = TRUE)
  expect_false("outliers" %in% colnames(result))

  # Test for numeric vector
  vec <- c(1, 2, 3, 4, 100)
  result <- detect_outliers(vec, remove = TRUE)
  expect_equal(names(result), NULL) # When removing outliers from a vector, it should remain a vector
})
