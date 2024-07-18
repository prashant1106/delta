test_data <- data.frame(
  A = c(1, 2, NA, 4, 5),
  B = c(NA, 2, 3, NA, 5),
  C = c(1, NA, NA, 4, 5)
)

# Test for mean imputation
test_that("NA values are replaced with mean", {
  result <- clean_na_values(test_data, method = "mean")
  expect_equal(result$A[3], mean(test_data$A, na.rm = TRUE))
  expect_equal(result$B[1], mean(test_data$B, na.rm = TRUE))
})

# Test for median imputation
test_that("NA values are replaced with median", {
  result <- clean_na_values(test_data, method = "median")
  expect_equal(result$A[3], median(test_data$A, na.rm = TRUE))
  expect_equal(result$B[1], median(test_data$B, na.rm = TRUE))
})

# Test for mode imputation
test_that("NA values are replaced with mode", {
  result <- clean_na_values(test_data, method = "mode")
  mode_function <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  expect_equal(result$A[3], mode_function(test_data$A))
  expect_equal(result$B[1], mode_function(test_data$B))
})

# Test for removal of rows with NA values
test_that("Rows with NA values are removed", {
  result <- clean_na_values(test_data, method = "remove")
  expect_equal(nrow(result), 1)
  expect_false(anyNA(result))
})

# Test for zero imputation
test_that("NA values are replaced with zero", {
  result <- clean_na_values(test_data, method = "zero")
  expect_equal(result$A[3], 0)
  expect_equal(result$B[1], 0)
})

# Test for specific value imputation
test_that("NA values are replaced with specific value", {
  result <- clean_na_values(test_data, method = "value", value = 100)
  expect_equal(result$A[3], 100)
  expect_equal(result$B[1], 100)
})

# Test for KNN imputation
test_that("NA values are replaced with KNN imputation", {
  result <- clean_na_values(test_data, method = "knn", k = 3)
  expect_false(anyNA(result))
})