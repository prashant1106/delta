test_that("generate_number generates a 4-digit number as a string", {
  num <- generate_number()
  expect_true(grepl("^\\d{4}$", num))
})

test_that("calculate_bulls_cows calculates bulls and cows correctly", {
  result <- calculate_bulls_cows("1234", "1234")
  expect_equal(result$bulls, 4)
  expect_equal(result$cows, 0)

  result <- calculate_bulls_cows("1234", "4321")
  expect_equal(result$bulls, 0)
  expect_equal(result$cows, 4)

  result <- calculate_bulls_cows("1234", "2143")
  expect_equal(result$bulls, 0)
  expect_equal(result$cows, 4)

  result <- calculate_bulls_cows("1234", "1243")
  expect_equal(result$bulls, 2)
  expect_equal(result$cows, 2)

  result <- calculate_bulls_cows("1234", "1111")
  expect_equal(result$bulls, 1)
  expect_equal(result$cows, 0)

  result <- calculate_bulls_cows("1122", "2211")
  expect_equal(result$bulls, 0)
  expect_equal(result$cows, 4)

  result <- calculate_bulls_cows("1234", "5678")
  expect_equal(result$bulls, 0)
  expect_equal(result$cows, 0)

  result <- calculate_bulls_cows("5555", "5555")
  expect_equal(result$bulls, 4)
  expect_equal(result$cows, 0)

  result <- calculate_bulls_cows("5555", "5588")
  expect_equal(result$bulls, 2)
  expect_equal(result$cows, 0)

  result <- calculate_bulls_cows("5555", "5858")
  expect_equal(result$bulls, 2)
  expect_equal(result$cows, 0)
})