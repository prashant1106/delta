# Helper function to create a temporary file with given content
create_temp_file <- function(content) {
  temp_file <- tempfile()
  writeLines(content, temp_file)
  return(temp_file)
}

test_that("read_txt handles 'line' mode with and without punctuation", {
  content <- c("This is a sentence.", "And another one!", "Is this the third one?")
  temp_file <- create_temp_file(content)

  # Test 'line' mode with punctuation
  result <- read_txt(temp_file, mode = "line", keep_punctuation = TRUE)
  expect_equal(result, c("This is a sentence.", "And another one!", "Is this the third one?"))

  # Test 'line' mode without punctuation
  result <- read_txt(temp_file, mode = "line", keep_punctuation = FALSE)
  expect_equal(result, c("This is a sentence", "And another one", "Is this the third one"))

  unlink(temp_file) # Clean up
})

test_that("read_txt handles 'word' mode with and without punctuation", {
  content <- c("This is a sentence.", "And another one!", "Is this the third one?")
  temp_file <- create_temp_file(content)

  # Test 'word' mode with punctuation
  result <- read_txt(temp_file, mode = "word", keep_punctuation = TRUE)
  expect_equal(result, c("This", "is", "a", "sentence", ".", "And", "another", "one", "!", "Is", "this", "the", "third", "one", "?"))

  # Test 'word' mode without punctuation
  result <- read_txt(temp_file, mode = "word", keep_punctuation = FALSE)
  expect_equal(result, c("This", "is", "a", "sentence", "And", "another", "one", "Is", "this", "the", "third", "one"))

  unlink(temp_file) # Clean up
})

test_that("read_txt handles empty file", {
  temp_file <- create_temp_file("")

  # Test 'line' mode
  result <- read_txt(temp_file, mode = "line", keep_punctuation = TRUE)
  expect_equal(result, character(0))

  # Test 'word' mode
  result <- read_txt(temp_file, mode = "word", keep_punctuation = TRUE)
  expect_equal(result, character(0))

  unlink(temp_file) # Clean up
})

test_that("read_txt handles invalid mode", {
  content <- c("This is a sentence.", "And another one!")
  temp_file <- create_temp_file(content)

  expect_error(read_txt(temp_file, mode = "invalid"), "Invalid mode. Choose either 'line' or 'word'.")

  unlink(temp_file) # Clean up
})

test_that("read_txt handles different newline characters", {
  content <- c("This is a sentence.\r\nAnd another one!\nIs this the third one?\rThis is a new line.")
  temp_file <- create_temp_file(content)

  # Test 'line' mode with punctuation
  result <- read_txt(temp_file, mode = "line", keep_punctuation = TRUE)
  expect_equal(result, c("This is a sentence.", "And another one!", "Is this the third one?", "This is a new line."))

  unlink(temp_file) # Clean up
})

test_that("read_txt handles multiple punctuation in 'line' mode", {
  content <- c("Hello... How are you? I'm fine! Let's test.", "Another sentence.")
  temp_file <- create_temp_file(content)

  # Test 'line' mode with punctuation
  result <- read_txt(temp_file, mode = "line", keep_punctuation = TRUE)
  expect_equal(result, c("Hello.", ".", ".", "How are you?", "I'm fine!", "Let's test.", "Another sentence."))

  # Test 'line' mode without punctuation
  result <- read_txt(temp_file, mode = "line", keep_punctuation = FALSE)
  expect_equal(result, c("Hello", "How are you", "I'm fine", "Let's test", "Another sentence"))

  unlink(temp_file) # Clean up
})

test_that("read_txt handles multiple punctuation in 'word' mode", {
  content <- c("Hello... How are you? I'm fine! Let's test.", "Another sentence.")
  temp_file <- create_temp_file(content)

  # Test 'word' mode with punctuation
  result <- read_txt(temp_file, mode = "word", keep_punctuation = TRUE)
  expect_equal(result, c("Hello", ".", ".", ".", "How", "are", "you", "?", "I'm", "fine", "!", "Let's", "test", ".", "Another", "sentence", "."))

  # Test 'word' mode without punctuation
  result <- read_txt(temp_file, mode = "word", keep_punctuation = FALSE)
  expect_equal(result, c("Hello", "How", "are", "you", "I'm", "fine", "Let's", "test", "Another", "sentence"))

  unlink(temp_file) # Clean up
})