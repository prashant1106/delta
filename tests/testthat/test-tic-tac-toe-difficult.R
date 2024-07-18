# As we can't check the whole game without playing it
# We will bec testing it different functions
test_that("check_winner function works correctly", {
  board <- matrix(c("X", "X", "X", " ", "O", " ", " ", " ", "O"), nrow = 3)
  expect_true(check_winner(board, "X"))
  expect_false(check_winner(board, "O"))

  board <- matrix(c("X", "O", "X", "O", "O", "X", "X", "O", "O"), nrow = 3)
  expect_true(check_winner(board, "O"))
  expect_false(check_winner(board, "X"))
})

test_that("is_full function works correctly", {
  board <- matrix(c("X", "X", "X", " ", "O", " ", " ", " ", "O"), nrow = 3)
  expect_false(is_full(board))

  board <- matrix(c("X", "O", "X", "O", "O", "X", "X", "O", "O"), nrow = 3)
  expect_true(is_full(board))
})

test_that("minimax function works correctly", {
  board <- matrix(c("X", "X", "X", " ", "O", " ", " ", " ", "O"), nrow = 3)
  expect_equal(minimax(board, 0, TRUE, "O", "X"), -10)
})

test_that("ai_move function works correctly", {
  board <- matrix(c("X", "O", "X", " ", "O", " ", " ", " ", " "), nrow = 3)
  move <- ai_move(board, "O", "X")
  expect_true(all(move == c(2, 3)))  # Expecting AI to choose the winning move
})