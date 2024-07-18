test_that("check_winner_easy function works correctly", {
  board <- matrix(c("X", "X", "X", " ", "O", " ", " ", " ", "O"), nrow = 3)
  expect_true(check_winner_easy(board))

  board <- matrix(c("X", "O", "X", "O", "O", "X", "X", "O", "O"), nrow = 3)
  expect_true(check_winner_easy(board))

  board <- matrix(c("X", "O", "X", "O", "O", " ", "X", "O", "X"), nrow = 3)
  expect_true(check_winner_easy(board))
})

test_that("is_full_easy function works correctly", {
  board <- matrix(c("X", "X", "X", " ", "O", " ", " ", " ", "O"), nrow = 3)
  expect_false(is_full_easy(board))

  board <- matrix(c("X", "O", "X", "O", "O", "X", "X", "O", "O"), nrow = 3)
  expect_true(is_full_easy(board))
})

test_that("ai_move_easy function works correctly", {
  board <- matrix(c("X", "O", "X", " ", "O", " ", " ", " ", " "), nrow = 3)
  move <- ai_move_easy(board)
  expect_true(board[move[1], move[2]] == " ")

  board <- matrix(c("X", "O", "X", "X", "O", "O", "O", "X", " "), nrow = 3)
  move <- ai_move_easy(board)
  expect_true(all(move == c(3, 3)))  # Expecting AI to choose the only remaining move
})