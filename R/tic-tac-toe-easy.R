# Tic-Tac-Toe Game in R with easy AI

# Function to print the board
print_board_easy <- function(board) {
  cat("\n")
  for (i in 1:3) {
    for (j in 1:3) {
      if (j == 3) {
        cat(board[i, j])
      } else {
        cat(board[i, j], "|")
      }
    }
    cat("\n")
    if (i != 3) {
      cat("---------\n")
    }
  }
  cat("\n")
}

# Function to check if there is a winner
check_winner_easy <- function(board) {
  for (i in 1:3) {
    # Check rows and columns
    if (all(board[i, ] == "X") || all(board[i, ] == "O")) return(TRUE)
    if (all(board[, i] == "X") || all(board[, i] == "O")) return(TRUE)
  }
  # Check diagonals
  if (all(diag(board) == "X") || all(diag(board) == "O")) return(TRUE)
  if (all(diag(t(apply(board, 2, rev))) == "X") || all(diag(t(apply(board, 2, rev))) == "O")) return(TRUE)
  return(FALSE)
}

# Function to check if the board is full
is_full_easy <- function(board) {
  return(all(board != " "))
}

# Function for AI move
ai_move_easy <- function(board) {
  empty_positions <- which(board == " ", arr.ind = TRUE)
  move <- empty_positions[sample(nrow(empty_positions), 1), ]
  return(move)
}

# Function to play the game
tic_tac_toe_easy <- function() {
  board <- matrix(rep(" ", 9), nrow = 3)
  player <- "X"
  ai <- "O"

  cat("Welcome to easy version of Tic-Tac-Toe!\n")
  print_board_easy(board)

  while (TRUE) {
    # Human player move
    cat("Player", player, "'s turn.\n")

    repeat {
      row <- suppressWarnings(as.integer(readline(prompt = "Enter row (1-3): ")))
      if (!is.na(row)) {
        break
      }
    }
    repeat {
      col <- suppressWarnings(as.integer(readline(prompt = "Enter column (1-3): ")))
      if (!is.na(col)) {
        break
      }
    }

    if (row < 1 || row > 3 || col < 1 || col > 3 || board[row, col] != " ") {
      cat("Invalid move. Try again.\n")
      next
    }

    board[row, col] <- player
    print_board_easy(board)

    if (check_winner_easy(board)) {
      cat("Player", player, "wins!\n")
      break
    }

    if (is_full_easy(board)) {
      cat("It's a tie!\n")
      break
    }

    # AI move
    cat("AI", ai, "'s turn.\n")
    ai_pos <- ai_move_easy(board)
    board[ai_pos[1], ai_pos[2]] <- ai
    print_board_easy(board)

    if (check_winner_easy(board)) {
      cat("AI", ai, "wins!\n")
      break
    }

    if (is_full_easy(board)) {
      cat("It's a tie!\n")
      break
    }
  }
}

