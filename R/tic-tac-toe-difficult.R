# Tic-Tac-Toe Game in R with Minimax AI
# Some ideas adapted from
# https://www.geeksforgeeks.org/finding-optimal-move-in-tic-tac-toe-using-minimax-algorithm-in-game-theory/

# Function to print the board
print_board <- function(board) {
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
check_winner <- function(board, player) {
  for (i in 1:3) {
    # Check rows and columns
    if (all(board[i, ] == player) || all(board[, i] == player)) return(TRUE)
  }
  # Check diagonals
  if (all(diag(board) == player) || all(diag(t(apply(board, 2, rev))) == player)) return(TRUE)
  return(FALSE)
}

# Function to check if the board is full
is_full <- function(board) {
  return(all(board != " "))
}

# Minimax function for the AI
minimax <- function(board, depth, is_maximizing, ai, player) {
  if (check_winner(board, ai)) {
    return(10 - depth)
  } else if (check_winner(board, player)) {
    return(depth - 10)
  } else if (is_full(board)) {
    return(0)
  }

  if (is_maximizing) {
    best_score <- -Inf
    for (i in 1:3) {
      for (j in 1:3) {
        if (board[i, j] == " ") {
          board[i, j] <- ai
          score <- minimax(board, depth + 1, FALSE, ai, player)
          board[i, j] <- " "
          best_score <- max(best_score, score)
        }
      }
    }
    return(best_score)
  } else {
    best_score <- Inf
    for (i in 1:3) {
      for (j in 1:3) {
        if (board[i, j] == " ") {
          board[i, j] <- player
          score <- minimax(board, depth + 1, TRUE, ai, player)
          board[i, j] <- " "
          best_score <- min(best_score, score)
        }
      }
    }
    return(best_score)
  }
}

# Function for AI move
ai_move <- function(board, ai, player) {
  best_score <- -Inf
  best_move <- NULL
  for (i in 1:3) {
    for (j in 1:3) {
      if (board[i, j] == " ") {
        board[i, j] <- ai
        score <- minimax(board, 0, FALSE, ai, player)
        board[i, j] <- " "
        if (score > best_score) {
          best_score <- score
          best_move <- c(i, j)
        }
      }
    }
  }
  return(best_move)
}

# Function to play the game
tic_tac_toe_difficult <- function() {
  board <- matrix(rep(" ", 9), nrow = 3)
  player <- "X"
  ai <- "O"

  cat("Welcome to difficult version Tic-Tac-Toe!\n")
  print_board(board)

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
    print_board(board)

    if (check_winner(board, player)) {
      cat("Player", player, "wins!\n")
      break
    }

    if (is_full(board)) {
      cat("It's a tie!\n")
      break
    }

    # AI move
    cat("AI", ai, "'s turn.\n")
    ai_pos <- ai_move(board, ai, player)
    board[ai_pos[1], ai_pos[2]] <- ai
    print_board(board)

    if (check_winner(board, ai)) {
      cat("AI", ai, "wins!\n")
      break
    }

    if (is_full(board)) {
      cat("It's a tie!\n")
      break
    }
  }
}

