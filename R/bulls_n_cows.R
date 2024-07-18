# Bulls and Cows Game in R

# Function to generate a random 4-digit number as a string
generate_number <- function() {
  return(sprintf("%04d", sample(1000:9999, 1)))
}

# Function to calculate bulls and cows
calculate_bulls_cows <- function(secret, guess) {
  secret_digits <- strsplit(secret, "")[[1]]
  guess_digits <- strsplit(guess, "")[[1]]

  bulls <- sum(secret_digits == guess_digits)

  # Count cows
  cows <- 0
  for (digit in unique(guess_digits)) {
    cows <- cows + min(sum(secret_digits == digit), sum(guess_digits == digit))
  }

  cows <- cows - bulls  # Subtract bulls from cows count
  return(list(bulls = bulls, cows = cows))
}

# Function to play the game
bulls_n_cows <- function() {
  secret <- generate_number()
 cat("Secret number (for testing purposes):", secret, "\n")
  attempts <- 0

  cat(paste0("\033[0;", 33, "m","Welcome to Bulls and Cows!","\033[0m\n"))
  cat("I have guessed a 4 digit number, and you have to guess the number.\nFor each guess, the number of bulls (correct digits in the correct position) and cows (correct digits in the wrong position) will be displayed.\n")
  cat(paste0("\033[0;", 31, "m", "The game continues until you guess the correct number.","\033[0m\n"))
  cat(paste0("\033[0;", 36, "m", "Guess the 4-digit number.","\033[0m\n"))

  while (TRUE) {
    guess <- readline(prompt = "Enter your guess: ")

    if (nchar(guess) != 4 || !grepl("^[0-9]+$", guess)) {
      cat("Invalid input. Please enter a 4-digit number.\n")
      next
    }

    attempts <- attempts + 1
    result <- calculate_bulls_cows(secret, guess)
    cat("Bulls:", result$bulls, "Cows:", result$cows, "\n")

    if (result$bulls == 4) {
      cat(paste0("\033[0;", 36, "m","Congratulations! You've guessed the number in ", attempts, " attempts.","\033[0m\n"))
      break
    }
  }
}
