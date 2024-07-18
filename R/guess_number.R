# Number Guessing Game in R

# Function to play the game
guess_number <- function() {
  # Generate a random number between 1 and 100
  target_number <- sample(1:100, 1)

  # Intro
  cat("Welcome to the Number Guessing Game!\n")
  cat("I'm thinking of a number between 1 and 100.\n")

  # Variables defined
  guess <- NULL
  attempts <- 0

  # A while loop to keep game running until the guessed number = target number
  while (is.null(guess) || guess != target_number) {
    # User can guess any number
    guess <- as.integer(readline(prompt = "Enter your guess: "))
    # Increase the count of attempt
    attempts <- attempts + 1

    # Cheks if the ipuut is higher, lower or equal to target number and print the output
    if (guess < target_number) {
      cat("Too low! Try again.\n")
    } else if (guess > target_number) {
      cat("Too high! Try again.\n")
    } else {
      cat("Congratulations! You guessed the correct number!\n")
      cat("It took you", attempts, "attempts.\n")
    }
  }
}
