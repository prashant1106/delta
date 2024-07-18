\name{guess_number}
\alias{guess_number}
\title{Number Guessing Game in R}
\author{Prashant Kumar Jha (\email{jha.prashant1106@gmail.com})}
\description{
  The \code{guess_number} function implements a number guessing game where the player attempts to guess a randomly generated number between 1 and 100.
}
\usage{
guess_number()
}
\details{
  This function generates a random number between 1 and 100 and prompts the player to guess the number. The player receives feedback on whether their guess is too low, too high, or correct. The game continues until the player guesses the correct number, and the number of attempts is displayed at the end.
}
\arguments{None}
\value{
  This function does not return a value. This function is used for playing the game interactively.
}
\examples{
# Example usage of guess_number function
\dontrun{
guess_number()
}
}
\seealso{
Check other games:
\code{\link{bulls_n_cows}}, \code{\link{tic_tac_toe_difficult}}, \code{\link{tic_tac_toe_easy}}
}