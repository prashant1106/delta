\name{bulls_n_cows}
\alias{bulls_n_cows}
\title{Bulls and Cows Game}
\author{Prashant Kumar Jha (\email{jha.prashant1106@gmail.com})}
\description{
  A game where the player guesses a secret 4-digit number and receives feedback in the form of 'bulls' and 'cows' numbers for correct digits in and out of place respectively.
}
\usage{
bulls_n_cows()
}
\details{
The `bulls_n_cows` function initiates and manages the game. A secret 4-digit number is generated at the start, and the player is prompted to guess the number. For each guess, the number of bulls (correct digits in the correct position) and cows (correct digits in the wrong position) is calculated and displayed. The game continues until the player guesses the correct number.
}
\arguments{None}
\section{Functions}{
  \describe{
    \item{\code{generate_number}}{Generates a random 4-digit number as a string.}
    \item{\code{calculate_bulls_cows(secret, guess)}}{Calculates the number of bulls and cows for a given guess.}
  }
}
\value{
  This function does not return a value. This function is used for playing the game interactively.
}
\examples{
# Example usage of bulls_n_cows function
# Note: This function requires interactive user input and is meant to be run in an interactive R session.

\dontrun{
bulls_n_cows()
}
}
\seealso{
Check other games:
\code{\link{guess_number}}, \code{\link{tic_tac_toe_difficult}}, \code{\link{tic_tac_toe_easy}}
}
\keyword{games}