\name{tic_tac_toe_difficult}
\alias{tic_tac_toe_difficult}
\title{Difficult Version of Tic-Tac-Toe with Minimax AI}
\author{Prashant Kumar Jha (\email{jha.prashant1106@gmail.com})}
\description{
  If you feel bored, you can play this tic-tac-toe game against an AI opponent using the Minimax algorithm.
}
\usage{
tic_tac_toe_difficult()
}
\details{
 This function initiates a game of Tic-Tac-Toe where the human player is \code{"X"} and the AI player is \code{"O"}. The human player moves first. The AI uses the Minimax algorithm to make optimal moves, making it a challenging opponent.

  The game continues until either the human player or the AI wins, or the board is full resulting in a tie. The current state of the board is printed after each move.
}
\arguments{
None
}
\references{
Some ideas for implementing the Minimax algorithm were adapted from:
    \url{https://www.geeksforgeeks.org/finding-optimal-move-in-tic-tac-toe-using-minimax-algorithm-in-game-theory/}
}
\value{
  This function does not return a value. This function is used for playing the game interactively.
}
\examples{
# Play the Tic-Tac-Toe game
\dontrun{
tic_tac_toe_difficult()
}
}
\seealso{
Check other games:
\code{\link{guess_number}}, \code{\link{bulls_n_cows}}, \code{\link{tic_tac_toe_easy}}
}