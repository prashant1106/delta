\name{tic_tac_toe_easy}
\alias{tic_tac_toe_easy}
\title{Play an Easy Version of Tic-Tac-Toe}
\author{Prashant Kumar Jha (\email{jha.prashant1106@gmail.com})}
\description{
  The \code{tic_tac_toe_easy} function allows the user to play a game of Tic-Tac-Toe against an easy AI.
}
\usage{
tic_tac_toe_easy()
}
\arguments{None}
\details{
  This function initiates a game of Tic-Tac-Toe where the human player is \code{"X"} and the AI player is \code{"O"}. The human player moves first. The AI makes random moves, making it an easy opponent.

  The game continues until either the human player or the AI wins, or the board is full resulting in a tie. The current state of the board is printed after each move.
}
\value{
  This function does not return a value. This function is used for playing the game interactively.
}
\examples{
# Play the easy version of the Tic-Tac-Toe game
\dontrun{
tic_tac_toe_easy()
}
}
\seealso{
Check other games:
\code{\link{guess_number}}, \code{\link{bulls_n_cows}}, \code{\link{tic_tac_toe_difficult}}
}