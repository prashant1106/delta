\name{read_txt}
\alias{read_txt}
\title{Read Text Files into a Vector of Lines or Words}
\author{Prashant Kumar Jha (\email{jha.prashant1106@gmail.com})}
\description{
  The \code{read_txt} function reads the contents of a text file and returns a vector of lines or words.
}
\usage{
read_txt(file, mode = c("line", "word"), keep_punctuation = FALSE)
}
\arguments{
  \item{file}{A character string specifying the path to the text file to be read.}
  \item{mode}{A character string specifying whether to split the text into lines or words. Possible values are \code{"line"} and \code{"word"}. Default is \code{"line"}.}
  \item{keep_punctuation}{A logical value indicating whether to keep punctuation in the output. Default is \code{FALSE}.}
}
\details{
  This function reads the contents of a text file and returns a vector of either lines or words, depending on the specified \code{mode}. If \code{keep_punctuation} is \code{TRUE}, punctuation is preserved in the output. Otherwise, punctuation is removed.

  The function uses \code{suppressWarnings} to ignore warnings such as incomplete final lines in the file. The \code{trimws} function is used to trim whitespace from the resulting lines or words.
}
\value{
  A character vector containing the lines or words from the text file.
}
\examples{
# Example usage of read_txt function
\dontrun{
# Read a file into lines
lines <- read_txt("example.txt", mode = "line")
print(lines)

# Read a file into words, keeping punctuation
words_with_punctuation <- read_txt("example.txt", mode = "word", keep_punctuation = TRUE)
print(words_with_punctuation)

# Read a file into words, removing punctuation
words_without_punctuation <- read_txt("example.txt", mode = "word", keep_punctuation = FALSE)
print(words_without_punctuation)
}
}