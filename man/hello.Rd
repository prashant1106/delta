\name{hello}
\alias{hello}
\title{Hello Function}
\author{Prashant Kumar Jha (\email{jha.prashant1106@gmail.com})}
\description{
  The \code{hello} function prints a customized greeting message introducing the author of the package and providing information about the package.
}
\usage{
hello(name = "user")
}
\arguments{
  \item{name}{A character string specifying the name of the user. Default is \code{"user"}.}
}
\details{
  This function prints a greeting message in color that introduces the author of the package and provides a brief description of the package's capabilities. It also includes a link to the author's Instagram profile for further contact.
}
\value{
  This function does not return a value. It prints a message to the console.
}
\examples{
# Example usage of hello function

hello()
hello(name = "CS50")
}
