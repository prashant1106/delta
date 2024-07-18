\name{generate_random_vector}
\alias{generate_random_vector}
\title{Generate Random Vector}
\author{Prashant Kumar Jha (\email{jha.prashant1106@gmail.com})}
\description{
  The \code{generate_random_vector} function generates a random vector of specified type and length.
}
\usage{
generate_random_vector(n, type = c("num", "decnum", "character", "logical", "name", "color", "date", "ip", "mobile_number", "email", "rdmemail"), min = 1, max = 10, start_date = "1970-01-01", end_date = as.Date(Sys.Date()), domain = NULL, replace = TRUE)
}
\arguments{
  \item{n}{A positive numeric value specifying the length of the vector.}
  \item{type}{A character string specifying the type of the vector. Options are "num", "decnum", "character", "logical", "name", "color", "date", "ip", "mobile_number", "email", and "rdmemail". Default is "num".}
  \item{min}{A numeric value specifying the minimum value for numeric data. Default is 1.}
  \item{max}{A numeric value specifying the maximum value for numeric data. Default is 10.}
  \item{start_date}{A character string specifying the start date for date data. Default is "1970-01-01".}
  \item{end_date}{A character string specifying the end date for date data. Default is your computer date defined by \code{as.Date(Sys.Date())}.}
  \item{domain}{A character string specifying the domain for email data. Default is NULL.}
  \item{replace}{A logical value indicating whether to sample with replacement. Default is TRUE.}
}
\details{
  \code{generate_random_vector} creates a random vector based on the specified type. Supported types include numeric, decimal numbers, character, logical, names, colors, dates, IP addresses, mobile numbers, emails, and random emails.

  For more information on generating random data, see the following references:
  \itemize{
    \item \url{https://www.geeksforgeeks.org/interquartile-range/}
    \item \url{https://www.digitalocean.com/community/tutorials/quantile-function-in-r}
  }
}
\value{
  A vector with random data based on the specified type and parameters.
}
\examples{
# Example usage of generate_random_vector function

# Generate a numeric vector of length 10 from 10 to 50 without repetation
generate_random_vector(10, type = "num", min = 10, max = 50, replace = FALSE)

# Generate a character vector of length 5
generate_random_vector(5, type = "character")

# Generate a date vector
generate_random_vector(7, type = "date", start_date = "2000-01-01", end_date = "2020-12-31")

# Generate a logical(TRUE/FALSE) vector of length 5
generate_random_vector(5, type = "logical")

# Generate a name vector of length 5
generate_random_vector(5, type = "name")

# Generate a email vector with domain cs50.harvard.edu of length 5
generate_random_vector(5, type = "email", domain = "cs50.harvard.edu")
}
\seealso{
\code{\link{generate_random_dataframe}}, \code{\link{generate_random_mobile_number}}, \code{\link{generate_random_ip}},
\code{\link{generate_random_email}},
\code{\link{generate_random_random_email}}
}