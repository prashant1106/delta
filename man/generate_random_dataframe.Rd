\name{generate_random_dataframe}
\alias{generate_random_dataframe}
\title{Generate Random Data Frame}
\author{Prashant Kumar Jha (\email{jha.prashant1106@gmail.com})}
\description{
  The \code{generate_random_dataframe} function generates a data frame with random data of specified types.
}
\usage{
generate_random_dataframe(n_rows, n_cols, types = rep("numeric", n_cols), min = 1, max = 10, start_date = "1970-01-01", end_date = as.Date(Sys.Date()), domain = NULL, replace = TRUE)
}
\arguments{
  \item{n_rows}{A positive numeric value specifying the number of rows in the data frame.}
  \item{n_cols}{A positive numeric value specifying the number of columns in the data frame.}
  \item{types}{A character vector specifying the type of each column. Default is \code{rep("numeric", n_cols)}.}
  \item{min}{A numeric value specifying the minimum value for numeric data. Default is 1.}
  \item{max}{A numeric value specifying the maximum value for numeric data. Default is 10.}
  \item{start_date}{A character string specifying the start date for date data. Default is "1970-01-01".}
  \item{end_date}{A character string specifying the end date for date data. Default is your computer date defined by \code{as.Date(Sys.Date())}".}
  \item{domain}{A character string specifying the domain for email data. Default is NULL.}
  \item{replace}{A logical value indicating whether to sample with replacement. Default is TRUE.}
}
\details{
  \code{generate_random_dataframe} creates a data frame with random values based on the specified types. Supported types include numeric, decimal numbers, character, logical, names, colors, dates, IP addresses, mobile numbers, emails, and random emails.
}

\seealso{
\code{\link{generate_random_vector}}, \code{\link{generate_random_mobile_number}}, \code{\link{generate_random_ip}},
\code{\link{generate_random_email}},
\code{\link{generate_random_random_email}}
}

\value{
  A data frame with random data based on the specified types and parameters.
}

\examples{
# Example usage of generate_random_dataframe function

# Generate a data frame with 10 rows and 3 columns
generate_random_dataframe(10, 3, types = c("num", "character", "date"))

# Generate a data frame with specified min and max values for numeric data
generate_random_dataframe(10, 3, types = c("num", "decnum", "logical"), min = 5, max = 15)
}