\name{generate_random_ip}
\alias{generate_random_ip}
\title{Generate Random IP Address}
\author{Prashant Kumar Jha (\email{jha.prashant1106@gmail.com})}
\description{
  The \code{generate_random_ip} function generates a specified number of random IP addresses.
}
\usage{
generate_random_ip(n)
}
\arguments{
  \item{n}{A positive numeric value specifying the number of IP addresses to generate.}
}
\details{
  This function generates random IP addresses by sampling four numbers between 1 and 255 and concatenating them with dots.
}

\seealso{
\code{\link{generate_random_dataframe}},
\code{\link{generate_random_vector}}, \code{\link{generate_random_mobile_number}},
\code{\link{generate_random_email}},
\code{\link{generate_random_random_email}}
}
\value{
  A character vector of random IP addresses.
}
\examples{
# Example usage of generate_random_ip function

generate_random_ip(5)
}