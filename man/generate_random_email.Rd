\name{generate_random_email}
\alias{generate_random_email}
\title{Generate Random Email Address}
\author{Prashant Kumar Jha (\email{jha.prashant1106@gmail.com})}
\description{
  The \code{generate_random_email} function generates a random email address in the format \code{firstname.lastname@domain.com}.
}
\usage{
generate_random_email(domain = NULL)
}
\arguments{
  \item{domain}{A character string specifying the domain for the email address. If \code{NULL}, a random domain is selected from a predefined list. Default is \code{NULL}.}
}
\details{
  This function generates a random email address with a format \code{firstname.lastname@domain.com}. If the domain is not provided, a random one is selected from a predefined list.
}
\seealso{
\code{\link{generate_random_dataframe}},
\code{\link{generate_random_vector}}, \code{\link{generate_random_ip}},
\code{\link{generate_random_mobile_number}},
\code{\link{generate_random_random_email}}
}
\value{
  A character string representing a random email address.
}
\examples{
# Example usage of generate_random_email function

generate_random_email()

# Generate an email with a specified domain
generate_random_email(domain = "customdomain.com")
}