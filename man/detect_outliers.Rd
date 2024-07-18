\name{detect_outliers}
\alias{detect_outliers}
\title{Detect Outliers in Numeric Data}
\author{Prashant Kumar Jha (\email{jha.prashant1106@gmail.com})}
\description{
  The \code{detect_outliers} function detects outliers in a numeric vector or in numeric columns of a data frame using the Interquartile Range (IQR) method. Optionally, it can remove the detected outliers.
}
\usage{
detect_outliers(data, threshold = 1.5, remove = FALSE)
}
\arguments{
  \item{data}{A numeric vector or a data frame containing numeric columns to be checked for outliers.}
  \item{threshold}{A numeric value specifying the threshold for defining outliers based on the IQR. Default is 1.5.}
  \item{remove}{A logical value indicating whether to remove the outliers. Default is FALSE.}
}
\details{
  \code{detect_outliers} uses the IQR method to detect outliers in numeric data:
  \itemize{
    \item \code{Q1}: 25th percentile
    \item \code{Q3}: 75th percentile
    \item \code{IQR}: Interquartile Range (Q3 - Q1)
    \item \code{Lower Bound}: Q1 - threshold * IQR
    \item \code{Upper Bound}: Q3 + threshold * IQR
  }
  Values outside the lower and upper bounds are considered outliers.

  This function does not recognize NA values as outliers.

}
\references{
    \url{https://www.geeksforgeeks.org/interquartile-range/}
    \url{https://www.digitalocean.com/community/tutorials/quantile-function-in-r}
}
\value{
  The function returns a data frame or a numeric vector with outliers either marked or removed based on the input parameters.
}
\examples{
# Example usage of detect_outliers function

# Detect outliers in a numeric vector
numeric_vector <- c(1, 2, 3, 4, 5, 100, 101)
detect_outliers(numeric_vector)

# Remove outliers in a numeric vector
detect_outliers(numeric_vector, remove = TRUE)

# Detect outliers in a data frame
data_frame <- data.frame(
  A = c(1, 2, 3, 4, 5, 100, 101),
  B = c(10, 20, 30, 40, 50, 1000, 1010))
  C = c(100, 200, 300, 400, 500, 10000, 10101)
detect_outliers(data_frame)

# Remove outliers in a data frame
detect_outliers(data_frame, remove = TRUE)
}
\keyword{manip}