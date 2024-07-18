\name{clean_na_values}
\alias{clean_na_values}
\title{Clean NA Values from Data}
\author{Prashant Kumar Jha (\email{jha.prashant1106@gmail.com})}
\description{
  The \code{clean_na_values} function removes or imputes NA values in a data frame using various methods such as mean, median, mode, removal, zero, a specific value, or k-nearest neighbors (KNN).

}
\usage{
clean_na_values(data, method = c("mean", "median", "mode", "remove", "zero", "value", "knn"), value = NULL, k = 5)
}
\arguments{
  \item{data}{A data frame containing the data to be cleaned.}
  \item{method}{A character string specifying the method to use for imputing or removing NA values. Options are "mean", "median", "mode", "remove", "zero", "value", and "knn". Default is "mean".}
  \item{value}{A numeric value to replace NAs if \code{ method = "value" }. Default is NULL.}
  \item{k}{An integer specifying the number of nearest neighbors to use for KNN imputation. Default is 5.}
}
\details{
  The function offers multiple methods for handling missing values in a data frame:
  \itemize{
    \item{"mean"}{ replaces NAs with the mean of the respective column.}
    \item{"median"}{ replaces NAs with the median of the respective column.}
    \item{"mode"}{ replaces NAs with the mode of the respective column.}
    \item{"remove"}{ removes rows containing NAs.}
    \item{"zero"}{ replaces NAs with zero.}
    \item{"value"}{ replaces NAs with a user-specified value.}
    \item{"knn"}{ replaces NAs using k-nearest neighbors imputation.}
  }
}
\value{
  A data frame with NA values replaced or removed based on the specified method.
}
\examples{
# Sample data frame with NA values
data <- data.frame(
  A = c(1, 2, NA, 4, 5),
  B = c(NA, 2, 3, NA, 5),
  C = c(1, NA, NA, 4, 5)
)

# Impute missing values using the mean
cleaned_data_mean <- clean_na_values(data, method = "mean")

# Impute missing values using the median
cleaned_data_median <- clean_na_values(data, method = "median")

# Impute missing values using the mode
cleaned_data_mode <- clean_na_values(data, method = "mode")

# Remove rows with missing values
cleaned_data_remove <- clean_na_values(data, method = "remove")

# Replace missing values with zero
cleaned_data_zero <- clean_na_values(data, method = "zero")

# Replace missing values with a specific value
cleaned_data_value <- clean_na_values(data, method = "value", value = 100)

# Impute missing values using KNN
cleaned_data_knn <- clean_na_values(data, method = "knn", k = 3)
}

\references{
\url{https://www.geeksforgeeks.org/how-to-impute-missing-values-in-r/} \url{https://www.appsilon.com/post/imputation-in-r}
}
\keyword{manip}