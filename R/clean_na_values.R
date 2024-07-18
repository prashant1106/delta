# Removes NA values from the data
# https://www.geeksforgeeks.org/how-to-impute-missing-values-in-r/
# https://www.appsilon.com/post/imputation-in-r

clean_na_values <- function(data, method = c("mean", "median", "mode", "remove", "zero", "value", "knn"), value = NULL, k = 5) {
  method <- match.arg(method)
  if (method == "mean") {
    # If method = mean then replace all the NAs with the mean value
    data <- data |>
      mutate_if(is.numeric, ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))

  } else if (method == "median") {
    # If method = median the replace all the NA with the median
    data <- data |>
      mutate_if(is.numeric, ~ ifelse(is.na(.), median(., na.rm = TRUE), .))

  } else if (method == "mode") {
    # If method = mode the replace all the NA with the mode
    mode_function <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }
    data <- data |>
      mutate_if(is.numeric, ~ ifelse(is.na(.), mode_function(.), .))

  } else if (method == "remove") {
    # Remove the NA from the vector if mode = remove
    data <- na.omit(data)

  } else if (method == "zero") {
    # Replace the Na with 0
    data <- data |>
      mutate_if(is.numeric, ~ ifelse(is.na(.), 0, .))

  } else if (method == "value") {
    # Replace the Na with a specific value
    if (is.null(value)) stop("Please provide a value to replace missing values")
    data <- data |>
      mutate_if(is.numeric, ~ ifelse(is.na(.), value, .))

  } else if (method == "knn") {
    # Replace the NA with the value calculated by KNN
    # Dangerous
    for (i in 1:ncol(data)) {
      if (anyNA(data[, i])) {
        data[, i] <- knn_impute(data[, i], k = k)
      }
    }
  } else{
    stop("Method not defined!")
  }
  return(data)
}

# Knn impute method defined
knn_impute <- function(x, k = 5) {
  missing_indices <- which(is.na(x))
  non_missing_indices <- which(!is.na(x))

  for (j in missing_indices) {
    # Find distances to non-missing values
    distances <- sqrt((x[j] - x[non_missing_indices])^2)

    # Find K-nearest neighbors
    nearest_neighbors <- order(distances)[1:k]

    # Impute missing value as the mean of nearest neighbors
    x[j] <- mean(x[non_missing_indices[nearest_neighbors]])
  }
  return(x)
}
