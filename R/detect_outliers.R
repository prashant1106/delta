# Detect the outliers in a numeric vector
# Uses Interquartile range method to find the outliers
# https://www.geeksforgeeks.org/interquartile-range/
# https://www.digitalocean.com/community/tutorials/quantile-function-in-r
# It does not recognise NA as outliers

detect_outliers <- function(data, threshold = 1.5, remove = FALSE) {
  # Check if data is a data frame, tibble, or numeric vector
  if (!is.data.frame(data) && !is.numeric(data)) {
    stop("The input data must be a data frame, tibble, or numeric vector.")
  }

  # Check if threshold is numeric
  if (!is.numeric(threshold) || length(threshold) != 1) {
    stop("Threshold must be a single numeric value.")
  }

  # Checks the outliers in the vector
  outlier_function <- function(x) {
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - threshold * IQR
    upper_bound <- Q3 + threshold * IQR
    is_outlier <- x < lower_bound | x > upper_bound
    is_outlier[is.na(x)] <- FALSE
    return(is_outlier)
  }

  # Send formatted vectors to the outlier function and then format the output according to the need
  if (is.data.frame(data)) {
    # Check outliers for every numeric column of dataframe
    data_outliers <- data |> mutate(across(where(is.numeric), outlier_function))

    # Either remove data or add column of outliers
   if (remove) {
      logical_df <- data_outliers |> select_if(is.logical)
      data <- data |> filter(!rowSums(logical_df))
    } else {
      logical_df <- data_outliers |> select_if(is.logical)
      data <- data |> mutate(outliers = rowSums(logical_df))
    }
  } else if (is.numeric(data)) {
    # For vector it send the data to function without formatting
    data_outliers <- outlier_function(data)

    if (remove) {
      data <- data[!data_outliers]
    } else {
      data <- data.frame(value = data, outliers = data_outliers)
    }
  }

  return(data)
}
