# Generate random data frames and vectors

# Generate random data frame
generate_random_dataframe <- function(n_rows, n_cols, types = rep("numeric", n_cols), min = 1, max = 10, start_date = "1970-01-01", end_date = as.Date(Sys.Date()),  domain = NULL, replace = TRUE) {
  if (!is.numeric(n_rows) || n_rows <= 0) stop("n_rows must be a positive numeric value")
  if (!is.numeric(n_cols) || n_cols <= 0) stop("n_cols must be a positive numeric value")
  if (length(types) != n_cols) stop("types length must equal n_cols")

  df <- data.frame(matrix(nrow = n_rows, ncol = n_cols))
  for (i in 1:n_cols) {
    df[[i]] <- generate_random_vector(n_rows, type = types[i], min = min, max = max, start_date = as.Date(start_date), end_date = as.Date(end_date),  domain = domain, replace = replace)
  }
  colnames(df) <- paste0("v", 1:n_cols)
  return(df)
}

# Generate random vectors
generate_random_vector <- function(n, type = c("num", "decnum", "character", "logical", "name", "color", "date", "ip", "mobile_number", "email", "rdmemail"), min = 1, max = 10, start_date = "1970-01-01", end_date = as.Date(Sys.Date()),  domain = NULL, replace = TRUE) {
  type <- match.arg(type)
  if (!is.numeric(n) || n <= 0) stop("n must be a positive numeric value")
  if (min == max) warning("min is eqal to max")

  start_date <- tryCatch(as.Date(start_date), error = function(e) stop("Invalid start_date"))
  end_date <- tryCatch(as.Date(end_date), error = function(e) stop("Invalid end_date"))

  switch(type,
         num = sample(min:max, n, replace = replace),
         decnum = runif(n, min = min, max = max),
         character = replicate(n, paste0(sample(LETTERS, 5, replace = replace), collapse = "")),
         logical = sample(c(TRUE, FALSE), n, replace = replace),
         name = gsub(',', '', randomNames(n, name.order="first.last", ethnicity = sample(1:6, n, replace = TRUE))),
         color = sample(colors(), n, replace = replace),
         date = as.Date(sample(seq.Date(start_date, end_date, by = "day"), n, replace = replace)),
         ip = generate_random_ip(n),
         mobile_number = replicate(n, generate_random_mobile_number()),
         email = replicate(n, generate_random_email(domain = domain)),
         rdmemail = replicate(n, generate_random_random_email())
  )
}

# Generate random mobile number
generate_random_mobile_number <- function() {
  country_codes <- c("+1", "+44", "+49", "+33", "+91")
  country_code <- sample(country_codes, 1)
  mobile_no <- sample(1000000000:9999999999, 1)
  return(paste(country_code, mobile_no, sep = "-"))
}

# Generate random ip
generate_random_ip <- function(n) {
  return(paste0(sample(1:255, size = n, replace = TRUE), ".",
                sample(1:255, size = n, replace = TRUE), ".",
                sample(1:255, size = n, replace = TRUE), ".",
                sample(1:255, size = n, replace = TRUE), sep = ""))
}

# Generate random email of format firstname.lastname@companyname.com
generate_random_email <- function(domain = NULL) {
  name <- gsub(',', '', randomNames(1))
  # Ensure the name is in lowercase and spaces are replaced with dots
  name <- tolower(gsub(" ", ".", name))

  # If domain is not provided, select a random one
  if (is.null(domain)) {
    domains <- c("gmail.com", "outlook.com", "example.com", "test.com", "mail.com", "domain.com")
    domain <- sample(domains, 1)
  }

  # Create the email address
  email <- paste0(name, "@", domain)

  return(email)
}

# Generate completely ranmdom email
generate_random_random_email <- function() {
  first_part <- paste0(sample(letters, size = sample(1:10, size = 1)), collapse = "")
  second_part <- paste0(sample(letters, size = sample(1:10, size = 1)), collapse = "")
  third_part <- paste0(sample(letters, size = 3), collapse = "")
  return(paste0(first_part, "@", second_part, ".", third_part))
}
