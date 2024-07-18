test_that("generate_random_dataframe produces a dataframe with the correct dimensions", {
  df <- generate_random_dataframe(10, 5, type = c("num", "character", "logical", "name", "color"))
  expect_equal(nrow(df), 10)
  expect_equal(ncol(df), 5)
})

test_that("generate_random_dataframe handles invalid inputs", {
  expect_error(generate_random_dataframe(-1, 5))
  expect_error(generate_random_dataframe(10, 0))
  expect_error(generate_random_dataframe(10, 5, types = rep("numeric", 4)))
})

# As generate_random_dataframe function is based on generate_random_vector function, it will check boththe function
test_that("generate_random_vector generates correct type of data", {
  vec_num <- generate_random_vector(10, "num", min = 1, max = 10)
  expect_type(vec_num, "integer")
  expect_true(all(vec_num >= 1 & vec_num <= 10))

  vec_decnum <- generate_random_vector(10, "decnum", min = 1, max = 10)
  expect_type(vec_decnum, "double")
  expect_true(all(vec_decnum >= 1 & vec_decnum <= 10))

  vec_char <- generate_random_vector(10, "character")
  expect_type(vec_char, "character")
  expect_true(all(nchar(vec_char) == 5))

  expect_type(generate_random_vector(10, "logical"), "logical")

  expect_type(generate_random_vector(10, "name"), "character")

  expect_type(generate_random_vector(10, "color"), "character")

  vec_date <- generate_random_vector(10, "date", start_date = "2020-01-01", end_date = "2020-12-31")
  expect_type(vec_date, "double")
  expect_true(all(vec_date >= as.Date("2020-01-01") & vec_date <= as.Date("2020-12-31")))

  vec_ip <- generate_random_vector(10, "ip")
  expect_type(vec_ip, "character")
  expect_true(all(grepl("^([0-9]{1,3}\\.){3}[0-9]{1,3}$", vec_ip)))

  vec_mobile <- generate_random_vector(10, "mobile_number")
  expect_type(vec_mobile, "character")
  expect_true(all(grepl("^\\+[0-9]+-[0-9]{10}$", vec_mobile)))

  vec_email <- generate_random_vector(10, "email")
  expect_type(vec_email, "character")
  expect_true(all(grepl("[a-z]+@[a-z]+\\.com$", vec_email)))

  vec_rdmemail <- generate_random_vector(10, "rdmemail")
  expect_type(vec_rdmemail, "character")
  expect_true(all(grepl("^[a-z]+@[a-z]+\\.[a-z]{3}$", vec_rdmemail)))
})

test_that("generate_random_vector handels error of min = max", {
  expect_warning(generate_random_vector(10, "num", 1, 1))
})

test_that("generate_random_ip produces valid IP addresses", {
  expect_true(all(grepl("^([0-9]{1,3}\\.){3}[0-9]{1,3}$", generate_random_ip(10))))
})

test_that("generate_random_mobile_number produces valid mobile numbers", {
  expect_true(grepl("^\\+[0-9]+-[0-9]{10}$", generate_random_mobile_number()))
})

test_that("generate_random_email produces valid emails", {
  email <- generate_random_email()
  expect_true(grepl("[a-z]+@[a-z]+\\.com$", email))

  email_with_domain <- generate_random_email(domain = "custom.com")
  expect_true(grepl("[a-z]+@custom\\.com$", email_with_domain))
})

test_that("generate_random_random_email produces valid random emails", {
  email <- generate_random_random_email()
  expect_true(grepl("^[a-z]+@[a-z]+\\.[a-z]{3}$", email))
})