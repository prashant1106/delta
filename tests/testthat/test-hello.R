test_that("hello function outputs correct message", {
  expected_output <- c(
    "\033[0;36mHello user! I am Prashant, the author of this package.\033[0m",
    "\033[0;33mThrough Delta, you can play game when you are bored or make random data to test your program or remove ouliers and do many more such things. ",
    "Do dm me on insta if you think I can improve something.\033[0m",
    "\033[0;35mhttps://www.instagram.com/prashantjha.11/\033[0m"
  )

  expect_equal(capture.output(hello()), expected_output)

  expected_output_name <- c(
    "\033[0;36mHello CS50! I am Prashant, the author of this package.\033[0m",
    "\033[0;33mThrough Delta, you can play game when you are bored or make random data to test your program or remove ouliers and do many more such things. ",
    "Do dm me on insta if you think I can improve something.\033[0m",
    "\033[0;35mhttps://www.instagram.com/prashantjha.11/\033[0m"
  )

  expect_equal(capture.output(hello("CS50")), expected_output_name)
})