test_that("rgb2col returns expected color names for known RGB values", {
  expect_equal(rgb2col(255, 0, 0), "red")
  expect_equal(rgb2col(0, 255, 0), "green")
  expect_equal(rgb2col(0, 0, 255), "blue")
  expect_equal(rgb2col(255, 255, 255), "white")
  expect_equal(rgb2col(0, 0, 0), "black")
})

test_that("rgb2col handles edge cases correctly", {
  # Close colors should map consistently
  firebrick_result <- rgb2col(178, 34, 34)
  expect_true(firebrick_result %in% c("firebrick", "firebrick1", "firebrick2", "firebrick3", "firebrick4"))
  
  # Values close to known colors
  expect_equal(rgb2col(240, 248, 255), "aliceblue")  # Exact match for aliceblue
})

test_that("rgb2col validates inputs correctly", {
  expect_error(rgb2col("red", 0, 0), "RGB values must be numeric")
  expect_error(rgb2col(256, 0, 0), "RGB values must be numeric values between 0 and 255")
  expect_error(rgb2col(-1, 0, 0), "RGB values must be numeric values between 0 and 255")
})