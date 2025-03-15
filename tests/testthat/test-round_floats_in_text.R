test_that("rounding works with different spacing", {
  text <- "Values: ( 1.2 , 3.4 ) and ( 5.6 , 7.8 )."
  new_text <- round_floats_in_text(text)
  expected <- "Values: (1, 3) and (6, 8)."
  expect_equal(new_text, expected)
})

test_that("rounding works with negative numbers", {
  text <- "Negative values: (-1.2, -3.4) and (-5.6, -7.8)."
  new_text <- round_floats_in_text(text)
  expected <- "Negative values: (-1, -3) and (-6, -8)."
  expect_equal(new_text, expected)
})

test_that("rounding works with mixed numbers", {
  text <- "Mixed values: (1.2, -3.4) and (-5.6, 7.8)."
  new_text <- round_floats_in_text(text)
  expected <- "Mixed values: (1, -3) and (-6, 8)."
  expect_equal(new_text, expected)
})

test_that("rounding works with one number", {
  text <- "Single value: 1.2. ( 8.8)"
  new_text <- round_floats_in_text(text)
  expected <- "Single value: 1.2. (9)"
  expect_equal(new_text, expected)
})

test_that("rounding works with digits parameter", {
  text <- "Values: (1.234, 5.6789)."
  new_text <- round_floats_in_text(text, digits = 2)
  expected <- "Values: (1.23, 5.68)."
  expect_equal(new_text, expected)
})

test_that("rounding works with no floats", {
  text <- "No floats here."
  new_text <- round_floats_in_text(text)
  expected <- "No floats here."
  expect_equal(new_text, expected)
})

test_that("rounding works with multiple decimal places", {
  text <- "Values: (1.23456, 7.89012)."
  new_text <- round_floats_in_text(text)
  expected <- "Values: (1, 8)."
  expect_equal(new_text, expected)
})


test_that("rounding works with mixed spacing", {
  text <- "Values: ( 1.2 ,3.4) and (5.6, 7.8 )."
  new_text <- round_floats_in_text(text)
  expected <- "Values: (1, 3) and (6, 8)."
  expect_equal(new_text, expected)
})

test_that("rounding works with different locale formats", {
  text <- "Values: 1,2 and 3,4."
  new_text <- round_floats_in_text(text)
  expected <- "Values: 1,2 and 3,4."
  expect_equal(new_text, expected)
})

test_that("rounding works with large numbers", {
  text <- "Large values: (123456.789, 987654.321)."
  new_text <- round_floats_in_text(text)
  expected <- "Large values: (123457, 987654)."
  expect_equal(new_text, expected)
})
