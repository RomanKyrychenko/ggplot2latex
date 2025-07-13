test_that("tex_size correctly calculates dimensions for a simple TikZ file", {
  temp_file <- tempfile(fileext = ".tex")
  writeLines(c(
    "\\begin{tikzpicture}",
    "(0,0) (5,3) (10,7)",
    "\\end{tikzpicture}"
  ), temp_file)
  
  dimensions <- tex_size(temp_file)
  
  expect_equal(dimensions$width, 10 / 72)
  expect_equal(dimensions$height, 7 / 72)
  unlink(temp_file)
})

test_that("tex_size handles files with no coordinates gracefully", {
  temp_file <- tempfile(fileext = ".tex")
  writeLines(c(
    "\\begin{tikzpicture}",
    "\\end{tikzpicture}"
  ), temp_file)
  
  dimensions <- tex_size(temp_file)
  
  expect_equal(dimensions$width, 0)
  expect_equal(dimensions$height, 0)
  unlink(temp_file)
})

test_that("resize_tex scales coordinates correctly", {
  temp_file <- tempfile(fileext = ".tex")
  output_file <- tempfile(fileext = ".tex")
  writeLines(c(
    "\\begin{tikzpicture}",
    "(0,0) (5,3) (10,7)",
    "\\end{tikzpicture}"
  ), temp_file)
  
  resize_tex(temp_file, width = 20, height = 14, output_path = output_file)
  output_content <- readLines(output_file)
  
  expect_true(any(grepl("\\(0.0000,0.0000\\)", output_content)))
  expect_true(any(grepl("\\(10.0000,6.0000\\)", output_content)))
  expect_true(any(grepl("\\(20.0000,14.0000\\)", output_content)))
  unlink(temp_file)
  unlink(output_file)
})

test_that("resize_tex handles missing tikzpicture environment", {
  temp_file <- tempfile(fileext = ".tex")
  writeLines(c(
    "\\documentclass{article}",
    "\\begin{document}",
    "No tikzpicture here.",
    "\\end{document}"
  ), temp_file)
  
  expect_error(resize_tex(temp_file, width = 10, height = 8))
  unlink(temp_file)
})

test_that("resize_tex handles invalid file paths", {
  expect_error(resize_tex("nonexistent_file.tex", width = 10, height = 8))
})