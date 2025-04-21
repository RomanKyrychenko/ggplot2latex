test_that("save_tikz saves a simple plot", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()
  file <- tempfile(fileext = ".tex")
  save_tikz(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tikz saves a plot with title", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    ggtitle("Title")
  file <- tempfile(fileext = ".tex")
  save_tikz(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tikz saves a plot with labels", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    labs(x = "Weight", y = "MPG")
  file <- tempfile(fileext = ".tex")
  save_tikz(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tikz saves a plot with theme", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    theme_minimal()
  file <- tempfile(fileext = ".tex")
  save_tikz(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tikz saves a plot with facets", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    facet_wrap(~cyl)
  file <- tempfile(fileext = ".tex")
  save_tikz(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tikz saves a plot with color", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
    geom_point()
  file <- tempfile(fileext = ".tex")
  save_tikz(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tikz saves a plot with size", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg, size = qsec)) +
    geom_point()
  file <- tempfile(fileext = ".tex")
  save_tikz(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tikz saves a plot with shape", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg, shape = factor(gear))) +
    geom_point()
  file <- tempfile(fileext = ".tex")
  save_tikz(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tikz saves a plot with line", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_line()
  file <- tempfile(fileext = ".tex")
  save_tikz(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tikz saves a pplot with smooth", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_smooth()
  file <- tempfile(fileext = ".tex")
  save_tikz(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tikz works with custom width and height", {
  require(ggplot2)
  require(pdftools)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()
  file <- tempfile(fileext = ".tex")
  # Use non-default dimensions
  save_tikz(p, file, width = 10, height = 8)

  expect_true(file.exists(file))
  
  tools::texi2pdf(file, clean = TRUE)
  
  size <- pdf_pagesize(basename(sub("\\.tex$", ".pdf", file)))
  
  file.remove(basename(sub("\\.tex$", ".pdf", file)))

  # Check content for width/height settings
  content <- readLines(file)
  expect_true(size$width/72 == 10)
  expect_true(size$height/72 == 8)

  unlink(file)
})

test_that("save_tikz preserves the essential plot structure", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()
  file <- tempfile(fileext = ".tex")
  save_tikz(p, file)

  content <- readLines(file)

  # Check that tikzpicture environment exists
  expect_true(any(grepl("\\\\begin\\{tikzpicture\\}", content)))
  expect_true(any(grepl("\\\\end\\{tikzpicture\\}", content)))

  # Check that standalone document elements exist
  expect_true(any(grepl("\\\\documentclass", content)))
  expect_true(any(grepl("\\\\begin\\{document\\}", content)))
  expect_true(any(grepl("\\\\end\\{document\\}", content)))

  unlink(file)
})

test_that("save_tikz handles complex plot with multiple layers", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    geom_line() +
    geom_smooth(method = "lm") +
    theme_minimal() +
    labs(title = "Weight vs MPG", x = "Weight", y = "MPG")
  file <- tempfile(fileext = ".tex")
  save_tikz(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tikz handles plots with annotations", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    annotate("text", x = 4, y = 30, label = "Special point") +
    annotate("rect", xmin = 3, xmax = 5, ymin = 15, ymax = 25,
             alpha = 0.2, fill = "blue")
  file <- tempfile(fileext = ".tex")
  save_tikz(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tikz properly handles special characters", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    labs(title = "Weight vs MPG: % $ _ ^ { }")
  file <- tempfile(fileext = ".tex")
  save_tikz(p, file)

  expect_true(file.exists(file))

  # Check if special characters are sanitized
  content <- paste(readLines(file), collapse = " ")
  expect_true(grepl("\\\\%", content) || grepl("\\\\$", content) ||
              grepl("\\\\textunderscore", content) || grepl("\\\\\\^\\{\\}", content) ||
              grepl("\\\\\\{", content) || grepl("\\\\\\}", content))

  unlink(file)
})

test_that("save_tikz handles plots with different point aesthetics", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point(size = 3, shape = 18, color = "red", alpha = 0.7)
  file <- tempfile(fileext = ".tex")
  save_tikz(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tikz works with plots containing statistical transformations", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt)) +
    geom_histogram(bins = 10)
  file <- tempfile(fileext = ".tex")
  save_tikz(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tikz can handle interaction between continuous and discrete variables", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
    geom_boxplot()
  file <- tempfile(fileext = ".tex")
  save_tikz(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tikz can handle plots with coordinate transformations", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    coord_flip()
  file <- tempfile(fileext = ".tex")
  save_tikz(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tikz can handle plots with custom scales", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg, color = qsec)) +
    geom_point() +
    scale_color_gradient(low = "blue", high = "red")
  file <- tempfile(fileext = ".tex")
  save_tikz(p, file)
  expect_true(file.exists(file))
  unlink(file)
})