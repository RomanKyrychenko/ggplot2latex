test_that("save_tex saves a simple plot", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()
  file <- tempfile(fileext = ".tex")
  save_tex(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tex saves a plot with title", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    ggtitle("Title")
  file <- tempfile(fileext = ".tex")
  save_tex(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tex saves a plot with labels", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    labs(x = "Weight", y = "MPG")
  file <- tempfile(fileext = ".tex")
  save_tex(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tex saves a plot with theme", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    theme_minimal()
  file <- tempfile(fileext = ".tex")
  save_tex(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tex saves a plot with facets", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    facet_wrap(~cyl)
  file <- tempfile(fileext = ".tex")
  save_tex(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tex saves a plot with color", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
    geom_point()
  file <- tempfile(fileext = ".tex")
  save_tex(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tex saves a plot with size", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg, size = qsec)) +
    geom_point()
  file <- tempfile(fileext = ".tex")
  save_tex(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tex saves a plot with shape", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg, shape = factor(gear))) +
    geom_point()
  file <- tempfile(fileext = ".tex")
  save_tex(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tex saves a plot with line", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_line()
  file <- tempfile(fileext = ".tex")
  save_tex(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tex saves a pplot with smooth", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_smooth()
  file <- tempfile(fileext = ".tex")
  save_tex(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tex and save_tikz comparison", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()

  file_tex <- tempfile(fileext = ".tex")
  file_tikz <- tempfile(fileext = ".tikz")

  save_tex(p, file_tex)
  save_tikz(p, file_tikz)

  size_tex <- file.info(file_tex)$size
  size_tikz <- file.info(file_tikz)$size

  lines_tex <- length(readLines(file_tex))
  lines_tikz <- length(readLines(file_tikz))

  print(paste("save_tex file size:", size_tex, "bytes"))
  print(paste("save_tikz file size:", size_tikz, "bytes"))
  print(paste("save_tex number of lines:", lines_tex))
  print(paste("save_tikz number of lines:", lines_tikz))

  expect_true(size_tex < size_tikz)
  expect_true(lines_tex < lines_tikz)

  unlink(file_tex)
  unlink(file_tikz)
})

test_that("save_tex saves a plot with smooth", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_smooth()
  file <- tempfile(fileext = ".tex")
  save_tex(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tex saves a plot with different point size", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point(size = 2)
  file <- tempfile(fileext = ".tex")
  save_tex(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tex saves a plot with different line size", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_path(linewidth = 1)
  file <- tempfile(fileext = ".tex")
  save_tex(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tex saves a plot with different alpha", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point(alpha = 0.5)
  file <- tempfile(fileext = ".tex")
  save_tex(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tex saves a plot with different color", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point(color = "blue")
  file <- tempfile(fileext = ".tex")
  save_tex(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tex saves a plot with different shape", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point(shape = 17)
  file <- tempfile(fileext = ".tex")
  save_tex(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tex saves a plot with different linetype", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_line(linetype = "dashed")
  file <- tempfile(fileext = ".tex")
  save_tex(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tex saves a plot with different fill", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point(fill = "red")
  file <- tempfile(fileext = ".tex")
  save_tex(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tex saves a plot with different stroke", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point(stroke = 2)
  file <- tempfile(fileext = ".tex")
  save_tex(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tex saves a plot with different size and reduce_power", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point(size = 1)
  file <- tempfile(fileext = ".tex")
  save_tex(p, file, reduce_power = 2)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tex saves a plot with different size and higher reduce_power", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point(size = 1)
  file <- tempfile(fileext = ".tex")
  save_tex(p, file, reduce_power = 3)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tex works with as_file = TRUE", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()
  file <- tempfile(fileext = ".tex")
  save_tex(p, file, as_file = TRUE)

  # Check file exists
  expect_true(file.exists(file))

  # Check content contains standalone document elements
  content <- readLines(file)
  expect_true(any(grepl("\\\\documentclass", content)))
  expect_true(any(grepl("\\\\begin\\{document\\}", content)))
  expect_true(any(grepl("\\\\end\\{document\\}", content)))

  unlink(file)
})

test_that("save_tex works with different width and height", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()
  file <- tempfile(fileext = ".tex")
  # Use non-default dimensions
  save_tex(p, file, width = 10, height = 8)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tex works with fractional reduce_power", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()
  file <- tempfile(fileext = ".tex")
  save_tex(p, file, reduce_power = 0.5)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tex handles plot with multiple geoms", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    geom_line() +
    geom_smooth(method = "lm")
  file <- tempfile(fileext = ".tex")
  save_tex(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tex handles plot with complex annotations", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    annotate("text", x = 4, y = 30, label = "Special point") +
    annotate("rect", xmin = 3, xmax = 5, ymin = 15, ymax = 25,
             alpha = 0.2, fill = "blue")
  file <- tempfile(fileext = ".tex")
  save_tex(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tex preserves the essential plot structure", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()
  file <- tempfile(fileext = ".tex")
  save_tex(p, file)

  content <- readLines(file)

  # Check that tikzpicture environment exists
  expect_true(any(grepl("\\\\begin\\{tikzpicture\\}", content)))
  expect_true(any(grepl("\\\\end\\{tikzpicture\\}", content)))

  # Check that path commands exist (common in tikz output)
  expect_true(any(grepl("\\\\path", content)))

  unlink(file)
})