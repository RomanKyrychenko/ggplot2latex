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
