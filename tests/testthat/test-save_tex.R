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
    geom_point(aes(color = factor(cyl), size = hp), alpha = 0.7) +
    geom_line(aes(group = factor(cyl), color = factor(cyl)), linetype = "dashed") +
    geom_smooth(method = "lm", se = TRUE, color = "red", fill = "pink", alpha = 0.3) +
    scale_color_brewer(name = "Cylinders", palette = "Set1") +
    scale_size_continuous(name = "Horsepower", range = c(1, 4)) +
    annotate("rect", xmin = 3, xmax = 4, ymin = 10, ymax = 15,
             alpha = 0.1, fill = "blue", color = "navy", linetype = "dotted") +
    labs(title = "Car Weight vs. Fuel Efficiency",
         subtitle = "Grouped by number of cylinders",
         x = "Weight (1000 lbs)",
         y = "Miles per Gallon")
  file <- tempfile(fileext = ".tex")
  save_tex(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tex handles plot with complex annotations", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point(aes(size = hp, shape = factor(am)), alpha = 0.8) +
    geom_smooth(method = "loess", se = TRUE, color = "tomato", fill = "pink", alpha = 0.3) +
    annotate("text", x = 4, y = 30, label = "Special point",
             fontface = "italic", size = 5, color = "purple") +
    annotate("rect", xmin = 3, xmax = 5, ymin = 15, ymax = 25,
             alpha = 0.2, fill = "blue", color = "darkblue", linetype = "dashed") +
    annotate("segment", x = 3.5, xend = 4, y = 20, yend = 30,
             arrow = arrow(length = unit(0.2, "cm")), color = "red", linewidth = 1) +
    annotate("curve", x = 2, y = 27, xend = 2.5, yend = 33,
             curvature = -0.3, arrow = arrow(length = unit(0.2, "cm")), color = "orange") +
    scale_size_continuous(range = c(1, 6)) +
    scale_shape_manual(values = c(16, 17))
  file <- tempfile(fileext = ".tex")
  save_tex(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tex preserves the essential plot structure", {
  require(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl), size = hp, group = cyl)) +
    geom_point(alpha = 0.7, shape = 16) +
    geom_smooth(method = "lm", se = TRUE, linetype = "dashed") +
    scale_color_viridis_d(name = "Cylinders") +
    scale_size_continuous(name = "Horsepower", range = c(1, 5)) +
    labs(title = "Car Weight vs. Fuel Efficiency",
         subtitle = "By cylinder count and horsepower",
         x = "Weight (1000 lbs)",
         y = "Miles per Gallon") +
    theme_minimal() +
    facet_wrap(~am, labeller = labeller(am = c("0" = "Automatic", "1" = "Manual")))
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

test_that("save_tex works with igraph plots", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggraph")
  require(ggplot2)
  require(igraph)
  require(ggraph)

  # Create a simple graph
  g <- make_ring(10)

  # Create plot with ggraph
  p <- ggraph(g, layout = "circle") +
    geom_edge_link() +
    geom_node_point(linewidth = 5) +
    theme_graph()

  file <- tempfile(fileext = ".tex")
  save_tex(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tex works with complex network graphs", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggraph")
  require(igraph)
  require(ggraph)

  # Create a more complex graph
  g <- sample_pa(30, power = 1, directed = FALSE)
  V(g)$size <- degree(g) * 0.5 + 2

  # Create plot with node colors, edge weights and custom layout
  p <- ggraph(g, layout = "kk") +
    geom_edge_link(aes(width = seq_len(ecount(g))/ecount(g)),
                  alpha = 0.7, color = "gray50") +
    geom_node_point(aes(size = size, color = size)) +
    scale_color_viridis_c() +
    scale_edge_width(range = c(0.2, 1.5)) +
    labs(title = "Preferential Attachment Network") +
    theme_graph()

  file <- tempfile(fileext = ".tex")
  save_tex(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tex works with network graphs with node labels", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggraph")
  require(igraph)
  require(ggraph)

  # Create a graph with named vertices
  g <- make_graph("Zachary")
  V(g)$name <- paste0("Node ", 1:vcount(g))

  # Create plot with node labels
  p <- ggraph(g, layout = "fr") +
    geom_edge_link(alpha = 0.3) +
    geom_node_point(color = "steelblue") +
    geom_node_text(aes(label = name), repel = TRUE, size = 3) +
    theme_graph()

  file <- tempfile(fileext = ".tex")
  save_tex(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tex works with network community detection visualization", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggraph")
  require(igraph)
  require(ggraph)

  # Create a graph with communities
  g <- sample_pa(40, power = 1, directed = FALSE)
  communities <- cluster_louvain(g)
  V(g)$community <- membership(communities)

  # Create plot with community colors
  p <- ggraph(g, layout = "fr") +
    geom_edge_link(alpha = 0.5) +
    geom_node_point(aes(color = factor(community)), size = 4) +
    scale_color_brewer(name = "Community", palette = "Set1") +
    theme_graph() +
    ggtitle("Network Communities")

  file <- tempfile(fileext = ".tex")
  save_tex(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tex works with hierarchical edge bundling", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggraph")
  require(igraph)
  require(ggraph)

  # Create hierarchical graph
  g <- make_tree(20, 2, mode = "out")

  # Create plot with edge bundling
  p <- ggraph(g, layout = "dendrogram") +
    geom_edge_diagonal(aes(alpha = after_stat(index)), color = "steelblue") +
    geom_node_point(color = "red") +
    theme_graph() +
    ggtitle("Hierarchical Edge Bundling")

  file <- tempfile(fileext = ".tex")
  save_tex(p, file)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tex works with network layouts with high reduce_power", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("ggraph")
  require(igraph)
  require(ggraph)

  # Create a complete graph
  g <- make_full_graph(8)

  # Create plot
  p <- ggraph(g, layout = "circle") +
    geom_edge_link(linewidth = 1, color = "grey70") +
    geom_node_point(size = 8, color = "steelblue") +
    geom_node_text(aes(label = 1:vcount(g)), color = "white") +
    theme_graph()

  file <- tempfile(fileext = ".tex")
  save_tex(p, file, reduce_power = 3)
  expect_true(file.exists(file))
  unlink(file)
})

test_that("save_tex produces smaller files than save_tikz", {
  # Skip if tikzDevice is not installed
  skip_if_not_installed("tikzDevice")
  require(ggplot2)

  # Create a simple plot
  p <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
    geom_point() +
    labs(title = "Weight vs Fuel Efficiency", color = "Cylinders") +
    theme_minimal()

  # Save using both methods
  tikz_file <- tempfile(fileext = ".tex")
  tex_file <- tempfile(fileext = ".tex")

  save_tikz(p, tikz_file)
  save_tex(p, tex_file)

  # Compare file sizes
  tikz_size <- file.size(tikz_file)
  tex_size <- file.size(tex_file)

  # Test that optimized file is smaller
  expect_lt(tex_size, tikz_size)

  # Test with different reduction powers
  tex_file_high <- tempfile(fileext = ".tex")
  save_tex(p, tex_file_high, reduce_power = 2)
  tex_size_high <- file.size(tex_file_high)

  # Higher reduce_power should produce even smaller files
  expect_lt(tex_size_high, tex_size)

  # Cleanup
  unlink(c(tikz_file, tex_file, tex_file_high))
})

test_that("save_tex reduces file size for complex plots", {
  require(ggplot2)

  # Create a more complex plot with many elements
  p <- ggplot(diamonds[1:1000,], aes(x = carat, y = price, color = cut)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm") +
    facet_wrap(~clarity) +
    scale_color_brewer(palette = "Set1") +
    theme_minimal() +
    labs(title = "Diamond Price by Carat and Cut",
         subtitle = "Grouped by clarity",
         x = "Carat (weight)",
         y = "Price (USD)")

  # Save using both methods
  tikz_file <- tempfile(fileext = ".tex")
  tex_file <- tempfile(fileext = ".tex")

  save_tikz(p, tikz_file)
  save_tex(p, tex_file)

  # Compare file sizes
  tikz_size <- file.size(tikz_file)
  tex_size <- file.size(tex_file)

  # Test that optimized file is smaller
  expect_lt(tex_size, tikz_size)

  # Calculate reduction percentage
  reduction_percent <- (1 - tex_size/tikz_size) * 100

  # Print size reduction info for debugging
  message(sprintf("Original size: %d bytes, Optimized size: %d bytes, Reduction: %.1f%%",
                  tikz_size, tex_size, reduction_percent))

  # Expect at least 10% reduction
  expect_gt(reduction_percent, 10)

  # Cleanup
  unlink(c(tikz_file, tex_file))
})