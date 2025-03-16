# ggplot2latex

<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/RomanKyrychenko/ggplot2latex/branch/main/graph/badge.svg)](https://app.codecov.io/gh/RomanKyrychenko/ggplot2latex?branch=main)
[![R-CMD-check](https://github.com/RomanKyrychenko/ggplot2latex/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RomanKyrychenko/ggplot2latex/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`ggplot2latex` is an R package designed to efficiently save `ggplot2` plots in LaTeX TikZ format. The package provides functions to optimize the resulting `.tex` files by rounding coordinates, removing duplicated elements, and reducing the file size. This ensures that the LaTeX documents are both lightweight and maintain high-quality visualizations.

## Features

- Save `ggplot2` plots as LaTeX (TikZ) files.
- Optimize `.tex` files by rounding coordinates.
- Remove duplicated elements to reduce file size.
- Utilities to handle special characters in LaTeX.

## Installation

To install the package, use the following command in R:

```r
# Install from CRAN (if available)
install.packages("ggplot2latex")

# Or install from GitHub
devtools::install_github("RomanKyrychenko/ggplot2latex")
```

## Usage

Here is an example of how to use the save_tex function to save a ggplot2 plot as a LaTeX (TikZ) file:

```r
library(ggplot2)
library(ggplot2latex)

# Create a ggplot2 plot
ggplot_object <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()

# Save the plot as a LaTeX (TikZ) file
save_tex(ggplot_object, "path/to/file.tex")
```

## License

This package is licensed under the MIT License.