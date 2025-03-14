\name{save_tex}
\alias{save_tex}
\title{Save ggplot2 Plot as LaTeX (TikZ)}
\usage{
save_tex(g, file, width = 5.5, height = 4.5, reduce_power = 5)
}
\description{
Saves a ggplot2 plot as a LaTeX (TikZ) file, optimizing the resulting .tex file by rounding coordinates, removing duplicated elements, and reducing the file size.
}
\examples{
# Example usage:
# save_tex(ggplot_object, "path/to/file.tex")
}
