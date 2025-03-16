#' @title Save ggplot2 Plot as LaTeX (TikZ)
#' @description Saves a ggplot2 plot as a LaTeX (TikZ) file, optimizing the resulting .tex file by rounding coordinates, removing duplicated elements, and reducing the file size.
#' @param g A ggplot2 object to be saved.
#' @param file The path to the output .tex file.
#' @param width The width of the plot in inches. Default is 6.9.
#' @param height The height of the plot in inches. Default is 4.5.
#' @param reduce_power A parameter to control the reduction of the file size. Default is 5.
#' @return None
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(ggplot2latex)
#' ggplot_object <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point()
#' save_tex(ggplot_object, "path/to/file.tex")
#' }
#' @export
save_tex <- function(g, file, width = 6.9, height = 4.5, reduce_power = 0) {
  save_tikz(g, file, width, height)
  optimize_tex(file, reduce_power)
}
