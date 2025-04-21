#' @title Save Plot as TikZ/TeX file without optimization
#' @description Saves a ggplot object as a TikZ/TeX file without optimization.
#' @param g A ggplot object to be saved.
#' @param file A character string specifying the file path to save the plot.
#' @param width The width of the plot in inches. Default is 6.9.
#' @param height The height of the plot in inches. Default is 4.5.
#' @return None. The function is called for its side effects.
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#' save_tikz(p, "plot.tex")
#' }
#' @export
save_tikz <- function(g, file, width = 6.9, height = 4.5) {
  # For some reason, Rstudio needs to know the time zone...
  options(tz = "CA")

  options(tikzSanitizeCharacters = c("%", "$", "}", "{", "^", "_"))
  options(tikzReplacementCharacters = c(
    "\\%", "\\$", "\\}", "\\{",
    "\\^{}", "\\textunderscore"
  ))
  tikzDevice::tikz(file = file, width = width, height = height, sanitize = TRUE, standAlone = TRUE)
  print(g)
  grDevices::dev.off()
}