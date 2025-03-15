#' @title Save ggplot2 Plot as LaTeX (TikZ)
#' @description Saves a ggplot2 plot as a LaTeX (TikZ) file, optimizing the resulting .tex file by rounding coordinates, removing duplicated elements, and reducing the file size.
#' @param g A ggplot2 object to be saved.
#' @param file The path to the output .tex file.
#' @param width The width of the plot in inches. Default is 5.5.
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
save_tex <- function(g, file, width = 5.5, height = 4.5, reduce_power = 0) {
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
  # remove all \r from the file
  txt <- readr::read_file(file)

  txt <- stringr::str_replace_all(txt, "\n\r", "")
  txt <- stringr::str_replace_all(txt, "\n\n", "\n")

  txt <- round_floats_in_text(txt)

  lines <- stringr::str_split_1(string = txt, pattern = ";")
  ulines <- unique(lines)
  for (ul in ulines) {
    to_remove <- which(lines == ul)[-1]
    if (length(to_remove) > 0) {
      lines <- lines[-to_remove]
    }
  }

  txt <- paste(lines, collapse = ";")

  readr::write_file(txt, file)

  lines <- readLines(con = file)
  lines <- lines[-which(grepl("\\path\\[clip\\]*", lines, perl = F))]
  lines <- lines[-which(grepl("\\path\\[use as bounding box*", lines, perl = F))]

  upath <- unique(lines[grepl("\\path\\[", lines, perl = F)])
  for (up in upath) {
    to_remove <- which(lines == up)[-1]
    if (length(to_remove) > 0) {
      lines <- lines[-to_remove]
    }
  }
  # remove duplicated \definecolor lines (keep only first)
  lines <- lines[lines != ""]
  lines <- lines[lines != dplyr::lag(lines, 1)]
  if (reduce_power != 0) {
    if (reduce_power < 1) {
      reduce_power <- ceiling(1 / reduce_power)
      flt <- rep(c(rep(F, reduce_power), T), length(lines) / reduce_power)[1:length(lines)]
    } else {
      flt <- rep(c(F, rep(T, reduce_power)), length(lines) / reduce_power)[1:length(lines)]
    }
    rnd <- as.logical(1 - grepl("\\path\\[.*\\] \\(.*\\) circle \\(.*\\);", lines, perl = F) * flt) & as.logical(1 - grepl("\\t\\(.*\\) --", lines, perl = F) * flt)
    lines <- lines[rnd]
  }
  lines <- lines[which(lines == "\\begin{tikzpicture}[x=1pt,y=1pt]"):which(lines == "\\end{tikzpicture}")]
  writeLines(lines, con = file)
}
