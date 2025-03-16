#' @title Optimize TeX File
#' @description Optimizes a TeX file by removing unnecessary lines and rounding floats.
#' @param file A character string specifying the file path to be optimized.
#' @param reduce_power A parameter to control the reduction of the file size. Default is 0.
#' @return None. The function is called for its side effects.
#' @examples
#' \dontrun{
#' optimize_tex("path/to/your/file.tex")
#' }
#' @export
optimize_tex <- function(file, reduce_power = 0) {
  txt <- readLines(file, warn = FALSE)
  
  txt <- gsub("\n\r", "", txt)
  txt <- gsub("\n\n", "\n", txt)
  
  txt <- round_floats_in_text(txt)
  
  lines <- unlist(strsplit(txt, "(?<=;)", perl = TRUE))
  ulines <- unique(lines)
  for (ul in ulines) {
    to_remove <- which(lines == ul)[-1]
    if (length(to_remove) > 0) {
      lines <- lines[-to_remove]
    }
  }

  lines <- lines[-which(grepl("\\path\\[clip\\]*", lines, perl = F))]
  lines <- lines[-which(grepl("\\path\\[use as bounding box*", lines, perl = F))]
  
  upath <- unique(lines[grepl("\\path\\[", lines, perl = F)])
  for (up in upath) {
    to_remove <- which(lines == up)[-1]
    if (length(to_remove) > 0) {
      lines <- lines[-to_remove]
    }
  }

  lines <- lines[lines != ""]
  lines <- lines[c(TRUE, lines[-1] != lines[-length(lines)])]
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