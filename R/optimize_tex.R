
predefined_colors <- c(
  "black", "blue", "brown", "cyan", "darkgray", "gray", "green", "lightgray", "lime", "magenta", "olive", 
  "orange", "pink", "purple", "red", "teal", "violet", "white", "yellow"
)

#' @title Optimize Colors in TeX File
#' @description
#' This function optimizes color definitions in a TeX file by replacing RGB or gray color definitions
#' with predefined or simplified color names, reducing redundancy and improving readability.
#'
#' @param lines A character vector containing the lines of the TeX file to be optimized.
#' @return A character vector containing the optimized lines of the TeX file.
#'
#' @details
#' The function reads the input TeX file, identifies color definitions, and replaces them with
#' simplified or predefined color names. It removes redundant color definitions and appends
#' new definitions at the beginning of the document. The function uses the `rgb2col` function
#' to map RGB values to the closest named R color.
#'
#' @examples
#' \dontrun{
#' optimized_lines <- optimize_colors("path/to/your/file.tex")
#' writeLines(optimized_lines, "path/to/your/optimized_file.tex")
#' }
optimize_colors <- function(lines) {
  
  # Identify document structure
  preamble_end <- which(grepl("\\\\begin\\{tikzpicture\\}\\[x=1pt,y=1pt\\]", lines))
  # if there is no preamble (integer(0)), assume the first line is the start of the document
  if (length(preamble_end) == 0) {
    preamble_end <- 1
  } 
  # First pass: identify and extract all color definitions
  color_pattern <- "\\\\definecolor\\{([^}]+)\\}\\{([^}]+)\\}\\{([^}]+)\\}"
  
  # Look for color definitions throughout the document
  color_lines <- grep(color_pattern, lines, perl = TRUE)
  
  color_definitions <- data.frame(
    line_number = color_lines,
    line_content = lines[color_lines],
    old_color_name = sub(".*\\\\definecolor\\{([^}]+)\\}.*", "\\1", lines[color_lines]),
    is_rgb = sub(".*\\\\definecolor\\{[^}]+\\}\\{([^}]+)\\}\\{([^}]+)\\}", "\\1", lines[color_lines]) == "RGB",
    color_values = sub(".*\\{([^}]+)\\}\\{([^}]+)\\}", "\\2", lines[color_lines]),
    stringsAsFactors = FALSE
  )
  
  color_definitions$new_color_name <- NA
  
  for (i in seq_len(nrow(color_definitions))) {
    if (color_definitions$is_rgb[i]) {
      color_definitions$new_color_name[i] <- do.call(rgb2col, as.list(as.numeric(strsplit(color_definitions$color_values[i], ",")[[1]])))
    } else {
      color_definitions$new_color_name[i] <- paste0("gray_", as.numeric(color_definitions$color_values[i])*100)
    }
  }
  
  color_definitions$new_color_name <- ifelse(color_definitions$new_color_name == "gray_50", "gray", color_definitions$new_color_name)
  color_definitions$new_color_name <- ifelse(color_definitions$new_color_name == "gray_83", "lightgray", color_definitions$new_color_name)
  color_definitions$new_color_name <- ifelse(color_definitions$new_color_name == "gray_17", "darkgray", color_definitions$new_color_name)
  
  # construct new color definitions
  color_definitions$new_color_definition <- ifelse(
    color_definitions$is_rgb,
    paste0("\\definecolor{", color_definitions$new_color_name, "}{RGB}{", color_definitions$color_values, "}"),
    paste0("\\definecolor{", color_definitions$new_color_name, "}{gray}{", color_definitions$color_values, "}")
  )
  
  color_definitions[,c("line_number", "old_color_name" ,"new_color_name")]
  
  # Replace old color names in the document with new ones
  for (i in seq_len(nrow(color_definitions))) {
    old_name <- color_definitions$old_color_name[i]
    new_name <- color_definitions$new_color_name[i]
    start <- color_definitions$line_number[i]
    same_color <- color_definitions$line_number[color_definitions$old_color_name == color_definitions$old_color_name[i]]
    if (sum(same_color > start)) {
      end <- same_color[same_color > start][1] - 1
    } else {
      end <- length(lines)
    }
    lines[start:end] <- gsub(old_name, new_name, lines[start:end], perl = TRUE)
  }
  
  # Remove old color definitions
  lines <- lines[-color_definitions$line_number]
  
  # Add new color definitions at the beginning of the document
  new_definitions <- unique(color_definitions$new_color_definition[!(color_definitions$new_color_name %in% predefined_colors)])
  lines <- append(lines, new_definitions, after = preamble_end)
  
  return (lines)
}

#' @title Optimize TeX File
#' @description Optimizes a TeX file by removing unnecessary lines and rounding floats.
#' @param file A character string specifying the file path to be optimized.
#' @param reduce_power A parameter to control the reduction of the file size. Default is 0.
#' @param as_file A logical value indicating whether to save the plot as a standalone TeX file. Default is FALSE.
#' @return None. The function is called for its side effects.
#' @examples
#' \dontrun{
#' optimize_tex("path/to/your/file.tex")
#' }
#' @export
optimize_tex <- function(file, reduce_power = 0, as_file = FALSE) {
  txt <- readLines(file, warn = FALSE)
  
  txt <- gsub("\n\r", "", txt)
  txt <- gsub("\n\n", "\n", txt)
  
  txt <- round_floats_in_text(txt)
  
  lines <- unlist(strsplit(txt, "(?<=;)", perl = TRUE))
  ulines <- unique(lines)
  #for (ul in ulines) {
  #  to_remove <- which(lines == ul)[-1]
  #  if (length(to_remove) > 0) {
  #    lines <- lines[-to_remove]
  #  }
  #}

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
  
  lines <- optimize_colors(lines)

  if (!as_file) {
    lines <- lines[which(lines == "\\begin{tikzpicture}[x=1pt,y=1pt]"):which(lines == "\\end{tikzpicture}")]
  }
  writeLines(lines, con = file)
}

