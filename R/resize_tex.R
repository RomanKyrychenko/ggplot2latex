#' Get Dimensions from TeX File
#'
#' @description Analyzes a TeX file and determines the maximum dimensions based on
#' coordinates found in the file. Returns the width and height in inches.
#'
#' @param file_path Character string specifying the path to the .tex file
#'
#' @return A list with two elements: width and height in inches
#'
#' @examples
#' \dontrun{
#' # Get dimensions of a TeX file
#' dimensions <- tex_size("figure.tex")
#' width <- dimensions$width
#' height <- dimensions$height
#' }
tex_size <- function(file_path) {
  # Check if file exists
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }

  # Read the TeX file
  tex_content <- readLines(file_path, warn = FALSE)

  # Initialize maximum values
  max_x <- 0
  max_y <- 0

  # Find all coordinate pairs in the file
  for (line in tex_content) {
    # Extract coordinates in format (x,y)
    coords <- gregexpr("\\(([0-9.-]+),([0-9.-]+)\\)", line)

    if (coords[[1]][1] != -1) {
      matches <- regmatches(line, coords)[[1]]
      for (match in matches) {
        # Extract the numbers
        coord_values <- as.numeric(strsplit(gsub("[()]", "", match), ",")[[1]])
        x <- abs(coord_values[1])
        y <- abs(coord_values[2])

        # Update maximum values
        max_x <- max(max_x, x)
        max_y <- max(max_y, y)
      }
    }
  }

  # Divide by 72 to convert to inches
  list(width = max_x / 72, height = max_y / 72)
}

#' Resize TikZ Figure in TeX File
#'
#' @description Resizes a TikZ figure in a .tex file by adjusting dimensions and scaling
#' elements proportionally to the new width and height.
#'
#' @param file_path Character string specifying the path to the .tex file
#' @param width Numeric value specifying the desired width in inches
#' @param height Numeric value specifying the desired height in inches
#' @param output_path Character string specifying the output file path (default: overwrite the input file)
#'
#' @return Logical indicating success of the operation
#'
#' @importFrom stringr str_extract str_replace
#'
#' @examples
#' \dontrun{
#' # Resize a TikZ figure to 10 cm width and 8 cm height
#' resize_tex("figure.tex", width = 10, height = 8)
#'
#' # Resize a figure and save to a new file
#' resize_tex("figure.tex", width = 12, height = 9, output_path = "resized_figure.tex")
#' }
#'
#' @export
resize_tex <- function(file_path, width, height, output_path = file_path) {
  # Check if file exists
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }

  # Get current dimensions using tex_size
  current_dimensions <- tex_size(file_path)
  current_width <- current_dimensions$width * 72 # Convert inches to pixels
  current_height <- current_dimensions$height * 72 # Convert inches to pixels

  # Calculate scaling factors
  x_scale <- (width * 72) / current_width
  y_scale <- (height * 72) / current_height

  # Read the TeX file
  tex_content <- readLines(file_path, warn = FALSE)

  # Find the tikzpicture environment
  begin_idx <- grep("\\\\begin\\{tikzpicture\\}", tex_content)
  if (length(begin_idx) == 0) {
    stop("No tikzpicture environment found in the file")
  }

  # Update width and height in the tikzpicture options
  tikz_options <- tex_content[begin_idx]
  if (grepl("width=", tikz_options)) {
    tex_content[begin_idx] <- stringr::str_replace(
      tex_content[begin_idx],
      "width=[0-9]+(\\.[0-9]+)?[a-z]+",
      paste0("width=", width, "in")
    )
  } else {
    tex_content[begin_idx] <- stringr::str_replace(
      tex_content[begin_idx],
      "\\\\begin\\{tikzpicture\\}",
      paste0("\\\\begin{tikzpicture}[width=", width, "in")
    )
  }

  if (grepl("height=", tikz_options)) {
    tex_content[begin_idx] <- stringr::str_replace(
      tex_content[begin_idx],
      "height=[0-9]+(\\.[0-9]+)?[a-z]+",
      paste0("height=", height, "in")
    )
  } else {
    tex_content[begin_idx] <- stringr::str_replace(
      tex_content[begin_idx],
      "\\\\begin\\{tikzpicture\\}",
      paste0("\\\\begin{tikzpicture}[height=", height, "in")
    )
  }

  # Scale coordinate values in the file
  for (i in seq_along(tex_content)) {
    tex_content[i] <- stringr::str_replace_all(
      tex_content[i],
      "\\(([0-9.-]+),([0-9.-]+)\\)",
      function(match) {
        groups <- stringr::str_match(match, "\\(([0-9.-]+),([0-9.-]+)\\)")
        x <- as.numeric(groups[2]) * x_scale
        y <- as.numeric(groups[3]) * y_scale
        sprintf("(%.4f,%.4f)", x, y)
      }
    )

    # Scale line widths and other parameters
    matches <- gregexpr("line width=([0-9.-]+)pt", tex_content[i])
    if (matches[[1]][1] != -1) {
      match_strings <- regmatches(tex_content[i], matches)

      for (j in seq_along(match_strings[[1]])) {
        width <- as.numeric(sub("line width=([0-9.-]+)pt", "\\1", match_strings[[1]][j]))
        match_strings[[1]][j] <- sprintf("line width=%.4fpt", width * x_scale)
      }

      regmatches(tex_content[i], matches) <- match_strings
    }

    # Scale font sizes
    matches <- gregexpr("font=\\\\fontsize\\{([0-9.-]+)\\}\\{([0-9.-]+)\\}", tex_content[i])
    if (matches[[1]][1] != -1) {
      match_strings <- regmatches(tex_content[i], matches)

      for (j in seq_along(match_strings[[1]])) {
        sizes <- as.numeric(unlist(regmatches(match_strings[[1]][j], gregexpr("[0-9.-]+", match_strings[[1]][j]))))
        match_strings[[1]][j] <- sprintf("font=\\fontsize{%.4f}{%.4f}", sizes[1] * x_scale, sizes[2] * y_scale)
      }

      regmatches(tex_content[i], matches) <- match_strings
    }
  }

  # Write the modified content to the output file
  writeLines(tex_content, output_path)

  return(TRUE)
}