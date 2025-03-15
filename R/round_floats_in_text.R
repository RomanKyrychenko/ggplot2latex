#' @title Round Floats in Text
#' @description Rounds the floating-point numbers in a given text to a specified number of digits.
#' @param text A character string containing the text with floating-point numbers.
#' @param digits The number of decimal places to round to. Default is 0.
#' @return A character string with rounded floating-point numbers.
#' @examples
#' library(ggplot2latex)
#' text <- "Some values are ( 12.7 ,45.3) and also (-3.6,  7.9 )."
#' new_text <- round_floats_in_text(text)
#' print(new_text)
#' @export
round_floats_in_text <- function(text, digits = 0) {
  # Define the pattern to match (float1, float2) with optional spaces
  pattern <- "\\(\\s*(-?\\d+\\.\\d+)\\s*(,\\s*(-?\\d+\\.\\d+))?\\s*\\)"

  # Find matches
  matches <- gregexpr(pattern, text, perl = TRUE)

  # Extract matches and process them
  regmatches(text, matches) <- lapply(regmatches(text, matches), function(m) {
    sapply(m, function(match) {
      # Extract numbers from the matched string
      numbers <- as.numeric(unlist(regmatches(match, gregexpr("-?\\d+\\.\\d+", match))))

      # Round the numbers
      rounded_numbers <- round(numbers, digits)

      # Replace with rounded values
      if (length(rounded_numbers) == 2) {
        format_string <- if (digits == 0) "(%.d, %.d)" else sprintf("(%%.%df, %%.%df)", digits, digits)
        sprintf(format_string, rounded_numbers[1], rounded_numbers[2])
      } else {
        format_string <- if (digits == 0) "(%.d)" else sprintf("(%%.%df)", digits)
        sprintf(format_string, rounded_numbers[1])
      }
    })
  })

  return(text)
}
