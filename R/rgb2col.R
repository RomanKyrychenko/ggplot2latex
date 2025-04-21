#' @title Convert RGB Values to the Closest Named R Color
#'
#' @description
#' This function takes RGB values and finds the closest named R color by
#' calculating the Euclidean distance between the input RGB values and all
#' available named colors in R.
#' 
#' @importFrom grDevices col2rgb colours
#' @importFrom stats dist
#'
#' @param r Numeric value for red (0-255)
#' @param g Numeric value for green (0-255)
#' @param b Numeric value for blue (0-255)
#'
#' @return A string containing the name of the closest R color to the RGB values
#'
#' @examples
#' rgb2col(255, 40, 10)  # Returns a color name like "red" or "firebrick"
#'
#' @export
rgb2col <- function(r, g, b) {
  # Input validation
  if (!all(is.numeric(c(r, g, b))) || any(c(r, g, b) < 0) || any(c(r, g, b) > 255)) {
    stop("RGB values must be numeric values between 0 and 255")
  }

  colourMap <- data.frame(colourNames = colours(), t(col2rgb(colours())))
  testDF <- data.frame(colourNames = "testCol", red = r, green = g, blue = b)
  combDF <- rbind(testDF, colourMap)
  combMat <- (as.matrix(combDF[, -1]))
  rownames(combMat) <- combDF[, 1]
  approxMatchCol <- which.min(as.matrix(dist(combMat, upper = TRUE))[1, ][-1])
  names(approxMatchCol)
}
