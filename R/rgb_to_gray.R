#' Convert RGB Values to Grayscale Intensity
#'
#' Converts RGB color values to grayscale intensity using the standard
#' luminance formula. This weighted conversion accounts for human perception
#' of different color channels (green appears brighter than red, which
#' appears brighter than blue).
#'
#' @param r `numeric`. Red channel values (0-255).
#' @param g `numeric`. Green channel values (0-255).
#' @param b `numeric`. Blue channel values (0-255).
#'
#' @return `numeric` vector of grayscale intensity values using the formula:
#'   0.299 * R + 0.587 * G + 0.114 * B
#' @export
rgb_to_gray <- function(r, g, b) {
  # Standard luminance formula
  return(0.299 * r + 0.587 * g + 0.114 * b)
}
