#' Initialize Image to Data Frame
#'
#' Reads an image file and converts it to a data frame format with pixel
#' coordinates and color information. The resulting data frame contains x/y
#' coordinates for each pixel along with RGB color values and hex color codes.
#' The y-axis is flipped to match standard image orientation.
#'
#' @param path_name `character(1)`. Path to the image file to be read and
#'   processed. Must be a valid image format supported by `magick::image_read()`.
#'
#' @return A `data.frame` with columns:
#'   \itemize{
#'     \item `x`: Pixel x-coordinate (column position)
#'     \item `y`: Pixel y-coordinate (row position, flipped to match image orientation)
#'     \item `R`: Red channel value (0-255)
#'     \item `G`: Green channel value (0-255)
#'     \item `B`: Blue channel value (0-255)
#'     \item `value`: Hex color code representation of the RGB values
#'   }
#' @export
initialize_image <- function(path_name) {
  img <- image_read(path_name)

  # Convert to array format
  img_array <- as.integer(img[[1]])

  # Get dimensions
  height <- dim(img_array)[1]
  width <- dim(img_array)[2]
  channels <- dim(img_array)[3]

  if (channels == 3) {
    img_df <- data.frame(
      x = rep(1:width, each = height),
      y = rep(height:1, width),  # Flip y-axis to match image orientation
      R = as.vector(img_array[,,1]),
      G = as.vector(img_array[,,2]),
      B = as.vector(img_array[,,3])
    )

    # Create hex color values
    img_df$value <- rgb(img_df$R, img_df$G, img_df$B, maxColorValue = 255)
  }
  return(img_df)
}
