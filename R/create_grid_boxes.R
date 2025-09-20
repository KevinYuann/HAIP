#' Create Normalized Grid Boxes for Colonies
#'
#' Creates square bounding boxes for each colony based on their size (area).
#' The function back-calculates the colony radius assuming circular colonies,
#' then creates the largest square that fits within each circle, centered on
#' the colony's coordinates.
#'
#' If there is no colony, the function will label the new coordinates as NA.
#'
#' @param colony_data `data.frame`. Colony data containing at minimum columns
#'   `x`, `y`, `size`, `row`, and `col`. The `size` column should represent
#'   the area of each colony.
#'
#' @return A `data.frame` containing the original colony data plus new columns:
#'   \itemize{
#'     \item `new_xl`, `new_xr`: Left and right x-coordinates of bounding box
#'     \item `new_yt`, `new_yb`: Top and bottom y-coordinates of bounding box
#'     \item `grid_box_size`: Area of the created square bounding box
#'   }
#' @export
create_grid_boxes <- function(colony_data) {

  colony_data$new_xl <- NA
  colony_data$new_xr <- NA
  colony_data$new_yt <- NA
  colony_data$new_yb <- NA
  colony_data$grid_box_size <- NA

  for(i in 1:nrow(colony_data)) {
    # Get the size (area) for this specific colony
    colony_size <- colony_data$size[i]

    # Skip if size is NA or invalid
    if(is.na(colony_size) || colony_size <= 0) {
      next
    }

    # Back-calculate radius assuming colony size is area of a circle
    colony_radius <- sqrt(colony_size / pi)

    # Find largest square that fits within this circle
    # For a square inscribed in a circle: diagonal of square = 2 * radius
    # side_length = (2 * radius) / √2 = radius * √2
    square_side_length <- colony_radius * sqrt(2)
    half_side <- square_side_length / 2

    # Create bounding box centered on colony's x, y coordinates
    colony_data$new_xl[i] <- colony_data$x[i] - half_side
    colony_data$new_xr[i] <- colony_data$x[i] + half_side
    colony_data$new_yt[i] <- colony_data$y[i] - half_side
    colony_data$new_yb[i] <- colony_data$y[i] + half_side
    colony_data$grid_box_size[i] <- square_side_length^2
  }

  # Calculate summary statistics for reporting
  valid_sizes <- colony_data$size[!is.na(colony_data$size) & colony_data$size > 0]
  valid_box_sizes <- (colony_data$new_xr - colony_data$new_xl)[!is.na(colony_data$new_xl)]

  return(colony_data)
}
