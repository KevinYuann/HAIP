#' Calculate Edge Intensity Difference Between Two Images
#'
#' Compares the mean pixel intensities in the edge regions of two images.
#' Extracts pixels within a specified distance from the image borders and
#' calculates the difference in their mean grayscale intensities. Useful
#' for background normalization and image comparison tasks.
#'
#' @param img_df1 `data.frame`. First image data frame with columns `x`, `y`,
#'   `R`, `G`, `B` created by `initialize_image()`.
#' @param img_df2 `data.frame`. Second image data frame with columns `x`, `y`,
#'   `R`, `G`, `B` created by `initialize_image()`.
#' @param edge_width `numeric(1)`. Width of edge region in pixels from the
#'   outer border. Default is 150.
#'
#' @return A `list` containing:
#'   \itemize{
#'     \item `mean_intensity1`: Mean intensity of edge pixels in first image
#'     \item `mean_intensity2`: Mean intensity of edge pixels in second image
#'     \item `difference`: Difference between the two mean intensities
#'   }
#' @export
calculate_edge_intensity <- function(img_df1, img_df2, edge_width = 150) {
  # Helper function to extract edge pixels and calculate mean intensity
  calculate_edge_mean <- function(img_df, width) {
    # Get image dimensions
    max_x <- max(img_df$x)
    max_y <- max(img_df$y)

    # Filter to edge pixels only
    edge_pixels <- img_df %>%
      filter(
        x <= width |                    # Left edge
          x >= (max_x - width + 1) |     # Right edge
          y <= width |                   # Bottom edge (assuming y=1 is bottom)
          y >= (max_y - width + 1)      # Top edge
      ) %>%
      mutate(intensity = rgb_to_gray(R, G, B)) %>%  # Convert to grayscale
      pull(intensity)                               # Extract intensity values

    return(mean(edge_pixels, na.rm = TRUE))
  }

  # Validate inputs
  if (missing(img_df1) || missing(img_df2)) {
    stop("Both img_df1 and img_df2 must be provided")
  }

  # Check required columns
  required_cols <- c("x", "y", "R", "G", "B")
  if (!all(required_cols %in% names(img_df1)) || !all(required_cols %in% names(img_df2))) {
    stop("Both data frames must have columns: x, y, R, G, B")
  }

  # Get dimensions for validation
  dims1 <- c(max(img_df1$x), max(img_df1$y))
  dims2 <- c(max(img_df2$x), max(img_df2$y))

  if (!identical(dims1, dims2)) {
    warning("Images have different dimensions")
  }

  # Check if edge width is reasonable
  min_dim <- min(dims1)
  max_edge_width <- floor(min_dim / 2) - 1

  if (edge_width > max_edge_width) {
    warning(paste("Edge width", edge_width, "is large for image size.",
                  "Consider using", max_edge_width, "or smaller."))
  }

  mean_intensity1 <- calculate_edge_mean(img_df1, edge_width)
  mean_intensity2 <- calculate_edge_mean(img_df2, edge_width)

  intensity_difference <- mean_intensity1 - mean_intensity2

  return(list(
    mean_intensity1 = mean_intensity1,
    mean_intensity2 = mean_intensity2,
    difference = intensity_difference
  ))
}
