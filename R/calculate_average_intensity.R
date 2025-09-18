#' Calculate Average Pixel Intensity in Bounding Boxes
#'
#' For each bounding box defined by coordinates (new_xl, new_xr, new_yt, new_yb),
#' calculates the mean, median, and standard deviation of pixel intensities within
#' that area.
#'
#' Requires `create_grid_boxes()` to be called first to establish
#' bounding box coordinates.
#'
#' Outputs a warning message if there are no pixels within the bounding box
#' created
#'
#' @param img_df `data.frame`. Image data frame created by `initialize_image()`,
#'   containing pixel coordinates and RGB color values.
#' @param colony_data `data.frame`. Colony data with bounding box coordinates
#'   (new_xl, new_xr, new_yt, new_yb) created by `create_grid_boxes()`.
#' @param prefix `character(1)`. Prefix to add to the new intensity columns.
#'   Default is "bg" (background).
#'
#' @return A `data.frame` containing the original colony data plus three new
#'   columns with the specified prefix: mean_intensity, median_intensity, and
#'   sd_intensity, containing the statistical measures of pixel intensity
#'   within each bounding box.
#' @export
calculate_average_intensity <- function(img_df, colony_data, prefix = "bg") {

  processed_img <- img_df

  processed_img$intensity <- rgb_to_gray(processed_img$R, processed_img$G, processed_img$B)

  colony_data$mean_intensity <- NA
  colony_data$median_intensity <- NA
  colony_data$sd_intensity <- NA

  # Calculate background statistics for each bounding box
  for(i in 1:nrow(colony_data)) {

    # Extract pixels within current bounding box
    box_pixels <- processed_img[
      processed_img$x >= colony_data$new_xl[i] &
        processed_img$x <= colony_data$new_xr[i] &
        processed_img$y >= colony_data$new_yt[i] &
        processed_img$y <= colony_data$new_yb[i],
    ]

    if(nrow(box_pixels) > 0) {
      colony_data$mean_intensity[i] <- mean(box_pixels$intensity, na.rm = TRUE)
      colony_data$median_intensity[i] <- median(box_pixels$intensity, na.rm = TRUE)
      colony_data$sd_intensity[i] <- sd(box_pixels$intensity, na.rm = TRUE)
    } else {
      cat(sprintf("Warning: No pixels found in box %d\n", i))
    }
  }
  colony_data <- colony_data %>%
    rename_with(~ paste0(prefix, "_", .x),
                c(mean_intensity, median_intensity, sd_intensity))

  return(colony_data)
}
