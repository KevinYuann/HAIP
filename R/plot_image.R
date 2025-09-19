#' Plot Image with Colony Overlays
#'
#' Creates a visualization of an image with optional overlays showing colony
#' grids, center points, and row-column labels. Flexible display options
#' allow customization of what colony information to show on top of the
#' base image.
#'
#' @param img `cimg`. Image object loaded by `imager::load.image()`.
#' @param colony_data `data.frame` or `NULL`. Colony data with coordinates and
#'   bounding box information. If `NULL`, only the base image is plotted.
#' @param show_colony_grid `logical(1)`. If `TRUE`, displays colony bounding
#'   boxes as red rectangles. Default is `FALSE`.
#' @param use_new_boxes `logical(1)`. If `TRUE`, uses normalized bounding boxes
#'   (new_xl, new_xr, new_yt, new_yb) from `create_grid_boxes()`. If `FALSE`,
#'   uses original boxes (xl, xr, yt, yb). Default is `FALSE`.
#' @param show_center_points `logical(1)`. If `TRUE`, displays colony center
#'   points as yellow dots. Default is `FALSE`.
#' @param show_rowcol_labels `logical(1)`. If `TRUE`, displays row-column
#'   labels for each colony. Default is `FALSE`.
#'
#' @return A `ggplot2` object showing the image with selected overlays.
#' @export
plot_image <- function(img,
                              colony_data = NULL,
                              show_colony_grid = FALSE,
                              use_new_boxes = FALSE,
                              show_center_points = FALSE,
                              show_rowcol_labels = FALSE) {

  # Convert imager object to RGB data frame for plotting
  img_rgb <- as.data.frame(img)

  # Create RGB hex values for each pixel
  # Extract RGB values by color channel
  red_vals <- img_rgb$value[img_rgb$cc == 1]
  green_vals <- img_rgb$value[img_rgb$cc == 2]
  blue_vals <- img_rgb$value[img_rgb$cc == 3]

  # Create plotting data frame
  img_plot_data <- data.frame(
    x = img_rgb$x[img_rgb$cc == 1],
    y = img_rgb$y[img_rgb$cc == 1],
    red = red_vals,
    green = green_vals,
    blue = blue_vals
  )

  # Convert to hex colors for ggplot
  img_plot_data$hex_color <- rgb(img_plot_data$red,
                                 img_plot_data$green,
                                 img_plot_data$blue)

  # Create base plot
  baseplot <- ggplot() +
    geom_raster(data = img_plot_data, aes(x = x, y = y, fill = hex_color)) +
    scale_fill_identity() +
    scale_y_reverse() +  # Flip y-axis to match image orientation
    coord_fixed() +      # Maintain aspect ratio
    theme_void() +
    theme(legend.position = "none")

  # Add colony grid overlay
  if (show_colony_grid && !is.null(colony_data)) {
    if (use_new_boxes) {
      baseplot <- baseplot +
        geom_rect(data = colony_data,
                  aes(xmin = new_xl, xmax = new_xr, ymin = new_yb, ymax = new_yt),
                  fill = NA, color = "red", linewidth = 0.8, alpha = 0.8)
    } else {
      baseplot <- baseplot +
        geom_rect(data = colony_data,
                  aes(xmin = xl, xmax = xr, ymin = yb, ymax = yt),
                  fill = NA, color = "red", linewidth = 0.8, alpha = 0.8)
    }
  }

  # Add center points
  if (show_center_points && !is.null(colony_data)) {
    baseplot <- baseplot +
      geom_point(data = colony_data,
                 aes(x = x, y = y),
                 color = "yellow", size = 1.0, alpha = 0.9)
  }

  # Add row-column labels
  if (show_rowcol_labels && !is.null(colony_data)) {
    baseplot <- baseplot +
      geom_text(data = colony_data,
                aes(x = x, y = y, label = paste0(row, "-", col)),
                color = "white", size = 2, vjust = -1.2)
  }

  return(baseplot)
}
