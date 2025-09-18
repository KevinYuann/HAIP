#' Plot Image with Colony Overlays
#'
#' Creates a visualization of an image with optional overlays showing colony
#' grids, center points, and row-column labels. Flexible display options
#' allow customization of what colony information to show on top of the
#' base image.
#'
#' @param img_df `data.frame`. Image data frame created by `initialize_image()`,
#'   containing pixel coordinates and color values.
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
plot_image <- function(img_df,
                       colony_data = NULL,
                       show_colony_grid = FALSE,
                       use_new_boxes = FALSE,
                       show_center_points = FALSE,
                       show_rowcol_labels = FALSE) {
  baseplot = ggplot() +
    geom_raster(data = img_df, aes(x = x, y = y, fill = value)) +
    scale_fill_identity() +
    theme_void() +
    theme(legend.position = "none")

  if (show_colony_grid) {
    if (use_new_boxes) {
      baseplot = baseplot + geom_rect(data = colony_data,
                                      aes(xmin = new_xl, xmax = new_xr, ymin = new_yt, ymax = new_yb),
                                      fill = NA, color = "red", linewidth = 0.8, alpha = 0.8)
    } else {
      baseplot = baseplot + geom_rect(data = colony_data,
                                      aes(xmin = xl, xmax = xr, ymin = yt, ymax = yb),
                                      fill = NA, color = "red", linewidth = 0.8, alpha = 0.8)
    }
  }

  if (show_center_points) {
    baseplot = baseplot + geom_point(data = colony_data,
                                     aes(x = x, y = y),
                                     color = "yellow", size = 1.0, alpha = 0.9)
  }

  if (show_rowcol_labels) {
    baseplot = baseplot + geom_text(data = data,
                                    aes(x = x, y = y, label = paste0(row, "-", col)),
                                    color = "white", size = 2, vjust = -1.2)
  }
  return(baseplot)
}
