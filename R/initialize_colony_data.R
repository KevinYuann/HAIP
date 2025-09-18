#' Initialize Colony Data from .dat File
#'
#' Reads a .dat file containing colony data and converts it to a data frame format.
#' Automatically skips header lines that start with "#" and adjusts y-coordinates
#' to match image coordinate system (top-left origin, y increasing downwards).
#'
#' @param path_name `character(1)`. Path to the .dat file to be read and
#'   processed. The file should be tab-separated with optional header lines
#'   starting with "#".
#' @param image_df `data.frame`. Image data frame created by `initialize_image()`.
#'   Used to determine the image height for coordinate transformation.
#'
#' @return A `data.frame` containing the colony data with y-coordinates flipped
#'   to match image orientation (y increases downwards from top-left origin).
#' @export
initialize_colony_data <- function(path_name, image_df) {
  con <- file(path_name, "r")
  lines_to_skip <- 0

  # Read lines one by one until we find a line that doesn't start with "#"
  while(TRUE) {
    line <- readLines(con, n = 1)
    if(length(line) == 0) break  # End of file
    if(startsWith(line, "#")) {
      lines_to_skip <- lines_to_skip + 1
    } else {
      break  # Found first non-header line
    }
  }
  close(con)

  data <- read_tsv(path_name, skip = lines_to_skip - 1, show_col_types = FALSE)
  colnames(data)[1] <- "row"

  image_height <- max(image_df$y)

  # Flip y-coordinates to match image coordinate system
  # Convert from bottom-left origin to top-left origin
  if("y" %in% colnames(data)) {
    data$y <- image_height - data$y
  }

  return(data)
}
