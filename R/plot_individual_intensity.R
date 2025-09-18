#' Create Histograms for Specific Row-Column Combinations
#'
#' Generates pixel intensity histograms for colonies at specified row-column
#' positions. Creates individual histograms showing intensity distributions
#' with statistical overlays (mean, median, standard deviation lines).
#' Useful for visualizing intensity patterns in specific colonies of interest.
#'
#' @param img_df `data.frame`. Image data frame created by `initialize_image()`,
#'   containing pixel coordinates and RGB color values.
#' @param colony_data `data.frame`. Colony data with bounding box coordinates
#'   from `create_grid_boxes()`.
#' @param target_row `character(1)` or `numeric(1)`. The row identifier of
#'   colonies to analyze.
#' @param target_cols `vector`. Vector of column identifiers to create
#'   histograms for within the specified row.
#'
#' @return A `list` containing:
#'   \itemize{
#'     \item `individual_plots`: Named list of individual ggplot histogram objects
#'     \item `combined_plot`: Combined plot object showing all histograms together
#'   }
#'   Returns `NULL` if no valid histograms could be created.
#' @export
plot_individual_intensity <- function(img_df, colony_data, target_row, target_cols) {

  processed_img <- img_df

  processed_img$intensity <- rgb_to_gray(processed_img$R, processed_img$G, processed_img$B)

  histogram_plots <- list()

  for(col in target_cols) {

    # Find the specific colony
    colony_match <- colony_data[colony_data$row == target_row & colony_data$col == col, ]

    if(nrow(colony_match) == 0) {
      cat(sprintf("Warning: No colony found for row %s, col %s\n", target_row, col))
      next
    }

    if(nrow(colony_match) > 1) {
      cat(sprintf("Warning: Multiple colonies found for row %s, col %s. Using first match.\n", target_row, col))
      colony_match <- colony_match[1, ]
    }

    # Extract pixels within the bounding box
    colony_pixels <- processed_img[
      processed_img$x >= colony_match$new_xl &
        processed_img$x <= colony_match$new_xr &
        processed_img$y >= colony_match$new_yt &
        processed_img$y <= colony_match$new_yb,
    ]


    if(nrow(colony_pixels) == 0) {
      cat(sprintf("Warning: No pixels found in bounding box for row %s, col %s\n", target_row, col))
      next
    }

    # Calculate statistics
    mean_intensity <- mean(colony_pixels$intensity, na.rm = TRUE)
    median_intensity <- median(colony_pixels$intensity, na.rm = TRUE)
    sd_intensity <- sd(colony_pixels$intensity, na.rm = TRUE)
    min_intensity <- min(colony_pixels$intensity, na.rm = TRUE)
    max_intensity <- max(colony_pixels$intensity, na.rm = TRUE)

    cat(sprintf("  Intensity stats - Mean: %.2f, Median: %.2f, SD: %.2f, Range: %.2f-%.2f\n",
                mean_intensity, median_intensity, sd_intensity, min_intensity, max_intensity))

    # Create histogram
    colony_id <- paste0("Row ", target_row, ", Col ", col)

    hist_plot <- ggplot(colony_pixels, aes(x = intensity)) +
      geom_histogram(bins = 30, fill = "lightblue", alpha = 0.7, color = "black") +
      geom_vline(xintercept = mean_intensity, color = "red", linetype = "dashed", size = 1) +
      geom_vline(xintercept = median_intensity, color = "blue", linetype = "dashed", size = 1) +
      geom_vline(xintercept = mean_intensity - sd_intensity, color = "orange", linetype = "dotted") +
      geom_vline(xintercept = mean_intensity + sd_intensity, color = "orange", linetype = "dotted") +
      labs(title = paste("Pixel Intensity Distribution -", colony_id),
           subtitle = paste("Mean:", round(mean_intensity, 2),
                            "| Median:", round(median_intensity, 2),
                            "| SD:", round(sd_intensity, 2),
                            "| n =", nrow(colony_pixels), "pixels"),
           x = "Intensity (0-255)",
           y = "Pixel Count") +
      theme_minimal() +
      theme(plot.title = element_text(size = 12),
            plot.subtitle = element_text(size = 10))

    # Store the plot
    histogram_plots[[colony_id]] <- hist_plot
  }

  # Create combined plot if we have multiple histograms
  if(length(histogram_plots) > 1) {
    cat("Creating combined histogram plot...\n")
    combined_plot <- do.call(grid.arrange, c(histogram_plots, ncol = length(histogram_plots)))

    return(list(
      individual_plots = histogram_plots,
      combined_plot = combined_plot
    ))
  } else if(length(histogram_plots) == 1) {
    return(list(
      individual_plots = histogram_plots,
      combined_plot = histogram_plots[[1]]
    ))
  } else {
    cat("No valid histograms were created.\n")
    return(NULL)
  }
}
