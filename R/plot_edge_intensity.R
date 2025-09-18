#' Create Edge Intensity Histograms with Optional Normalization
#'
#' Generates histograms comparing edge pixel intensities between two images
#' with optional background normalization. Can either compare original images
#' directly or show the effect of normalization on the second image. Useful
#' for visualizing background correction and intensity distributions.
#'
#' @param img_df1 `data.frame`. First image data frame (typically background)
#'   with columns `x`, `y`, `R`, `G`, `B` created by `initialize_image()`.
#' @param img_df2 `data.frame`. Second image data frame (typically washed)
#'   with columns `x`, `y`, `R`, `G`, `B` created by `initialize_image()`.
#' @param edge_width `numeric(1)`. Width of edge region in pixels from the
#'   outer border. Default is 150.
#' @param img1_name `character(1)`. Display name for first image.
#'   Default is "Background".
#' @param img2_name `character(1)`. Display name for second image.
#'   Default is "Washed".
#' @param normalize_background `logical(1)`. If `FALSE`, compares original images.
#'   If `TRUE`, shows normalization effect on second image. Default is `FALSE`.
#'
#' @return A `list` containing:
#'   \itemize{
#'     \item `plot`: ggplot2 comparison histogram
#'     \item `statistics`: Statistical summaries of intensities
#'     \item `normalized_image`: Normalized image data frame (only if normalize_background = TRUE)
#'   }
#' @export
plot_edge_intensity <- function(img_df1, img_df2, edge_width = 150,
                                      img1_name = "Background", img2_name = "Washed",
                                      normalize_background = FALSE) {
  # Helper function to extract edge pixels and return intensity data
  extract_edge_intensities <- function(img_df, width) {
    max_x <- max(img_df$x)
    max_y <- max(img_df$y)

    edge_pixels <- img_df %>%
      filter(
        x <= width |                    # Left edge
          x >= (max_x - width + 1) |     # Right edge
          y <= width |                   # Bottom edge
          y >= (max_y - width + 1)      # Top edge
      ) %>%
      mutate(intensity = rgb_to_gray(R, G, B))

    return(edge_pixels$intensity)
  }

  # Validate inputs
  required_cols <- c("x", "y", "R", "G", "B")
  if (!all(required_cols %in% names(img_df1)) || !all(required_cols %in% names(img_df2))) {
    stop("Both data frames must have columns: x, y, R, G, B")
  }

  # Extract edge pixel intensities
  intensities1 <- extract_edge_intensities(img_df1, edge_width)
  intensities2 <- extract_edge_intensities(img_df2, edge_width)

  # Calculate statistics for original images
  stats1 <- list(
    mean = mean(intensities1, na.rm = TRUE),
    median = median(intensities1, na.rm = TRUE),
    sd = sd(intensities1, na.rm = TRUE),
    n = length(intensities1)
  )

  stats2 <- list(
    mean = mean(intensities2, na.rm = TRUE),
    median = median(intensities2, na.rm = TRUE),
    sd = sd(intensities2, na.rm = TRUE),
    n = length(intensities2)
  )

  if (!normalize_background) {
    # Compare background vs washed (before normalization)
    cat("=== BACKGROUND vs WASHED COMPARISON ===\n")
    cat(sprintf("%s Image: Mean=%.2f, Median=%.2f, SD=%.2f, n=%d\n",
                img1_name, stats1$mean, stats1$median, stats1$sd, stats1$n))
    cat(sprintf("%s Image: Mean=%.2f, Median=%.2f, SD=%.2f, n=%d\n",
                img2_name, stats2$mean, stats2$median, stats2$sd, stats2$n))
    cat(sprintf("Mean difference: %.2f\n\n", stats1$mean - stats2$mean))

    comparison_data <- data.frame(
      intensity = c(intensities1, intensities2),
      image = factor(c(rep(img1_name, length(intensities1)),
                       rep(img2_name, length(intensities2))),
                     levels = c(img1_name, img2_name))
    )

    # Create individual plots
    plot1 <- ggplot(data.frame(intensity = intensities1), aes(x = intensity)) +
      geom_histogram(bins = 30, fill = "lightblue", alpha = 0.7, color = "black") +
      geom_vline(xintercept = stats1$mean, color = "darkblue", linetype = "dashed", size = 1) +
      geom_vline(xintercept = stats1$median, color = "blue", linetype = "dotted", size = 1) +
      labs(title = paste(img1_name, "Image"),
           subtitle = paste("Mean:", round(stats1$mean, 2), "| Median:", round(stats1$median, 2),
                            "| SD:", round(stats1$sd, 2)),
           x = "Intensity (0-255)", y = "Pixel Count") +
      theme_minimal() +
      theme(plot.title = element_text(size = 12))

    plot2 <- ggplot(data.frame(intensity = intensities2), aes(x = intensity)) +
      geom_histogram(bins = 30, fill = "lightcoral", alpha = 0.7, color = "black") +
      geom_vline(xintercept = stats2$mean, color = "darkred", linetype = "dashed", size = 1) +
      geom_vline(xintercept = stats2$median, color = "red", linetype = "dotted", size = 1) +
      labs(title = paste(img2_name, "Image"),
           subtitle = paste("Mean:", round(stats2$mean, 2), "| Median:", round(stats2$median, 2),
                            "| SD:", round(stats2$sd, 2)),
           x = "Intensity (0-255)", y = "Pixel Count") +
      theme_minimal() +
      theme(plot.title = element_text(size = 12))

    comparison_plot <- grid.arrange(plot1, plot2, ncol = 2,
                                    top = paste("Background vs Washed Comparison | Mean difference:",
                                                round(stats1$mean - stats2$mean, 2)))

    return(list(
      plot = comparison_plot,
      statistics = list(
        background_stats = stats1,
        washed_stats = stats2,
        difference = stats1$mean - stats2$mean
      )
    ))

  } else {
    # Perform normalization and compare original vs normalized washed

    # Calculate normalization factor
    normalization_factor <- stats1$mean - stats2$mean

    # Create normalized washed image
    normalized_img2 <- img_df2
    normalized_img2$R <- pmax(0, pmin(255, img_df2$R + normalization_factor))
    normalized_img2$G <- pmax(0, pmin(255, img_df2$G + normalization_factor))
    normalized_img2$B <- pmax(0, pmin(255, img_df2$B + normalization_factor))

    # Extract normalized edge intensities
    normalized_intensities2 <- extract_edge_intensities(normalized_img2, edge_width)

    # Calculate normalized statistics
    normalized_stats2 <- list(
      mean = mean(normalized_intensities2, na.rm = TRUE),
      median = median(normalized_intensities2, na.rm = TRUE),
      sd = sd(normalized_intensities2, na.rm = TRUE),
      n = length(normalized_intensities2)
    )

    comparison_data <- data.frame(
      intensity = c(intensities2, normalized_intensities2),
      type = factor(c(rep("Original", length(intensities2)),
                      rep("Normalized", length(normalized_intensities2))),
                    levels = c("Original", "Normalized"))
    )

    # Create individual plots
    plot1 <- ggplot(data.frame(intensity = intensities2), aes(x = intensity)) +
      geom_histogram(bins = 30, fill = "lightcoral", alpha = 0.7, color = "black") +
      geom_vline(xintercept = stats2$mean, color = "darkred", linetype = "dashed", size = 1) +
      geom_vline(xintercept = stats2$median, color = "red", linetype = "dotted", size = 1) +
      labs(title = paste("Original", img2_name),
           subtitle = paste("Mean:", round(stats2$mean, 2), "| Median:", round(stats2$median, 2),
                            "| SD:", round(stats2$sd, 2)),
           x = "Intensity (0-255)", y = "Pixel Count") +
      theme_minimal() +
      theme(plot.title = element_text(size = 12))

    plot2 <- ggplot(data.frame(intensity = normalized_intensities2), aes(x = intensity)) +
      geom_histogram(bins = 30, fill = "darkgreen", alpha = 0.7, color = "black") +
      geom_vline(xintercept = normalized_stats2$mean, color = "darkgreen", linetype = "dashed", size = 1) +
      geom_vline(xintercept = normalized_stats2$median, color = "green", linetype = "dotted", size = 1) +
      labs(title = paste("Normalized", img2_name),
           subtitle = paste("Mean:", round(normalized_stats2$mean, 2), "| Median:", round(normalized_stats2$median, 2),
                            "| SD:", round(normalized_stats2$sd, 2)),
           x = "Intensity (0-255)", y = "Pixel Count") +
      theme_minimal() +
      theme(plot.title = element_text(size = 12))

    comparison_plot <- grid.arrange(plot1, plot2, ncol = 2,
                                    top = paste("Washed Image: Original vs Normalized | Shift:",
                                                round(normalization_factor, 2)))

    return(list(
      plot = comparison_plot,
      normalized_image = normalized_img2,
      statistics = list(
        original_washed_stats = stats2,
        normalized_washed_stats = normalized_stats2,
        normalization_factor = normalization_factor
      )
    ))
  }
}
