#' Plot Point Spread Function
#'
#' Creates visualizations of the PSF including a 2D heatmap and radial profile,
#' along with diagnostic metrics for quality assessment.
#'
#' @param psf `matrix`. Point spread function to visualize.
#'
#' @return A `list` containing ggplot objects (heatmap, radial profile) and a
#'   data frame of diagnostic metrics.
#' @export
#'
plot_psf <- function(psf) {
  library(ggplot2)
  library(reshape2)

  psf_df <- melt(psf)
  colnames(psf_df) <- c("x", "y", "value")

  p1 <- ggplot(psf_df, aes(x = x, y = y, fill = value)) +
    geom_tile() +
    scale_fill_viridis_c() +
    coord_equal() +
    theme_minimal() +
    labs(title = "Point Spread Function (2D)", fill = "Intensity")

  # Radial profile
  center_x <- ceiling(nrow(psf) / 2)
  center_y <- ceiling(ncol(psf) / 2)

  radial_data <- do.call(rbind, lapply(1:nrow(psf), function(i) {
    lapply(1:ncol(psf), function(j) {
      data.frame(
        distance = sqrt((i - center_x)^2 + (j - center_y)^2),
        intensity = psf[i, j]
      )
    })
  }))
  radial_data <- do.call(rbind, radial_data)
  radial_data$dist_bin <- round(radial_data$distance)
  radial_summary <- aggregate(intensity ~ dist_bin, radial_data, mean)

  p2 <- ggplot(radial_summary, aes(x = dist_bin, y = intensity)) +
    geom_line(color = "blue", size = 1) +
    geom_point(color = "blue", size = 2) +
    theme_minimal() +
    labs(title = "PSF Radial Profile",
         x = "Distance from Center (pixels)",
         y = "Mean Intensity",
         subtitle = "Should decay smoothly from center")

  diagnostics <- data.frame(
    Metric = c("Peak Value", "Center Value", "Sum (should be ~1)",
               "FWHM (pixels)", "Symmetry Score"),
    Value = c(
      sprintf("%.4f", max(psf)),
      sprintf("%.4f", psf[center_x, center_y]),
      sprintf("%.4f", sum(psf)),
      sprintf("%.1f", calculate_fwhm(radial_summary)),
      sprintf("%.3f", assess_psf_symmetry(psf))
    )
  )

  return(list(heatmap = p1, radial = p2, diagnostics = diagnostics))
}
