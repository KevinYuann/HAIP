#' Classify Pixels by Colony Bounding Box
#'
#' Classifies pixels within each colony's bounding box as "clearing", "background",
#' or "halo" based on intensity thresholds derived from each individual box's background statistics.
#' Uses mean ± 2 standard deviations as classification thresholds. Requires
#' colony data with background intensity statistics.
#'
#' @param img_df `data.frame`. Image data frame created by `initialize_image()`,
#'   containing pixel coordinates and RGB color values.
#' @param colony_data_with_bg `data.frame`. Colony data that includes background
#'   intensity statistics (bg_mean_intensity, bg_sd_intensity) from
#'   `calculate_average_intensity()`.
#' @param background_normalization `numeric(1)`. Value to add to all pixel
#'   intensities for normalization purposes. Calculated by
#'   `edge_intensity_difference()$difference`.
#'
#' @return A `list` containing three elements:
#'   \itemize{
#'     \item `classified_data`: Image data frame with added `label` and `colony_id` columns
#'     \item `colony_summary`: Summary statistics for each colony including pixel counts and percentages
#'     \item `total_stats`: Overall statistics across all colonies
#'   }
#' @export
classify_pixels_by_box <- function(img_df, colony_data_with_bg, background_normalization) {

  processed_img <- img_df

  processed_img$intensity <- rgb_to_gray(processed_img$R, processed_img$G, processed_img$B) + background_normalization

  # Initialize classification columns
  processed_img$label <- "outside"  # Default for pixels outside any bounding box
  processed_img$colony_id <- NA     # Track which colony each pixel belongs to

  # Initialize summary statistics
  total_classified <- 0
  classification_summary <- data.frame(
    colony_id = character(),
    row = character(),
    col = character(),
    clearing_count = integer(),
    background_count = integer(),
    halo_count = integer(),
    total_pixels = integer(),
    clearing_pct = numeric(),
    background_pct = numeric(),
    halo_pct = numeric(),
    mean_intensity = numeric(),
    sd_intensity = numeric(),
    clearing_mean_intensity = numeric(),
    background_mean_intensity = numeric(),
    halo_mean_intensity = numeric(),
    stringsAsFactors = FALSE
  )

  # Process each bounding box
  for(i in 1:nrow(colony_data_with_bg)) {

    # Check if any bounding box coordinates are NA
    if(any(is.na(c(colony_data_with_bg$new_xl[i],
                   colony_data_with_bg$new_xr[i],
                   colony_data_with_bg$new_yt[i],
                   colony_data_with_bg$new_yb[i])))) {

      cat(sprintf("  Warning: Bounding box coordinates contain NA values for colony %s-%s. Skipping...\n",
                  colony_data_with_bg$row[i], colony_data_with_bg$col[i]))

      # Set box_pixels_count to 0 and add empty summary row
      box_pixels_count <- 0

      # Add to summary with zeros
      classification_summary <- rbind(classification_summary, data.frame(
        colony_id = paste0(colony_data_with_bg$row[i], "-", colony_data_with_bg$col[i]),
        row = colony_data_with_bg$row[i],
        col = colony_data_with_bg$col[i],
        clearing_count = 0,
        background_count = 0,
        halo_count = 0,
        total_pixels = 0,
        clearing_pct = 0,
        background_pct = 0,
        halo_pct = 0,
        mean_intensity = ifelse(is.na(colony_data_with_bg$bg_mean_intensity[i]), 0, colony_data_with_bg$bg_mean_intensity[i]),
        sd_intensity = ifelse(is.na(colony_data_with_bg$bg_sd_intensity[i]), 0, colony_data_with_bg$bg_sd_intensity[i]),
        clearing_mean_intensity = ifelse(is.na(colony_data_with_bg$bg_sd_intensity[i]), 0, colony_data_with_bg$bg_sd_intensity[i]),
        background_mean_intensity = ifelse(is.na(colony_data_with_bg$bg_sd_intensity[i]), 0, colony_data_with_bg$bg_sd_intensity[i]),
        halo_mean_intensity = ifelse(is.na(colony_data_with_bg$bg_sd_intensity[i]), 0, colony_data_with_bg$bg_sd_intensity[i]),
        stringsAsFactors = FALSE
      ))

      next  # Skip to next iteration
    }

    # Get current colony's background statistics
    bg_mean <- colony_data_with_bg$bg_mean_intensity[i]
    bg_sd <- colony_data_with_bg$bg_sd_intensity[i]

    # Calculate thresholds (mean ± 2 standard deviation)
    lower_threshold <- bg_mean - 2 * bg_sd  # clearing threshold
    upper_threshold <- bg_mean + 2 * bg_sd  # halo threshold

    # Find pixels within current bounding box
    box_mask <- processed_img$x >= colony_data_with_bg$new_xl[i] &
      processed_img$x <= colony_data_with_bg$new_xr[i] &
      processed_img$y >= colony_data_with_bg$new_yt[i] &
      processed_img$y <= colony_data_with_bg$new_yb[i]

    box_pixels_count <- sum(box_mask)

    if(box_pixels_count > 0) {
      # Assign colony ID
      processed_img$colony_id[box_mask] <- paste0(colony_data_with_bg$row[i], "-", colony_data_with_bg$col[i])

      # Classify pixels based on intensity relative to this colony's background
      box_intensities <- processed_img$intensity[box_mask]

      # Assign labels
      processed_img$label[box_mask] <- ifelse(
        box_intensities < lower_threshold, "clearing",
        ifelse(box_intensities > upper_threshold, "halo", "background")
      )

      # Calculate statistics for this colony
      colony_labels <- processed_img$label[box_mask]
      clearing_count <- sum(colony_labels == "clearing")
      background_count <- sum(colony_labels == "background")
      halo_count <- sum(colony_labels == "halo")

      # Calculate mean intensities for each classification type
      clearing_mean_intensity <- ifelse(clearing_count > 0,
                                        mean(box_intensities[colony_labels == "clearing"]),
                                        NA)
      background_mean_intensity <- ifelse(background_count > 0,
                                          mean(box_intensities[colony_labels == "background"]),
                                          NA)
      halo_mean_intensity <- ifelse(halo_count > 0,
                                    mean(box_intensities[colony_labels == "halo"]),
                                    NA)

      # Add to summary
      classification_summary <- rbind(classification_summary, data.frame(
        colony_id = paste0(colony_data_with_bg$row[i], "-", colony_data_with_bg$col[i]),
        row = colony_data_with_bg$row[i],
        col = colony_data_with_bg$col[i],
        clearing_count = clearing_count,
        background_count = background_count,
        halo_count = halo_count,
        total_pixels = box_pixels_count,
        clearing_pct = round(clearing_count / box_pixels_count * 100, 1),
        background_pct = round(background_count / box_pixels_count * 100, 1),
        halo_pct = round(halo_count / box_pixels_count * 100, 1),
        mean_intensity = bg_mean,
        sd_intensity = bg_sd,
        clearing_mean_intensity = clearing_mean_intensity,
        background_mean_intensity = background_mean_intensity,
        halo_mean_intensity = halo_mean_intensity,
        stringsAsFactors = FALSE
      ))

      total_classified <- total_classified + box_pixels_count

    } else {
      cat(sprintf("  Warning: No pixels found in bounding box for colony %s-%s\n",
                  colony_data_with_bg$row[i], colony_data_with_bg$col[i]))
    }
  }

  # Convert label to factor for better plotting
  processed_img$label <- factor(processed_img$label,
                                levels = c("clearing", "background", "halo", "outside"))

  # Overall percentages across all colonies
  overall_clearing <- sum(classification_summary$clearing_count)
  overall_background <- sum(classification_summary$background_count)
  overall_halo <- sum(classification_summary$halo_count)

  # Calculate overall mean intensities across all colonies
  overall_clearing_mean <- mean(classification_summary$clearing_mean_intensity, na.rm = TRUE)
  overall_background_mean <- mean(classification_summary$background_mean_intensity, na.rm = TRUE)
  overall_halo_mean <- mean(classification_summary$halo_mean_intensity, na.rm = TRUE)

  return(list(
    classified_data = processed_img,
    colony_summary = classification_summary,
    total_stats = list(
      clearing_count = clearing_count,
      background_count = background_count,
      halo_count = halo_count,
      total_pixels = box_pixels_count,
      total_pixels = total_classified,
      clearing_total = overall_clearing,
      background_total = overall_background,
      halo_total = overall_halo,
      overall_clearing_mean_intensity = overall_clearing_mean,
      overall_background_mean_intensity = overall_background_mean,
      overall_halo_mean_intensity = overall_halo_mean
    )
  ))
}
