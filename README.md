# HAIP: Halo Assay Image Processing

[![R-CMD-check](https://github.com/KevinYuann/HAIP/workflows/R-CMD-check/badge.svg)](https://github.com/KevinYuann/HAIP/actions)

## Overview

HAIP (Halo Assay Image Processing) is an R package that provides comprehensive 
tools for visualizing and analyzing plate images from BHET halo assays. This 
package enables researchers to efficiently process colony images and 
characterize bacterial activities as 'halo', 'clearing', or 'background' phenotypes.

The package is designed to work seamlessly with output from the 
[gitter package](https://cran.r-project.org/package=gitter), providing a 
complete workflow for automated halo assay analysis.

## Key Features

- **Multi-image processing**: Handle background, unwashed, and washed plate images simultaneously
- **Automated colony detection**: Integration with gitter package output for precise colony positioning
- **Quantitative analysis**: Calculate intensity measurements and background normalization
- **Flexible classification**: Classify pixels and colonies based on customizable thresholds
- **Reproducible workflow**: Standardized pipeline for consistent results across experiments

## Installation

Install the development version from GitHub:
```r
# Install from GitHub
if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
}
remotes::install_github("KevinYuann/HAIP")
```

## Dependencies
HAIP requires the following R packages:

- **dplyr** - Data manipulation and transformation
- **ggplot2** - Data visualization and plotting
- **readr** - Fast and friendly data import
- **gridExtra** - Arranging multiple grid-based plots
- **imager** - Image processing and manipulation

## Quick Start
Here's a basic workflow using HAIP:

```r
# 1. Initialize images and colony data files
prewashed_img <- load_image("Plate Images/unwashed/BHET25_8h_5_6.JPG")
washed_img <- load_image("Plate Images/washed/washed_BHET25_8h_5_6.JPG")
background_img <- load_image("Plate Images/background/background_BHET25_5_6.JPG")
colony_data <- initialize_colony_data("Plate Images/unwashed/BHET25_8h_5_6.JPG.dat", prewashed_img)

# 2. Process images and calculate measurements
colony_data <- create_grid_boxes(colony_data)
colony_data <- calculate_average_intensity(background_img, colony_data, prefix = "bg")

# 3. Perform background normalization and pixel classification
background_normalization <- calculate_edge_intensity(background_img, washed_img)$difference
label_pixels <- classify_pixels(washed_img, colony_data, background_normalization)$colony_summary
```

## Required File Structure
For optimal results, organize your data with 4 files per plate per timepoint:

Background image: Clean plate without colonies
Unwashed image: Plate with colonies before washing
Washed image: Plate with colonies after washing to reveal halos
DAT file: Gitter package output containing colony size and position data

## Function Reference
Image and Data Initialization

initialize_colony_data() - Import and structure colony data from gitter output files

Image Processing and Analysis

create_grid_boxes() - Generate spatial grid boxes around detected colonies
calculate_average_intensity() - Compute mean pixel intensity values within specified regions
calculate_edge_intensity() - Measure intensity differences at plate edges for normalization
classify_pixels() - Categorize pixels within colony boxes based on intensity thresholds

Visualization

plot_image() - Generates a plot of the actual plate with or without the colony grid
plot_edge_intensity() - Generates a histogram of the edge pixels for background normalization
plot_pixel_classficiation - Generates a plot of the plate showing each pixel's classification

