
# HAIP

HAIP, an acronym for Halo Assay Image Processing, provides tools to visualize and analyze 
plate images from the BHET halo assay. The goal of this tool is to allow users to easily process
and characterize the activities of each colony as 'halo', 'clearing', or 'background'.

## Usage

This package is meant to be used with the output from the gitter package. Ideally,
there are 4 files for each plate at each timepoint: a background image, an unwashed
image, a washed image, and a .dat file (the output of the gitter package) which
gives information on the size and position of each colony.

## Example Pipeline

```r
# Initialize images and files
prewashed_img <- initialize_image("Plate Images/unwashed/BHET25_8h_5_6.JPG")
washed_img <- initialize_image("Plate Images/washed/washed_BHET25_8h_5_6.JPG")
background_img <- initialize_image("Plate Images/background/background_BHET25_5_6.JPG")
data <- initialize_colony_data("Plate Images/unwashed/BHET25_8h_5_6.JPG.dat", prewashed_img)

# Image Processing
colony_data <- create_grid_boxes(colony_data)
colony_data <- calculate_average_intensity(background_img, colony_data, prefix = "bg")
background_normalization <- calculate_edge_intensity(background_img, washed_img)$difference
label_pixels <- classify_pixels_by_box(washed_img, colony_data, background_normalization)$colony_summary
```

## Requirements

Required packages: magick, dplyr, ggplot2, readr, gridExtra


## Installation

```r
# from github
remotes::install_github("KevinYuann/HAIP")
```

