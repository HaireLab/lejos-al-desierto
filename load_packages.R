# RMRS 
# Author: Jamie Sanderlin
# Date: 3/24/2023

# Code purpose: create files for selecting CASC study sites
# Load packages (check if installed and if not install them)

################################################################################
## Load required packages
# Function from https://vbaliga.github.io/verify-that-r-packages-are-installed-and-loaded/
## First specify the packages of interest
packages = c("spsurvey","dplyr")

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)


