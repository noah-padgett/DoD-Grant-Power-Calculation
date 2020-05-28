# ============================================= #
# script: load_packages.R
# Project: DoD Grant Power Calculation
# Author(s): P. Gordon et al.
# ============================================= #
# Date Created: 2020-05-26
# Date Modified: 2020-05-26
# By: R. Noah Padgett
# ============================================= #
# ============================================= #
# Purpose:
# This R script is for loading all necessary
#   R packages
#
# No output - just loading packages into the
#   environment
# ============================================= #
# Set up directory and libraries
rm(list=ls())

# list of packages
packages <- c("tidyverse", "readr", "readxl", "forcats",
              "data.table", "ggplot2",
              "kableExtra", "xtable", "gridExtra", "viridis",
              "patchwork", "dplyr")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# Load packages
lapply(packages, library, character.only = TRUE)

w.d <- getwd()

