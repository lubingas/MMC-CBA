# R/00_packages.R
# packages the model uses
# install.packages(c("readxl", "dplyr", "MASS", "ggplot2", "scales", "openxlsx"))
library(readxl)    # read_excel()
library(dplyr)     # filter(), pull()
library(ggplot2)   # figures in main.R
library(scales)    # dollar axis labels
# MASS::mvrnorm() is called with its package name, so MASS::select doesn't mask dplyr::select
