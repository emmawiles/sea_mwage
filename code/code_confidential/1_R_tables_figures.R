# Master file to run all R scripts used to create the tables and figures in 
# Minimum Wages and Low Wage Employment: Evidence from Seattle
# by Jardim, Long, Plotnick, Van Inwegen, Vigdor, Wething 2021

##############################
### Set Up: Run this first ###
##############################

# Set base directory ******
setwd("R:\\Project\\SeattleMinimumWage\\Stata\\AttenuationPaper\\dofiles\\AEJEP_submission_2020\\code\\code_confidential\\")
# Set base directory *******
path.to.root <- "R:\\Project\\SeattleMinimumWage\\Stata\\AttenuationPaper\\dofiles\\AEJEP_submission_2020\\"
# Data
path.to.data <- paste0(path.to.root,"data\\data_confidential\\")
# Output
path.to.save <- paste0(path.to.root,"output\\output_confidential\\")


# Load all packages and functions
source("packages_and_functions.r")


###########################
### Tables and Figures  ###
###########################

# Create Figure 1
source("figure_1.r")

# Create Figure 2
source("figure_2.r")

# Create data for Appendix Figure A11
source("figure_a11_a.r")

# Create Appendix Table D1
source("table_d1.r")
