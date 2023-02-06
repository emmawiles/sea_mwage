# Master file to run all R scripts used to create the tables and figures in 
# Minimum Wages and Low Wage Employment: Evidence from Seattle
# by Jardim, Long, Plotnick, Van Inwegen, Vigdor, Wething 2021

##############################
### Set Up: Run this first ###
##############################

# Set base directory ******
setwd("R:\\Project\\SeattleMinimumWage\\Stata\\AttenuationPaper\\dofiles\\AEJEP_submission_2020\\code\\")
# Set base directory *******
path.to.root <- "R:\\Project\\SeattleMinimumWage\\Stata\\AttenuationPaper\\dofiles\\AEJEP_submission_2020\\"
# Data
path.to.data <- paste0(path.to.root,"data\\")
# Output
path.to.save <- paste0(path.to.root,"output\\")
# Load all packages and functions
source("packages_and_functions.r")

# Create NeighborPUMA dataset for analysis (used for Table 6)
source("Identify_neighbor_PUMAs.r")

###########################
### Tables and Figures  ###
###########################


# Create all auxilary datasets (and Table 6)
source("table_6.r")

# Create Table 5
source("table_5.r")

# Create Table 7
source("table_7.r")

# Create Table 10 Panel D
source("table_10d.r")

# Create Figure 3
source("figure_3.r")

# Create Appendix Table A2
source("table_a2.r")

# Create Appendix Table A4
source("table_a4.r")

# Create Appendix Table A5
source("table_a5.r")

# Create Appendix Table A6
source("table_a6.r")

# Create Appendix Figure's A1 and A2
source("figure_a1_a2.r")

# Create Appendix Figure's A4, A5, and A6
source("figure_a4_a5_a6.r")

# Create Appendix Figure's A9, A10, and A13
source("figure_a9_a10_a13.r")

# Create Appendix Table C1 and Figure C1
source("table_figure_c1.r")

