####################################
# global libraries used everywhere #
####################################

mran.date <- "2019-09-01"
options(repos=paste0("https://cran.microsoft.com/snapshot/",mran.date,"/"))



pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
  return("OK")
}

global.libraries <- c("dplyr","readstata13","data.table","limSolve","lmtest", "sandwich",
                      "stringr", "parallel", "multiwayvcov", "systemfit", "ggplot2",
                      "scales", "grid", "zoo", "tis", "tidyr", "RColorBrewer", "psych",
                      "rgeos", "rgdal", "maptools")

results <- sapply(as.list(global.libraries), pkgTest)

source("R:/Project/SeattleMinimumWage/Stata/AttenuationPaper/dofiles/AEJEP_submission_2020/code/functions.R")



