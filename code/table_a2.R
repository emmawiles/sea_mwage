##################################################
### Results in Levels, Synthetic Control Method
##################################################
# Stub to load the data
stub.DiD.bins <- "nworker_bins"

#################################################
###                                           ###
### Descriptive statistics on jobs in Seattle ###
###                                           ###
#################################################

# Wage bins to be used (note "12" means "jobs paying <= $12.99")	
wage.to.use <- c(12,18,40)

# Variable stubs	
var <- c("cum_nworkers_beg","cum_hours_flow","cum_mean_wagerate","cum_payroll_flow")

# Build a vector of outcomes to use in regressions
outcomes <- c()
for(i in var){
  for(j in wage.to.use){
    outcomes <- c(outcomes,paste0(i,j))
  }
}

########################################	
### Summary statistics for bins ###
### All industries                   ###
### Seattle and WA                   ###	
########################################

# Bins
# <13, 13-19, 19-25, 25-30, 30-35, 35-40, >40

# Read file 
cohort.series.DiD <- read.dta13(paste0(path.to.data,stub.DiD.bins,".dta", collapse = ""))
cohort.series.DiD <- data.table(cohort.series.DiD, key = c("region0","yearquarter"))
cohort.series.DiD <- melt(cohort.series.DiD, id.vars = c("region0","yearquarter"))
cohort.series.DiD <- data.table(cohort.series.DiD, key = c("region0","yearquarter","variable"))	

# Keep only time periods after SMWO passage
cohort.series.DiD <- cohort.series.DiD[yearquarter >= 20142 & yearquarter <= 20163, ]

# Variable for wage group and stub 
cohort.series.DiD[ , variable := gsub("\\_","",as.character(variable))]
cohort.series.DiD[ , variable := gsub("beg","",as.character(variable))]
cohort.series.DiD[ , variable := gsub("flow","",as.character(variable))]
cohort.series.DiD[ , w := as.integer(gsub("[a-z]*([0-9]+)","\\1",as.character(variable)))]
cohort.series.DiD[ , stub := gsub("([a-z]*)[0-9]+","\\1",as.character(variable))]

cohort.series.DiD[w < 13, w_coarse := 13]
cohort.series.DiD[w >= 13 & w < 19, w_coarse := 19]	
cohort.series.DiD[w >= 19 & w < 25, w_coarse := 25]	
cohort.series.DiD[w >= 25 & w < 30, w_coarse := 30]	
cohort.series.DiD[w >= 30 & w < 35, w_coarse := 35]	
cohort.series.DiD[w >= 35 & w < 40, w_coarse := 40]	
cohort.series.DiD[w >= 40 , w_coarse := 41]	

cohort.series.DiD.Seattle <- cohort.series.DiD[stub == "binnworkers" & region0 == 1, sum(value), by = c("yearquarter","w_coarse")]
setnames(cohort.series.DiD.Seattle,"V1","value")
cohort.series.DiD.WA <- cohort.series.DiD[stub == "binnworkers", sum(value), by = c("yearquarter","w_coarse")]
setnames(cohort.series.DiD.WA,"V1","value")

cohort.series.DiD.Seattle <- dcast(cohort.series.DiD.Seattle, yearquarter ~ w_coarse)
cohort.series.DiD.WA <- dcast(cohort.series.DiD.WA, yearquarter ~ w_coarse)

write.csv(cbind(cohort.series.DiD.Seattle,cohort.series.DiD.WA),paste0(path.to.save,"\\table_a2.csv"))
