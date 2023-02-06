# Data stubs 
stub <- "aggregate_establishments_cumulative_all"

stub.save <- "jobs_cumulative_all"
mode.changes = "divided_by_five"

#######################
### Define outcomes ###
#######################

wge = 18 # Jobs paying <$19

# Stubs for outcomes -- variables 
var <- c("cum_nworkers_beg","cum_mean_wagerate","cum_payroll_flow","cum_hours_flow")
# Note on variable notation:
# cum_hours_flow -- hours worked in jobs paying <=w.99, where w=18 for example
# cum_nworkers_beg -- number of beginning-of-quarter jobs paying <=w.99, where w=18 for example
# cum_mean_wagerate -- average wage rate (weighted by hours) in jobs paying <=w.99, where w=18 for example
# cum_payroll_flow -- total payroll to jobs paying <=w.99, where w=18 for example

# Build a vector of outcomes to use in regressions
outcomes <- c()
for(i in var){
  for(j in wge){
    outcomes <- c(outcomes,paste0(i,j))
  }
}

#############################
###                       ###
### Load the data         ###
### for synthetic control ###
### and interactive FE    ### 
###                       ###
#############################

cohort.series <- read.dta13(paste0(path.to.data,stub,".dta", collapse = ""))
cohort.series <- data.table(cohort.series, key = c("region0","yearquarter"))

# Melt for analysis
cohort.series <- melt(cohort.series, id.vars = c("region0","puma_id0","yearquarter","R"))
cohort.series <- data.table(cohort.series, key = c("region0","puma_id0","yearquarter","variable"))
cohort.series.N <- cohort.series[substr(variable,1,14)=="cum_hours_flow", list(variable,yearquarter,puma_id0,region0,value)]
cohort.series.N <- cohort.series.N[is.na(value)==FALSE, ]

# Wage cutoff
cohort.series[ , w := as.integer(gsub("[a-z\\_]+([0-9]+)","\\1",variable))]
cohort.series.N[ , w := as.integer(gsub("([a-z\\_]+)([0-9]+)","\\2",variable))]
cohort.series.N[ , stub_N := (gsub("([a-z\\_]+)([0-9]+)","\\1",variable))]
setnames(cohort.series.N,"value","N")
cohort.series.N[ , variable := NULL]

# Add year variable
cohort.series[ , year := floor(yearquarter/10) ]

# Drop if variable is not one of the outcomes of interest! 
cohort.series <- cohort.series[is.element(variable,outcomes), ]

cohort.series <- cohort.series[region0!=0 & region0!=9 & is.na(puma_id0)==F & puma_id0!=0, ]

# Region - PUMA ID
cohort.series[ , region_puma_id0 := region0 * 1e5 + puma_id0]	

# Drop 2005 & 20164 cohort -- N/A for flows
cohort.series <- cohort.series[yearquarter > 20061, ]
cohort.series <- cohort.series[yearquarter != 20164, ]

cohort.series[ , dY := value]

# Eliminate PUMAS with at least one missing obs. in any variable #
cohort.series[, max_dY := max(dY), by = "region_puma_id0"]
cohort.series[, n_dY := sum(is.na(dY)), by = c("region_puma_id0","variable")]
print("PUMAs with missing data:")
print(unique(cohort.series[is.na(max_dY), region_puma_id0]))
cohort.series <- cohort.series[is.na(max_dY)==F, ]
cohort.series$max_dY <- NULL

# Eliminate PUMAs codes with fewer than 10 observations (i.e. pre-policy + policy years)
cohort.series[ , n_obs := length(yearquarter), by = c("region_puma_id0","variable")]
cohort.series[ , max_n_obs := max(n_obs), by = "region_puma_id0"]	

max.obs = cohort.series[ , max(max_n_obs)] 
unique(cohort.series[max_n_obs < max.obs, region_puma_id0])
cohort.series <- cohort.series[max_n_obs==max.obs, ]
cohort.series$n_obs <- NULL
cohort.series$max_n_obs <- NULL	

print("Number of PUMAs for Synthetic Control and Interactive FE (incl. 5 PUMAs in Seattle):")
print(length(unique(cohort.series$region_puma_id0[is.element(cohort.series$region0,c(1,4,5))])))

# Merge lagged number of hours and lagged number of workers for averaging
cohort.series[ , stub_N := "cum_hours_flow"]
cohort.series <- merge(cohort.series, cohort.series.N, by = c("puma_id0","region0","yearquarter","w","stub_N"), all.x = TRUE, allow.cartesian = TRUE)

#######################################################
### To use contiguous PUMAs for SCM s.e.            ###
### List neighbors for each PUMA in WA              ###
### excl. King County outside of Seattle and SeaTac ###
#######################################################

# Load the file with all possible combinations of 
# 5 contiguous PUMAs in WA outside of King County 
AllCombinations.ContigPUMAs <- readRDS(file = paste0(path.to.data,"PUMA_unique_combinations.RDS"))

########################################
###                                  ###
###        Analysis Part 2           ###
### Synthetic Control                ###
### Using non-Seattle PUMAS in WA    ###
### as control regions               ###
###                                  ###
########################################	

###################################			  
### PUMAS excluding King County ###
###################################			  

# Keep only relevant variables
cohort.series.synth.exclKing <- cohort.series[region0 != 2 & region0 != 3 & region0!=9 ,
                                              c("variable","region0","region_puma_id0","yearquarter","dY", "N"), with = F]	

# Initialize results data.frames -- coefficients
results.synth.exclKing <- data.frame(variable = character(),
                                     method = character(),
                                     T = integer(),
                                     dY = double(), 
                                     coef = double(), 
                                     coef.w = double(),	  
                                     stringsAsFactors = FALSE)


weights.synth.exclKing <- data.frame(variable = character(), 
                                     region_puma_id0 = integer(),
                                     weights = double(),
                                     stringsAsFactors = FALSE)
# Set Treated periods
treated.periods <- c("20143","20144", "20151" ,"20152","20153","20154","20161","20162","20163")	

bootstrap.synth.exclKing.TE <- list()
bootstrap.synth.exclKing.TEcum <- list()
bootstrap.synth.exclKing.PUMAs <- list()
bootstrap.synth.exclKing.Resid <- list()

for (j in outcomes) {
  j_index = match(j,outcomes)
  
  print("ESTIMATING SYNTHETIC CONTROL TE USING PUMAS EXCLUDING KING COUNTY")
  print(paste("Working on variable:",j))
  
  ### How many PUMAS in Seattle? Average the outcomes to form 1 unit
  PUMAs.Seattle <- unique(cohort.series.synth.exclKing[variable == j & region0 == 1, region_puma_id0])
  # Employment variable? Then estimate wage effect as well
  est.elast = 0
  if (est.elast == 1) {
    var_w = paste0("d_cum_mean_wagerate",gsub("[a-z\\_]+([0-9]+)","\\1",j))
    df_w = cohort.series.synth.exclKing[variable == var_w,]
  }	
  
  if (est.elast == 0) {
    df_w = NULL
  }  
  
  # Get the estimates
  if (grepl("wage",j)) {
    results.synth.temp <- EstimateSynth(j,cohort.series.synth.exclKing[variable == j,],
                                        df_w,est.elast,"Synth_exclKing",PUMAs.Seattle, AllCombinations.ContigPUMAs, treated.periods,
                                        aggr.method = "average", standardize = FALSE)
  }										
  if (grepl("wage",j)==FALSE) {
    results.synth.temp <- EstimateSynth(j,cohort.series.synth.exclKing[variable == j,],
                                        df_w,est.elast,"Synth_exclKing",PUMAs.Seattle, AllCombinations.ContigPUMAs, treated.periods,
                                        aggr.method = "SumDividedByFive", standardize = FALSE)
  }
  
  # Re-shape p-values
  temp <- results.synth.temp$coeff %>% select(!starts_with("pval"))
  pval_wide <- results.synth.temp$coeff %>% select(starts_with("pval"))
  
  #Create vectors of pvalues
  pval <- vector()
  pval_rmspe <- vector()
  pval.cum <- vector()
  
  for (i in 1:9) {
    pval[i] <- pval_wide[1, i]
    pval_rmspe[i] <- pval_wide[1, 9 + i]
    pval.cum[i] <- pval_wide[1, 18 + i] }
  
  #Merge with coefficients 
  pval_long <- data.frame(T = seq(1:9), pval, pval_rmspe, pval.cum)
  results.synth.temp$coeff <- merge(temp, pval_long, by = "T")
  
  # Save the estimates with other variables
  results.synth.exclKing <- rbind(results.synth.exclKing, results.synth.temp$coeff)	    
  weights.synth.exclKing <- rbind(weights.synth.exclKing, results.synth.temp$weights)
}	

saveRDS(results.synth.exclKing,paste0(path.to.save,stub.save,"_synth_exclKing_",mode.changes,".Rds"))			
saveRDS(weights.synth.exclKing,paste0(path.to.save,stub.save,"_synth_weights_exclKing_",mode.changes,".Rds"))			

#############################################################################
### Save all estimation results in one RDS file to use in future analysis ###
#############################################################################

results.coefs.all <- results.synth.exclKing	
saveRDS(results.coefs.all, file = paste0(path.to.save,stub.save,"_coef_CI_",mode.changes,".Rds"))
print(paste("File",paste0(path.to.save,stub.save,"_coef_CI_",mode.changes,".Rds"),"saved"))

#################################
### Levels Columns of Table 6 ###
#################################

# Why are we using pval.cum here? In paper we report just pval
results.coefs.best <- results.coefs.all
setnames(results.coefs.best,"variable","variable_name")
results.coefs.best <- data.table(results.coefs.best, key = c("method","variable_name","T"))
results.coefs.best$sig[results.coefs.best$pval.cum > 0.1] <- ""
results.coefs.best$sig[results.coefs.best$pval.cum <= 0.1] <- "*"
results.coefs.best$sig[results.coefs.best$pval.cum <= 0.05] <- "**"
results.coefs.best$sig[results.coefs.best$pval.cum <= 0.01] <- "***"

results.coefs.best[ , coefcum_text := paste0(as.character(round(coefcum,3)),sig)]
results.coefs.best[ , pvalcum_text := paste0("[",as.character(round(pval.cum,3)),"]")]	

table.to.save <- results.coefs.best[ , c("variable_name","method","T","coefcum_text","pvalcum_text")]
table.to.save <- melt(table.to.save, id.vars = c("variable_name","method","T"))
table.to.save <- dcast(table.to.save, T + variable ~ variable_name + method)  

table.to.save.addl <- unique(results.coefs.best[grepl("18",variable_name) & is.element(method,c("Synth_exclKing","Int_FE_exlKing")), c("variable_name","method","RMSE","Obs")])
table.to.save.addl[ , RMSE := as.character(round(RMSE,3))]
table.to.save.addl[is.na(RMSE) , RMSE := ""]
table.to.save.addl[ , T := 10 ]
table.to.save.addl <- melt(table.to.save.addl, id.vars = c("variable_name","method","T"))
table.to.save.addl[variable=="RMSE"  , T := 22 ]
table.to.save.addl[variable=="Obs"  , T := 23 ]
table.to.save.addl <- dcast(table.to.save.addl, T + variable ~  variable_name + method)  

write.csv(rbind(as.matrix(table.to.save),as.matrix(table.to.save.addl)), file = paste0(path.to.save,"table_6_",mode.changes,".csv"))	

