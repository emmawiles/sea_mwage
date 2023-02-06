# Data stubs 
stub <- "establishments_samplesize100synth_bin_prep_all"
stub.DiD <- "establishments_samplesize100DiD_bins_prep_all"
stub.save <- "appendix_figure_a11"
mode.changes = "growth_rates"

############################
### Choose specification ###
############################


#######################
### Define outcomes ###
#######################

# Wage bins -- from jobs paying (<=$9.99) to (<=$18.99)
wge = seq(9, 40) 

# Stubs for outcomes -- variables 
var <- c("d_bin_hours_flow")

# Build a vector of outcomes to use in regressions
outcomes <- c()
for(i in var){
  for(j in wge){
    outcomes <- c(outcomes,paste0(i,j))
  }
}

# Load Did data for Seattle averages
cohort.series.DiD <- read.dta13(paste0(path.to.data,stub.DiD,".dta", collapse = ""))
cohort.series.DiD <- data.table(cohort.series.DiD, key = c("region0","yearquarter"))

cohort.series.DiD[ , year := floor(yearquarter/10) ]

# Melt for analysis # goes long so only columns are region, time temp yearquarter N variable value, second command sorts
cohort.series.DiD <- melt(cohort.series.DiD, id.vars = c("region0", "yearquarter","T","year","quarter","date_q"))
cohort.series.DiD <- data.table(cohort.series.DiD, key = c("region0","yearquarter","variable","year"))

# create dY just as the value
cohort.series.DiD[ , dY := value, by = c("region0","yearquarter","variable")]
# Drop 2005 cohort -- N/A for flows
cohort.series.DiD <- cohort.series.DiD[yearquarter > 20061 & region0!=9, ]
cohort.series.DiD <- cohort.series.DiD[yearquarter != 20164, ]



# Load the synthetic control data
cohort.series <- read.dta13(paste0(path.to.data,stub,".dta", collapse = ""))
cohort.series <- data.table(cohort.series, key = c("region0","yearquarter"))

# Melt for analysis
cohort.series <- melt(cohort.series, id.vars = c("region0","puma_id0","yearquarter","R"))
cohort.series <- data.table(cohort.series, key = c("region0","puma_id0","yearquarter","variable"))
cohort.series.N <- cohort.series[substr(variable,1,2)=="L4", list(variable,yearquarter,puma_id0,region0,value)]
cohort.series.N <- cohort.series.N[is.na(value)==FALSE, ]

# Wage cutoff
cohort.series[ , w := as.integer(gsub("[a-z\\_]+([0-9]+)","\\1",variable))]
cohort.series.N[ , w := as.integer(gsub("L4\\_([a-z\\_]+)([0-9]+)","\\2",variable))]
cohort.series.N[ , stub_N := (gsub("([a-z\\_]+)([0-9]+)","\\1",variable))]
setnames(cohort.series.N,"value","N")
cohort.series.N[ , variable := NULL]

# Add year variable
cohort.series[ , year := floor(yearquarter/10) ]

# Drop if variable is not one of the outcomes of interest! 
cohort.series <- cohort.series[is.element(variable,outcomes), ]
# Difference between current and baseline periods
cohort.series[ , dY := value]

cohort.series <- cohort.series[region0!=0 & region0!=9 & is.na(puma_id0)==F & puma_id0!=0, ]

# Region - PUMA ID
cohort.series[ , region_puma_id0 := region0 * 1e5 + puma_id0]	

# Drop 2005 & 20164 cohort -- N/A for flows
cohort.series <- cohort.series[yearquarter > 20061, ]
cohort.series <- cohort.series[yearquarter != 20164, ]

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
cohort.series[grepl("nworkers",variable)==TRUE, stub_N := "L4_bin_nmworkers"]
cohort.series[grepl("nworkers",variable)==FALSE, stub_N := "L4_bin_hours"]	
cohort.series <- merge(cohort.series, cohort.series.N, by = c("puma_id0","region0","yearquarter","w","stub_N"), all.x = TRUE, allow.cartesian = TRUE)

#######################################################
### To use contiguous PUMAs for SCM s.e.            ###
### List neighbors for each PUMA in WA              ###
### excl. King County outside of Seattle and SeaTac ###
#######################################################

# Load the file with all possible combinations of 
# 5 contiguous PUMAs in WA outside of King County 
AllCombinations.ContigPUMAs <- readRDS(file = paste0(path.to.data,"PUMA_unique_combinations.RDS"))

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

bootstrap.synth.exclKing.Weights <- data.frame(variable = character(),
                                               region_puma_id0 = integer(),
                                               region_puma_id0 = integer(),
                                               combinationNo = integer(),
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

  est.elast = 0
   df_w = NULL
  
  
  # Get the estimates
  results.synth.temp <- EstimateSynth(j,cohort.series.synth.exclKing[variable == j,],
                                      df_w,est.elast,"Synth_exclKing",PUMAs.Seattle, AllCombinations.ContigPUMAs, treated.periods,
                                      CIlevels = c(0.50,0.90,0.95))
  
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

results.coefs.all <- results.synth.exclKing	

Y0.Seattle <- cohort.series.DiD[region0 == 1 & year == 2014 & quarter == 2 , list(variable,dY)]
Y0.Seattle <- Y0.Seattle[grepl("d\\_",Y0.Seattle$variable)==FALSE, ]
Y0.Seattle$variable <- paste0("d_",Y0.Seattle$variable)
setnames(Y0.Seattle,"dY","Y0")

results.coefs.all <- merge(results.coefs.all, Y0.Seattle, by = c("variable"), all.x = TRUE, all.y = FALSE, allow.cartesian = TRUE)
setnames(results.coefs.all,"variable","variable_name")
	

#############################################################################
### Save all estimation results in one RDS file to use in future analysis ###
#############################################################################

saveRDS(results.coefs.all, file = paste0(path.to.save,stub.save,"_coef_CI_",mode.changes,".Rds"))
print(paste("File",paste0(path.to.save,stub.save,"_coef_CI_",mode.changes,".Rds"),"saved"))

write.csv(rbind(results.coefs.all), file = paste0(path.to.save,"jobs_bins_all.csv"))
              