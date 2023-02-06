# Stub to load the data
stub <- "aggregate_establishments_cumulative_all"

# Stub to save the results
stub.save <- "jobs_cumulative_2012"

mode.changes = "growth_rates"

############################
###                      ###
### Choose specification ###
###                      ###
############################

# Choose the number of factors to use in Interactive Fixed Effects estimation
# Note: cannot be larger than n_period - n_coefficients - 1 (i.e. 33)
N_factors = 24

#######################
###                 ###
### Define outcomes ###
###                 ###
#######################

# Wage bins -- # Jobs paying <$19
wge = 18 

# Stubs for outcomes -- variables 
var <- c("d_cum_hours_flow","d_cum_mean_wagerate")


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
	cohort.series <- cohort.series[yearquarter < 20144, ]
	
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
	cohort.series[grepl("nworkers",variable)==TRUE, stub_N := "L4_cum_nmworkers"]
	cohort.series[grepl("nworkers",variable)==FALSE, stub_N := "L4_cum_hours"]	
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
	
	# Set Treated periods
	treated.periods <- c("20123","20124", "20131" ,"20132","20133","20134", "20141","20142","20143")	

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
										 
	bootstrap.synth.exclKing.Weights <- data.frame(variable = character(),
													region_puma_id0 = integer(),
													region_puma_id0 = integer(),
													combinationNo = integer(),
													weights = double(),
													stringsAsFactors = FALSE)
	

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
	  results.synth.temp <- EstimateSynth(j,cohort.series.synth.exclKing[variable == j,],
	    										df_w,est.elast,"Synth_exclKing",PUMAs.Seattle, AllCombinations.ContigPUMAs, treated.periods)

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
		bootstrap.synth.exclKing.TE[[j_index]] <- results.synth.temp$bootstrapTE
		bootstrap.synth.exclKing.PUMAs[[j_index]] <- results.synth.temp$bootstrapPUMAs
		bootstrap.synth.exclKing.Resid[[j_index]] <- results.synth.temp$bootstrapResid
		bootstrap.synth.exclKing.TEcum[[j_index]] <- results.synth.temp$bootstrapTEcum
		bootstrap.synth.exclKing.Weights <- rbind(bootstrap.synth.exclKing.Weights, results.synth.temp$bootstrapWeights)

	}	
	
	########################################
	###                                  ###
	###        Analysis Part 3           ###
	### Interactive Fixed Effects        ###
	### Using non-Seattle PUMAS in WA    ###
	### as control regions               ###
	###                                  ###
	########################################
	
	###################################			  
	### PUMAS excluding King County ###
	###################################	
	
	cohort.series.bai.exclKing <- cohort.series[region0 != 2 & region0 != 3 & region0 != 9 , ]
	
	# Keep only relevant variables
	cohort.series.bai.exclKing <- cohort.series.bai.exclKing[ , 
	            c("variable","region0","region_puma_id0","yearquarter","R","dY","N"), with = FALSE]	
	
	# Treatment indicator
	
	  cohort.series.bai.exclKing[ , paste0("treated",1) := as.integer(region0 == 1 & yearquarter == 20123)]		
	  cohort.series.bai.exclKing[ , paste0("treated",2) := as.integer(region0 == 1 & yearquarter == 20124)]		
	  cohort.series.bai.exclKing[ , paste0("treated",3) := as.integer(region0 == 1 & yearquarter == 20131)]		
	  cohort.series.bai.exclKing[ , paste0("treated",4) := as.integer(region0 == 1 & yearquarter == 20132)]		
	  cohort.series.bai.exclKing[ , paste0("treated",5) := as.integer(region0 == 1 & yearquarter == 20133)]		
	  cohort.series.bai.exclKing[ , paste0("treated",6) := as.integer(region0 == 1 & yearquarter == 20134)]		
	  cohort.series.bai.exclKing[ , paste0("treated",7) := as.integer(region0 == 1 & yearquarter == 20141)]		
	  cohort.series.bai.exclKing[ , paste0("treated",8) := as.integer(region0 == 1 & yearquarter == 20142)]		
	  cohort.series.bai.exclKing[ , paste0("treated",9) := as.integer(region0 == 1 & yearquarter == 20143)]	
	
	# Initialize data.frames to save the results
	
	# Initialize results data.frames -- coefficients
	results.bai.exclKing <- data.frame(variable = character(),
									method = character(),
									T = integer(),
									dY = double(), 
									coef = double(), 
									coef.w = double(),	  
									stringsAsFactors = FALSE)

	# Initialize results data.frames -- variance covariance matrix 
	Vcov.bai.exclKing <- data.frame(variable = character(),
								method = character(),
								T = integer(),
								VcovE1 = double(), VcovE2 = double(), VcovE3 = double(), 
								VcovE4 = double(), VcovE5 = double(), VcovE6 = double(), 
								VcovE7 = double(), VcovE8 = double(), VcovE9 = double(),
								VcovW1 = double(), VcovW2 = double(), VcovW3 = double(),
								VcovW4 = double(), VcovW5 = double(), VcovW6 = double(),
								VcovW7 = double(), VcovW8 = double(), VcovW9 = double(),
								VcovEw1 = double(), VcovEw2 = double(), VcovEw3 = double(),
								VcovEw4 = double(), VcovEw5 = double(), VcovEw6 = double(),
								VcovEw7 = double(), VcovEw8 = double(), VcovEw9 = double(),
								stringsAsFactors = FALSE)
	
	weights.bai.exclKing <- data.frame(variable = character(), 										 
	                                   region_puma_id0 = integer(),
	                                   constant = double(),
	                                   regionFE = double(),	
	                                   loadings = double(),
	                                   factorID = integer(),
	                                   N_factors = integer(),
	                                   stringsAsFactors = FALSE)
	
	factors.bai.exclKing <- data.frame(variable = character(), 
	                                   yearquarter = integer(),
	                                   R = integer(),
	                                   constant = double(),
	                                   cohortFE = double(),
	                                   factorID = integer(),
	                                   N_factors = integer(),
	                                   factor = double(),
	                                   stringsAsFactors = FALSE)
	
	for (j in outcomes) {
	  for (K in 1:N_factors) {
	    
	    print("ESTIMATING INTERACTIVE FE MODEL TE USING PUMAS EXCLUDING KING COUNTY")
	    print(paste("Working on variable:",j,"with",K,"unobserved factors"))	
	    
	    # Note: j indexes variables
	    # K indexes number of factors used in Bai (2009) estimation procedure
	    
	    # Matrix of outcomes
	    # Matrix should be stacked regions x cohorts (i.e. all outcomes for ZCTA 1, all outcomes for ZCTA 2 etc.)
	    y <- cohort.series.bai.exclKing[variable == j, list(region_puma_id0,yearquarter,dY)]
	    X <- cohort.series.bai.exclKing[variable ==j, list(region_puma_id0,yearquarter,treated1,treated2,treated3,treated4,treated5,treated6,treated7,treated8,treated9)]		
	    	    
  	  # Employment variable? Then estimate wage effect as well
 	  if (i == "all") est.elast = max(grepl("hours",j),grepl("nworkers",j)) 
	  if (i != "all") est.elast = 0

  	  if (est.elast == 1) {
  	    j_w = paste0("d_cum_mean_wagerate",gsub("[a-z\\_]+([0-9]+)","\\1",j))
  	    yw <- cohort.series.bai.exclKing[variable == j_w, list(region_puma_id0,yearquarter,dY)]
  	    Xw <- cohort.series.bai.exclKing[variable == j_w, list(region_puma_id0,yearquarter,treated1,treated2,treated3,treated4,treated5,treated6,treated7,treated8,treated9)]		
  	  }
  	  if (est.elast == 0) {
  	  	yw = NULL
  	  	Xw = NULL
  	  }	    

	    # Get the estimates
	    results.bai.temp <- EstimateIFE(j,y,X,yw, Xw,est.elast,"Int_FE_exlKing")

	    # Save the estimates with other variables
		results.bai.exclKing <- rbind(results.bai.exclKing, results.bai.temp$coeff)	    
		Vcov.bai.exclKing <- rbind(Vcov.bai.exclKing, results.bai.temp$Vcov)	    
		weights.bai.exclKing <- rbind(weights.bai.exclKing, results.bai.temp$weights)  	  
		factors.bai.exclKing <- rbind(factors.bai.exclKing, results.bai.temp$factors)  	  	    
	  }
	}	
	

###############################
### Cumulative coefficients ###
### and elasticities        ###	
###############################	
	
  # Initialize
  results.synth.exclKing.full <- list()
  results.bai.exclKing.full <- list()	

	for (j in outcomes) {

		print(paste("Working on variable:",j))	
		
		# Is the outcome = employment (hours or jobs?)
		# If yes --> estimate employment elasticity as well
	    est.elast = max(grepl("hours",j),grepl("nworkers",j),grepl("payroll",j)) 
	    
	    # Get the estimates
		print("TRANSFORMING INTERACTIVE FIXED EFFECTS COEF")
		  for (K in 1:N_factors) {
	      results.bai.exclKing.full  <- c(results.bai.exclKing.full,
	    							list(CumulCoefs(results.bai.exclKing[results.bai.exclKing$variable == j & results.bai.exclKing$N_factors == K, ], 
	    							as.matrix(Vcov.bai.exclKing[Vcov.bai.exclKing$variable == j & results.bai.exclKing$N_factors == K, paste0("VcovE",1:9)]),
	    							as.matrix(Vcov.bai.exclKing[Vcov.bai.exclKing$variable == j & results.bai.exclKing$N_factors == K, paste0("VcovW",1:9)]),
	    							as.matrix(Vcov.bai.exclKing[Vcov.bai.exclKing$variable == j & results.bai.exclKing$N_factors == K, paste0("VcovEw",1:9)]), est.elast)))
		  }
	}
	
  results.synth.exclKing.full <- results.synth.exclKing
  results.bai.exclKing.full <- do.call("rbind",results.bai.exclKing.full)	
  
###################################################
### Choose optimal number of factors for Int FE ###
###################################################	

  results.bai.exclKing.full <- data.table(results.bai.exclKing.full, key = c("variable","N_factors","T"))	
	eigenv.bai <- unique(results.bai.exclKing.full[ , list(variable,N_factors,mineigenv,IC1,IC2)])
	setorder(eigenv.bai, variable, N_factors)
	eigenv.bai[ , ER := mineigenv/c(tail(mineigenv,-1),NA), by = "variable"]
	eigenv.bai[N_factors <= 30 , max_ER := max(ER, na.rm = TRUE), by = c("variable")]
	eigenv.bai[N_factors <= 30 , min_IC1 := min(IC1, na.rm = TRUE), by = c("variable")]
	eigenv.bai[N_factors <= 30 , min_IC2 := min(IC2, na.rm = TRUE), by = c("variable")]
	eigenv.bai[ , N_optimal_ER := as.integer(ER == max_ER)]
	eigenv.bai[ , N_optimal_IC1 := as.integer(IC1 == min_IC1)]
	eigenv.bai[ , N_optimal_IC2 := as.integer(IC2 == min_IC2)]
	
	results.bai.exclKing.full <- merge(results.bai.exclKing.full, eigenv.bai[, c("variable","N_factors","N_optimal_ER","N_optimal_IC1","N_optimal_IC2")], by = c("variable","N_factors"))	
	
################################################
### Save all estimation results in one table ###
################################################	

	vars.bai <- names(results.bai.exclKing.full)
	vars.synth <- names(results.synth.exclKing.full)

	all.vars <- union(vars.bai,vars.synth)

	vars.missing.synth <- setdiff(all.vars,vars.synth)
	vars.missing.synth <- setdiff(vars.missing.synth, c("N_optimal_ER","N_optimal_IC1","N_optimal_IC2"))
	vars.missing.bai <- setdiff(all.vars,vars.bai)

	for (v in vars.missing.synth) {
		results.synth.exclKing.full[["N_optimal_ER"]] = 1
	  results.synth.exclKing.full[["N_optimal_IC1"]] = 1
	  results.synth.exclKing.full[["N_optimal_IC2"]] = 1
	  
	  results.synth.exclKing.full[[v]] = NA
	}
	for (v in vars.missing.bai) {
		results.bai.exclKing.full[[v]] = NA
	}

	results.coefs.all <- do.call("rbind",list(results.synth.exclKing.full,results.bai.exclKing.full))
	setnames(results.coefs.all,"variable","variable_name")
	
	saveRDS(results.coefs.all, file = paste0(path.to.save,stub.save,"_coef_CI_",mode.changes,".Rds"))
	print(paste("File",paste0(path.to.save,stub.save,"_coef_CI_",mode.changes,".Rds"),"saved"))

	##########################
	### Tables with results ###
	##########################

	
	results.coefs.all<- readRDS(paste0(path.to.save,stub.save,"_coef_CI_",mode.changes,".Rds"))
	results.coefs.best <- results.coefs.all[results.coefs.all$N_optimal_IC2== 1, ]
	results.coefs.best<-data.table(results.coefs.best)
	results.coefs.best[is.element(method,"Synth_exclKing")==FALSE , tstatcum := coefcum / secum ]
	results.coefs.best[is.element(method,"Synth_exclKing")==FALSE , pval.cum := 2 * pmin(1-pnorm(tstatcum),pnorm(tstatcum))]
	
	results.coefs.best$sig[results.coefs.best$pval.cum > 0.1] <- ""
	results.coefs.best$sig[results.coefs.best$pval.cum <= 0.1] <- "*"
	results.coefs.best$sig[results.coefs.best$pval.cum <= 0.05] <- "**"
	results.coefs.best$sig[results.coefs.best$pval.cum <= 0.01] <- "***"
	
	results.coefs.best[ , coefcum_text := paste0(as.character(round(coefcum,3)),sig)]
	results.coefs.best[ , pvalcum_text := paste0("[",as.character(round(pval.cum,3)),"]")]	
	
	table.to.save <- results.coefs.best[grepl("18",variable_name) & is.element(method,c("Synth_exclKing","Int_FE_exlKing")), c("variable_name","method","T","coefcum_text","pvalcum_text")]
	table.to.save <- melt(table.to.save, id.vars = c("variable_name","method","T"))
	table.to.save <- dcast(table.to.save, T + variable ~ variable_name + method)  
	
	table.to.save.addl <- unique(results.coefs.best[grepl("18",variable_name) & is.element(method,c("Synth_exclKing","Int_FE_exlKing")), c("variable_name","method","R2","RMSE","Obs")])
	table.to.save.addl[ , R2 := as.character(round(R2,3))]
	table.to.save.addl[is.na(R2) , R2 := ""]	
	table.to.save.addl[ , RMSE := as.character(round(RMSE,3))]
	table.to.save.addl[is.na(RMSE) , RMSE := ""]
	table.to.save.addl[ , T := 10 ]
	table.to.save.addl <- melt(table.to.save.addl, id.vars = c("variable_name","method","T"))
	table.to.save.addl[variable=="R2" , T := 21 ]
	table.to.save.addl[variable=="RMSE"  , T := 22 ]
	table.to.save.addl[variable=="Obs"  , T := 23 ]
	table.to.save.addl <- dcast(table.to.save.addl, T + variable ~  variable_name + method)  
	
	# HW table.average <- table.to.save[which (table.to.save$variable== "coefcum_text") ,]
	# table.average <- c(as.numeric(table.average$d_cum_hours_flow18_Int_FE_exlKing), as.numeric(table.average$d_cum_hours_flow18_Synth_exclKing ), as.numeric(table.average$d_cum_mean_wagerate18_Int_FE_exlKing ),
	#                 as.numeric(table.average$d_cum_mean_wagerate18_Synth_exclKing))
	

	write.csv(rbind(as.matrix(table.to.save),as.matrix(table.to.save.addl)), file = paste0(path.to.save,"table_a6_",stub.save,"_sc_ife.csv"))
	

	