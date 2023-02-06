# variable_name to save the results
stub.save <- "jobs_cumulative_all"

mode.changes = "decomposition"

region.list <- c(0,1)
outcomes <- c("d_hours_all_0","d_hours_nonwage","d_hours_missing","d_hours_hires_seps","d_hours_seps","d_hours_contjobs","d_hours_up","d_hours_down")

#######################################################
### To use contiguous PUMAs for SCM s.e.            ###
### List neighbors for each PUMA in WA              ###
### excl. King County outside of Seattle and SeaTac ###
#######################################################

# Load the file with all possible combinations of 
# 5 contiguous PUMAs in WA outside of King County 
AllCombinations.ContigPUMAs <- readRDS(file = paste0(path.to.data,"PUMA_unique_combinations.RDS"))


#############################
###                       ###
### Load the data         ###
### for synthetic control ###
### and interactive FE    ### 
###                       ###
#############################

cohort.series <- read.dta13(paste0(path.to.data,"delta_hours_decomposed_jobs_below_19_v6.dta", collapse = ""))
cohort.series <- data.table(cohort.series, key = c("region","yearquarter"))
cohort.series[ , d_hours_nonwage := d_hours_hires_seps + d_hours_contjobs]

# Melt for analysis 
cohort.series <- melt(cohort.series, id.vars = c("region","puma_id","yearquarter","L4_hours_all"))
cohort.series <- data.table(cohort.series, key = c("region","puma_id","yearquarter","variable","L4_hours_all"))
setnames(cohort.series,"L4_hours_all","N")

# Add year variable
cohort.series[ , year := floor(yearquarter/10) ]

# Diference between current and baseline periods
cohort.series[ , dY := value]

cohort.series <- cohort.series[region!=0 & region!=9 & is.na(puma_id)==F & puma_id!=0, ]

# Region - PUMA ID
cohort.series[ , region_puma_id0 := region * 1e5 + puma_id]	

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
print(length(unique(cohort.series$region_puma_id0[is.element(cohort.series$region,c(1,4,5))])))

# Aggregate Seattle into one group
cohort.Seattle <- cohort.series[region == 1 , sum(dY*N)/sum(N), by = c("variable","yearquarter")]
setnames(cohort.Seattle,"V1","dY")
cohort.Seattle$region <- 1

#############################################
### Calculate predicted growth in Seattle ###
### using weights from Synthetic Control  ###
#############################################

## Load weights -- Seattle ###
weights.synth.exclKing <- readRDS(paste0(path.to.save,stub.save,"_synth_weights_exclKing_growth_rates.Rds"))
weights.synth.exclKing <- weights.synth.exclKing[weights.synth.exclKing$variable=="d_cum_hours_flow18", ]
weights.synth.exclKing$variable <- NULL 

# Calculate predicted growth
cohort.series.forsynth <- merge(cohort.series, weights.synth.exclKing, by = c("region_puma_id0"), allow.cartesian = TRUE)
cohort.series.forsynth <- cohort.series.forsynth[ , sum(dY*weights), by = c("variable","yearquarter")]
setnames(cohort.series.forsynth,c("V1"),c("dY"))
cohort.series.forsynth$region <- 0
cohort.series.forsynth$quarter <- cohort.series.forsynth$yearquarter - cohort.series.forsynth$year*10

cohort.series.forsynth <- rbind(cohort.series.forsynth,cohort.Seattle, fill = TRUE)

## Load weights -- placebo ###
bootstrapweights.synth.exclKing <- readRDS(paste0(path.to.save,stub.save,"_synth_bootstrapWeights_exclKing_growth_rates.Rds"))
bootstrapweights.synth.exclKing <- bootstrapweights.synth.exclKing[bootstrapweights.synth.exclKing$variable=="d_cum_hours_flow18", ]
bootstrapweights.synth.exclKing$variable <- NULL 

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
	cohort.series.synth.exclKing <- cohort.series[region != 2 & region != 3 & region!=9 ,
	                                 c("variable","region","region_puma_id0","yearquarter","dY", "N"), with = F]
	setnames(cohort.series.synth.exclKing,"region","region0")								 
	
	# Initialize results data.frames -- coefficients
	results.synth.exclKing <- data.frame(variable = character(),
									method = character(),
									T = integer(),
									dY = double(), 
									coef = double(), 
									coef.w = double(),	  
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
	  results.synth.temp <- EstimateSynth(j,cohort.series.synth.exclKing[variable == j,],
	    										df_w,est.elast,"Synth_exclKing",PUMAs.Seattle, AllCombinations.ContigPUMAs, treated.periods,
												CIlevels = c(0.50,0.90,0.95),
												weights.pre.Seattle = weights.synth.exclKing, weights.pre.placebo = bootstrapweights.synth.exclKing)
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
		bootstrap.synth.exclKing.TE[[j_index]] <- results.synth.temp$bootstrapTE
		bootstrap.synth.exclKing.PUMAs[[j_index]] <- results.synth.temp$bootstrapPUMAs
		bootstrap.synth.exclKing.Resid[[j_index]] <- results.synth.temp$bootstrapResid
		bootstrap.synth.exclKing.TEcum[[j_index]] <- results.synth.temp$bootstrapTEcum
	}	
	

################################
### Print table with results ###
################################

	results.coefs.best <- results.synth.exclKing
	setnames(results.coefs.best,"variable","variable_name")
	results.coefs.best <- data.table(results.coefs.best, key = c("variable_name","method","T"))
	
	results.coefs.best$sig[results.coefs.best$pval > 0.1] <- ""
	results.coefs.best$sig[results.coefs.best$pval <= 0.1] <- "*"
	results.coefs.best$sig[results.coefs.best$pval <= 0.05] <- "**"
	results.coefs.best$sig[results.coefs.best$pval <= 0.01] <- "***"
	
	results.coefs.best[ , coefcum_text := paste0(as.character(round(coef,3)),sig)]
	results.coefs.best[ , pvalcum_text := paste0("[",as.character(round(pval,3)),"]")]	

	table.to.save <- results.coefs.best[, c("variable_name","method","T","coefcum_text","pvalcum_text")]
	table.to.save <- melt(table.to.save, id.vars = c("variable_name","method","T"))
	table.to.save <- dcast(table.to.save, T + variable ~ variable_name + method)  
	
	table.to.save.addl <- unique(results.coefs.best[, c("variable_name","method","RMSE","Obs")])
	table.to.save.addl[ , RMSE := as.character(round(RMSE,3))]
	table.to.save.addl[is.na(RMSE) , RMSE := ""]
	table.to.save.addl[ , T := 10 ]
	table.to.save.addl <- melt(table.to.save.addl, id.vars = c("variable_name","method","T"))
	table.to.save.addl[variable=="RMSE"  , T := 22 ]
	table.to.save.addl[variable=="Obs"  , T := 23 ]
	table.to.save.addl <- dcast(table.to.save.addl, T + variable ~  variable_name + method)  
	
	write.csv(rbind(as.matrix(table.to.save),as.matrix(table.to.save.addl)), file = paste0(path.to.save,"table_c1_",mode.changes,".csv"))
	

############################################
### Additional variables for plotting    ###
############################################

if (max(grepl("variable\\_name",names(cohort.series.forsynth)))==0) setnames(cohort.series.forsynth,"variable","variable_name")
if (max(grepl("variable\\_name",names(cohort.series)))==0) setnames(cohort.series,"variable","variable_name")

cohort.series.forsynth[is.element(variable_name,outcomes), y.min:= min(dY, na.rm = TRUE), by = c("variable_name")]
cohort.series.forsynth[is.element(variable_name,outcomes), y.max := max(dY, na.rm = TRUE), by = c("variable_name")]

cohort.series.forsynth[ , year := floor(yearquarter/10)]
cohort.series.forsynth[ , quarter := yearquarter - year*10]
cohort.series.forsynth[ , yq := as.Date(as.yearqtr(year + 0.25*(quarter-1)))]
cohort.series.forsynth[yq >= as.Date(as.yearqtr(2007.75)) & yq <= as.Date(as.yearqtr(2009.50)) & is.element(variable_name,outcomes), recession_max := 100*y.max ]
cohort.series.forsynth[yq >= as.Date(as.yearqtr(2007.75)) & yq <= as.Date(as.yearqtr(2009.50)) & is.element(variable_name,outcomes), recession_min := 100*y.min ]
cohort.series.forsynth[ , yq := yq + 365/(4*2)]

cohort.series[is.element(variable_name,outcomes), y.min:= min(dY, na.rm = TRUE), by = c("variable_name")]
cohort.series[is.element(variable_name,outcomes), y.max := max(dY, na.rm = TRUE), by = c("variable_name")]

cohort.series[ , year := floor(yearquarter/10)]
cohort.series[ , quarter := yearquarter - year*10]
cohort.series[ , yq := as.Date(as.yearqtr(year + 0.25*(quarter-1)))]
cohort.series[yq >= as.Date(as.yearqtr(2007.75)) & yq <= as.Date(as.yearqtr(2009.50)) & is.element(variable_name,outcomes), recession_max := 100*y.max ]
cohort.series[yq >= as.Date(as.yearqtr(2007.75)) & yq <= as.Date(as.yearqtr(2009.50)) & is.element(variable_name,outcomes), recession_min := 100*y.min ]
cohort.series[ , yq := yq + 365/(4*2)]

#####################################
### Plot 1a:                      ###
### Fit of synthetic Seattle      ###
### in growth rates               ###
#####################################

outcomes <- c("d_hours_up")

for (i in 1:length(outcomes)) {

	y.max <- ceiling(100*max(cohort.series.forsynth[is.element(variable_name,outcomes[[i]]), y.max], na.rm = TRUE))
	y.min <- floor(100*min(cohort.series.forsynth[is.element(variable_name,outcomes[[i]]), y.min], na.rm = TRUE))  	  
	  
	plot.matrix <- ggplot() + 
				# Great Recession (shaded)
				geom_ribbon(data = cohort.series.forsynth[is.element(variable_name,outcomes[[i]]), ],
					aes(x = yq, ymin = recession_min, ymax = recession_max, fill = "The Great Recession\n(nationwide)"), alpha = 0.5) +
	          	# MW passage (vertical line)
	          	geom_vline(data = cohort.series.forsynth[is.element(variable_name,outcomes[[i]]), ],
	             	aes(xintercept = as.numeric(as.Date("2014-06-10"))), color = "red", linetype = "solid") +			 
	          	# MW step-up 1 (vertical line)
	          	geom_vline(data = cohort.series.forsynth[is.element(variable_name,outcomes[[i]]), ],
					aes(xintercept = as.numeric(as.Date("2015-04-01"))), color = "red", linetype = "dashed") +			 
	          	# MW step-up 2 (vertical line)
				geom_vline(data = cohort.series.forsynth[is.element(variable_name,outcomes[[i]]), ],
					aes(xintercept = as.numeric(as.Date("2016-01-01"))), color = "red", linetype = "dashed") +			 
	          	# Zero level (horizontal line)
				geom_hline(data = cohort.series.forsynth[is.element(variable_name,outcomes[[i]]), ],
					aes(yintercept = 0), color = "#444444") +	
				# Predicted Seattle									
				geom_line(data = cohort.series.forsynth[is.element(variable_name,outcomes[[i]]) & region == 0, ],
					 		   aes(x = yq, y = 100*(dY), color = factor(2)), size = 1) +
				# Observed Seattle									
				geom_line(data = cohort.series.forsynth[is.element(variable_name,outcomes[[i]]) & region == 1, ],
					 		   aes(x = yq, y = 100*(dY), color = factor(1)), size = 0.75) +
				# Label -- passage
	         	geom_text(data = cohort.series.forsynth[is.element(variable_name,outcomes[[i]]) & yearquarter==20142, ],
	            	aes(x = as.Date("2014-06-10"), y = 100*y.min, label = "Min \n wage \npassed "), size = 2.5, vjust = 0, hjust = 1) +	
	            # Label -- step-up 1		 
	         	geom_text(data = cohort.series.forsynth[is.element(variable_name,outcomes[[i]]) & yearquarter==20152, ],
					aes(x = as.Date("2015-04-01"), y = 100*y.min, label = "$11 \nmin \nwage "), size = 2.5, vjust = 0, hjust = 1) +
				# Label -- step-up 2				 
				geom_text(data = cohort.series.forsynth[is.element(variable_name,outcomes[[i]]) & yearquarter==20162, ],
								 aes(x = as.Date("2016-01-01"), y = 100*y.min, label = "$13 \nmin \nwage "), size = 2.5, vjust = 0, hjust = 1) +			 
	         	theme_bw(base_size = 10) + scale_y_continuous(breaks = pretty_breaks(n = 5), limits = c(y.min,y.max))

	plot.matrix <- plot.matrix + 
					 xlab("") + ylab("Change due to this component, %") +
					 scale_color_manual(values = c("black","dodgerblue"), labels = c("Seattle","Synthetic Seattle")) +
					 scale_fill_manual(values = c("grey")) +
					 guides(color = guide_legend(""), fill = guide_legend("")) +
					 theme(legend.position = "bottom", legend.direction = "horizontal",
					 	   legend.margin = margin(t = -3, unit='mm'),
					 	   legend.key = element_blank(), legend.key.size = unit(2, 'lines'),
					 	   plot.margin = unit(x = c(5,5,5,5), units = "mm"))
						   
	ggsave(plot.matrix, 
	       file = paste0(path.to.save,"\\figure_c1_decomposition_",outcomes[[i]],".png"), 
	       units = "in", width = 6.5, height = 4)		
}
 





