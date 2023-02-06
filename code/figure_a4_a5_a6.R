# Data stubs 
stub <- "aggregate_establishments_cumulative_all"
stub.DiD <- "region_level_cumulative"

# Stub to save the results
stub.save <- "jobs_cumulative_all"
mode.changes = "growth_rates"

w.list <- c(19)
region.list <- c(0,1)
outcomes.list <- c("dcumhoursflow","dcumnworkersbeg","dcummeanwagerate","dcumpayrollflow")
outcomes.list.abs <- c("cumhoursflow","cumnworkersbeg","cummeanwagerate","cumpayrollflow")

##############################
### Load the data          ###
### for Synthetic Controls ###
##############################

cohort.series <- read.dta13(paste0(path.to.data,stub,".dta", collapse = ""))
cohort.series <- data.table(cohort.series, key = c("region0","yearquarter"))
cohort.series <- melt(cohort.series, id.vars = c("region0","puma_id0","yearquarter","R"))
cohort.series <- data.table(cohort.series, key = c("region0","puma_id0","yearquarter","variable"))
cohort.series[ , year := floor(yearquarter/10) ]

# Diference between current and baseline periods
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

# Standardized PUMAs -- i.e. - mean / sd over time 
cohort.series[yearquarter < 20142 , preMW_mean_tmp := mean(dY), by = c("region_puma_id0","variable")]
cohort.series[ , max_preMW_mean := max(preMW_mean_tmp, na.rm = TRUE), by = c("region_puma_id0","variable")]
cohort.series[ , preMW_mean := max_preMW_mean]
cohort.series[ , preMW_mean_tmp := NULL]
cohort.series[ , max_preMW_mean := NULL]
cohort.series[yearquarter < 20142 , preMW_sd_tmp := sd(dY), by = c("region_puma_id0","variable")]
cohort.series[ , max_preMW_sd := max(preMW_sd_tmp, na.rm = TRUE), by = c("region_puma_id0","variable")]
cohort.series[ , preMW_sd := max_preMW_sd]
cohort.series[ , preMW_sd_tmp := NULL]
cohort.series[ , max_preMW_sd := NULL]

cohort.series[ , Z := (dY - preMW_mean) / preMW_sd]

#####################
### Load the data ###
### for DiD       ### 
#####################

# read file, change column name of region, sort by region,time_temp, yearquarter 
cohort.series.DiD <- read.dta13(paste0(path.to.data,stub.DiD,".dta", collapse = ""))
cohort.series.DiD <- data.table(cohort.series.DiD, key = c("region0","yearquarter"))

cohort.series.DiD[ , year := floor(yearquarter/10) ]

# Melt for analysis
cohort.series.DiD <- melt(cohort.series.DiD, id.vars = c("region0", "yearquarter","T","region_time","year","quarter","date_q"))
cohort.series.DiD <- data.table(cohort.series.DiD, key = c("region0","yearquarter","variable","year"))

# create dY just as the value
cohort.series.DiD[ , dY := value, by = c("region0","yearquarter","variable")]
# Drop 2005 cohort -- N/A for flows
cohort.series.DiD <- cohort.series.DiD[yearquarter > 20061 & region0!=9, ]
cohort.series.DiD <- cohort.series.DiD[yearquarter != 20164, ]

# Wage cutoff
cohort.series.DiD[ , w := as.integer(gsub("[a-z\\_]+([0-9]+)","\\1",variable))]

# Merge wage outcomes
cohort.series.DiD.w <- cohort.series.DiD[grepl("d\\_cum\\_mean\\_wagerate",variable), list(yearquarter,region0,T,dY,w)]
cohort.series.DiD.w <- cohort.series.DiD.w[is.na(w) == FALSE, ]
setnames(cohort.series.DiD.w,"dY","dY_wagerate")

cohort.series.DiD <- merge(cohort.series.DiD, cohort.series.DiD.w, by = c("yearquarter","region0","T","w"), all.x = TRUE, all.y = FALSE)

# Standardized Seattle -- i.e. - mean / sd over time 
cohort.series.DiD[yearquarter < 20142 , preMW_mean_tmp := mean(dY), by = c("region0","variable")]
cohort.series.DiD[ , max_preMW_mean := max(preMW_mean_tmp, na.rm = TRUE), by = c("region0","variable")]
cohort.series.DiD[ , preMW_mean := max_preMW_mean]
cohort.series.DiD[ , preMW_mean_tmp := NULL]
cohort.series.DiD[ , max_preMW_mean := NULL]
cohort.series.DiD[yearquarter < 20142 , preMW_sd_tmp := sd(dY), by = c("region0","variable")]
cohort.series.DiD[ , max_preMW_sd := max(preMW_sd_tmp, na.rm = TRUE), by = c("region0","variable")]
cohort.series.DiD[ , preMW_sd := max_preMW_sd]
cohort.series.DiD[ , preMW_sd_tmp := NULL]
cohort.series.DiD[ , max_preMW_sd := NULL]

cohort.series.DiD[ , Z := (dY - preMW_mean) / preMW_sd]

#############################################
### Calculate predicted growth in Seattle ###
### using weights from Synthetic Control  ###
#############################################

## Load weights 
weights.synth.exclKing <- readRDS(paste0(path.to.save,stub.save,"_synth_weights_exclKing_growth_rates.Rds"))
weights.synth.exclKing$variable.aug <- weights.synth.exclKing$variable
weights.synth.exclKing$variable <- NULL

# Calculate predicted growth
cohort.series[grepl("d\\_",variable)==FALSE, variable.aug := paste0("d_",variable) ]
cohort.series[grepl("d\\_",variable)==TRUE, variable.aug := variable ]
cohort.series.forsynth <- merge(cohort.series, weights.synth.exclKing, by = c("variable.aug","region_puma_id0"), allow.cartesian = TRUE)
cohort.series.forsynth <- cohort.series.forsynth[ , list(sum(dY * weights),sum(Z * weights)), by = c("variable","yearquarter","year")]
setnames(cohort.series.forsynth,c("V1","V2"),c("dY","Z"))
cohort.series.forsynth$region0 <- 0
cohort.series.forsynth$quarter <- cohort.series.forsynth$yearquarter - cohort.series.forsynth$year*10

# Combine data for synthetic control and for DiD
cohort.series.forsynth <- rbind(cohort.series.forsynth,cohort.series.DiD, fill = TRUE)

############################################
### Additional variables for plotting    ###
############################################

setnames(cohort.series.forsynth,"variable","variable_name")

cohort.series.forsynth[ , variable_name := gsub("\\_","",as.character(variable_name))]
cohort.series.forsynth[ , w := as.integer(str_sub(variable_name,-2,-1)) + 1]
cohort.series.forsynth[ , stub := str_sub(variable_name,1,-3)]
cohort.series.forsynth[is.na(w) , stub := str_sub(variable_name,1,-2)]
cohort.series.forsynth[is.na(w) , w := as.integer(str_sub(variable_name,-1,-1)) + 1]

cohort.series.forsynth[is.element(region0,region.list) & is.element(stub,outcomes.list), y.min:= min(dY - 1, na.rm = TRUE), by = c("stub","w")]
cohort.series.forsynth[is.element(region0,region.list) & is.element(stub,outcomes.list), y.max := max(dY - 1, na.rm = TRUE), by = c("stub","w")]
cohort.series.forsynth[is.element(region0,c(4,5)) & is.element(stub,outcomes.list.abs), y.max := max(dY, na.rm = TRUE), by = c("stub","w")]
cohort.series.forsynth[is.element(region0,c(4,5)) & is.element(stub,outcomes.list.abs), y.min:= 0.01*y.max]

cohort.series.forsynth[ , quarter := yearquarter - year*10]
cohort.series.forsynth[ , yq := as.Date(as.yearqtr(year + 0.25*(quarter-1)))]
cohort.series.forsynth[yq >= as.Date(as.yearqtr(2007.75)) & yq <= as.Date(as.yearqtr(2009.50)) & is.element(stub,outcomes.list), recession_max := 100*y.max ]
cohort.series.forsynth[yq >= as.Date(as.yearqtr(2007.75)) & yq <= as.Date(as.yearqtr(2009.50)) & is.element(stub,outcomes.list), recession_min := 100*y.min ]
cohort.series.forsynth[yq >= as.Date(as.yearqtr(2007.75)) & yq <= as.Date(as.yearqtr(2009.50)) & is.element(stub,outcomes.list.abs), recession_max := y.max ]
cohort.series.forsynth[yq >= as.Date(as.yearqtr(2007.75)) & yq <= as.Date(as.yearqtr(2009.50)) & is.element(stub,outcomes.list.abs), recession_min := 0 ]
cohort.series.forsynth[ , yq := yq + 365/(4*2)]

cohort.series.forsynth[ , region.labeled := ordered(100-region0, labels = c("Washington State excl.\nKing, Snohomish, Pierce,\nand Kitsap Counties",
                                                                            "Snohomish, Pierce,\nand Kitsap Counties","King County excl.\nSeattle and SeaTac",
                                                                            "SeaTac","Seattle","Synthetic Seattle"))]
cohort.series.forsynth[ , w.labeled := ordered(w, labels = paste0("Jobs paying <$",sort(unique(cohort.series.forsynth$w))))]


cohort.series[ , variable_name := gsub("\\_","",as.character(variable))]
cohort.series[ , w := as.integer(str_sub(variable_name,-2,-1)) + 1]
cohort.series[ , stub := str_sub(variable_name,1,-3)]
cohort.series[is.na(w) , stub := str_sub(variable_name,1,-2)]
cohort.series[is.na(w) , w := as.integer(str_sub(variable_name,-1,-1)) + 1]

cohort.series[is.element(region0,c(4,5)) & is.element(stub,outcomes.list), y.min:= min(dY - 1, na.rm = TRUE), by = c("stub","w")]
cohort.series[is.element(region0,c(4,5)) & is.element(stub,outcomes.list), y.max := max(dY - 1, na.rm = TRUE), by = c("stub","w")]
cohort.series[is.element(region0,c(4,5)) & is.element(stub,outcomes.list.abs), y.max := max(dY, na.rm = TRUE), by = c("stub","w")]
cohort.series[is.element(region0,c(4,5)) & is.element(stub,outcomes.list.abs), y.min:= 0.01*y.max, by = c("stub","w")]
cohort.series[is.element(region0,c(4,5)) & is.element(stub,outcomes.list.abs), Z.min := min(Z, na.rm = TRUE), by = c("stub","w")]
cohort.series[is.element(region0,c(4,5)) & is.element(stub,outcomes.list.abs), Z.max := max(Z, na.rm = TRUE), by = c("stub","w")]

cohort.series[ , quarter := yearquarter - year*10]
cohort.series[ , yq := as.Date(as.yearqtr(year + 0.25*(quarter-1)))]
cohort.series[yq >= as.Date(as.yearqtr(2007.75)) & yq <= as.Date(as.yearqtr(2009.50)) & is.element(stub,outcomes.list), recession_max := 100*y.max ]
cohort.series[yq >= as.Date(as.yearqtr(2007.75)) & yq <= as.Date(as.yearqtr(2009.50)) & is.element(stub,outcomes.list), recession_min := 100*y.min ]
cohort.series[yq >= as.Date(as.yearqtr(2007.75)) & yq <= as.Date(as.yearqtr(2009.50)) & is.element(stub,outcomes.list.abs), recession_max := y.max ]
cohort.series[yq >= as.Date(as.yearqtr(2007.75)) & yq <= as.Date(as.yearqtr(2009.50)) & is.element(stub,outcomes.list.abs), recession_min := 0 ]
cohort.series[yq >= as.Date(as.yearqtr(2007.75)) & yq <= as.Date(as.yearqtr(2009.50)) & is.element(stub,outcomes.list.abs), recession_Z_max := Z.max ]
cohort.series[yq >= as.Date(as.yearqtr(2007.75)) & yq <= as.Date(as.yearqtr(2009.50)) & is.element(stub,outcomes.list.abs) & stub=="cummeanwagerate", recession_Z_max := 1.7*Z.max ]
cohort.series[yq >= as.Date(as.yearqtr(2007.75)) & yq <= as.Date(as.yearqtr(2009.50)) & is.element(stub,outcomes.list.abs), recession_Z_min := Z.min ]
cohort.series[ , yq := yq + 365/(4*2)]

cohort.series[ , region.labeled := ordered(100-region0, labels = c("Washington State excl.\nKing, Snohomish, Pierce,\nand Kitsap Counties",
                                                                   "Snohomish, Pierce,\nand Kitsap Counties","King County excl.\nSeattle and SeaTac",
                                                                   "SeaTac","Seattle"))]
cohort.series[ , w.labeled := ordered(w, labels = paste0("Jobs paying <$",sort(unique(cohort.series$w))))]

#######################################
### Figure A4:  Levels              ###
#######################################

for (i in 1:length(outcomes.list.abs)) {
  if (grepl("wage",outcomes.list.abs[[i]])) {
    for (j in w.list) {
      
      plot.matrix <- ggplot() + 
        #				# Great Recession (shaded)
        geom_ribbon(data = cohort.series[is.element(w,j) & is.element(stub,outcomes.list.abs[[i]]) & is.element(region0,4), ],
                    aes(x = yq, ymin = recession_min, ymax = recession_max, fill = factor(3)), alpha = 0.5) +
        # MW passage (vertical line)
        geom_vline(data = cohort.series[is.element(w,j) & is.element(stub,outcomes.list.abs[[i]]), ],
                   aes(xintercept = as.numeric(as.Date("2014-06-10"))), color = "red", linetype = "solid") +			 
        # MW step-up 1 (vertical line)
        geom_vline(data = cohort.series[is.element(w,j) & is.element(stub,outcomes.list.abs[[i]]) & is.element(region0,region.list), ],
                   aes(xintercept = as.numeric(as.Date("2015-04-01"))), color = "red", linetype = "dashed") +			 
        # MW step-up 2 (vertical line)
        geom_vline(data = cohort.series[is.element(w,j) & is.element(stub,outcomes.list.abs[[i]]) & is.element(region0,region.list), ],
                   aes(xintercept = as.numeric(as.Date("2016-01-01"))), color = "red", linetype = "dashed") +			 
        # Zero level (horizontal line)
        geom_hline(data = cohort.series[is.element(w,j) & is.element(stub,outcomes.list.abs[[i]]) & is.element(region0,region.list), ],
                   aes(yintercept = 0), color = "#444444") +	
        # Observed PUMAs outside of Seattle									
        geom_line(data = cohort.series[is.element(w,j) & is.element(stub,outcomes.list.abs[[i]]) & is.element(region0,c(4,5)), ],
                  aes(x = yq, y = (dY), color = factor(2), group = region_puma_id0), size = 0.5) +
        # Observed Seattle									
        geom_line(data = cohort.series.forsynth[is.element(w,j) & is.element(stub,outcomes.list.abs[[i]]) & is.element(region0,1), ],
                  aes(x = yq, y = (dY), color = factor(1)), size = 1) +
        # Label -- passage
        geom_text(data = cohort.series[is.element(w,j) & is.element(stub,outcomes.list.abs[[i]]) & yearquarter==20142 & is.element(region0,4), ],
                  aes(x = as.Date("2014-06-10"), y = y.min, label = "Min \n wage \npassed "), size = 2.5, vjust = 0, hjust = 1) +	
        # Label -- step-up 1		 
        geom_text(data = cohort.series[is.element(w,j) & is.element(stub,outcomes.list.abs[[i]]) & yearquarter==20152 & is.element(region0,4), ],
                  aes(x = as.Date("2015-04-01"), y = y.min, label = "$11 \nmin \nwage "), size = 2.5, vjust = 0, hjust = 1) +
        # Label -- step-up 2				 
        geom_text(data = cohort.series[is.element(w,j) & is.element(stub,outcomes.list.abs[[i]]) & yearquarter==20162 & is.element(region0,4), ],
                  aes(x = as.Date("2016-01-01"), y = y.min, label = "$13 \nmin \nwage "), size = 2.5, vjust = 0, hjust = 1) +			 
        theme_bw(base_size = 10)
      
      plot.matrix <- plot.matrix + 
        xlab("") + ylab("$") +
        scale_color_manual(values = c("black","dark grey"), labels = c("Seattle","PUMAs in WA\noutside of King County")) +
        scale_fill_manual(values = c("grey"), labels = "The Great Recession\n(nationwide)") +
        guides(color = guide_legend(""), fill = guide_legend("")) +
        theme(legend.position = "bottom", legend.direction = "horizontal",
              legend.margin = margin(t = -3, unit='mm'),
              legend.key = element_blank(), legend.key.size = unit(2, 'lines'),
              plot.margin = unit(x = c(5,5,5,5), units = "mm"))
      
      ggsave(plot.matrix, 
             file = paste0(path.to.save,"\\figure_a4_levels_",outcomes.list.abs[[i]],"_w",j,".png"), 
             units = "in", width = 6.5, height = 4)		
    }
  }
  if (grepl("wage",outcomes.list.abs[[i]])==FALSE) {
    for (j in w.list) {
      
      plot.matrix <- ggplot() + 
        #				# Great Recession (shaded)
        geom_ribbon(data = cohort.series[is.element(w,j) & is.element(stub,outcomes.list.abs[[i]]) & is.element(region0,4), ],
                    aes(x = yq, ymin = recession_min/1e3, ymax = recession_max/1e3, fill = factor(3)), alpha = 0.5) +
        # MW passage (vertical line)
        geom_vline(data = cohort.series[is.element(w,j) & is.element(stub,outcomes.list.abs[[i]]), ],
                   aes(xintercept = as.numeric(as.Date("2014-06-10"))), color = "red", linetype = "solid") +			 
        # MW step-up 1 (vertical line)
        geom_vline(data = cohort.series[is.element(w,j) & is.element(stub,outcomes.list.abs[[i]]) & is.element(region0,region.list), ],
                   aes(xintercept = as.numeric(as.Date("2015-04-01"))), color = "red", linetype = "dashed") +			 
        # MW step-up 2 (vertical line)
        geom_vline(data = cohort.series[is.element(w,j) & is.element(stub,outcomes.list.abs[[i]]) & is.element(region0,region.list), ],
                   aes(xintercept = as.numeric(as.Date("2016-01-01"))), color = "red", linetype = "dashed") +			 
        # Zero level (horizontal line)
        geom_hline(data = cohort.series[is.element(w,j) & is.element(stub,outcomes.list.abs[[i]]) & is.element(region0,region.list), ],
                   aes(yintercept = 0), color = "#444444") +	
        # Observed PUMAs outside of Seattle									
        geom_line(data = cohort.series[is.element(w,j) & is.element(stub,outcomes.list.abs[[i]]) & is.element(region0,c(4,5)), ],
                  aes(x = yq, y = (dY)/1e3, color = factor(2), group = region_puma_id0), size = 0.5) +
        # Observed Seattle									
        geom_line(data = cohort.series.forsynth[is.element(w,j) & is.element(stub,outcomes.list.abs[[i]]) & is.element(region0,1), ],
                  aes(x = yq, y = (dY)/5e3, color = factor(1)), size = 1) +
        # Label -- passage
        geom_text(data = cohort.series[is.element(w,j) & is.element(stub,outcomes.list.abs[[i]]) & yearquarter==20142 & is.element(region0,4), ],
                  aes(x = as.Date("2014-06-10"), y = y.min/1e3, label = "Min \n wage \npassed "), size = 2.5, vjust = 0, hjust = 1) +	
        # Label -- step-up 1		 
        geom_text(data = cohort.series[is.element(w,j) & is.element(stub,outcomes.list.abs[[i]]) & yearquarter==20152 & is.element(region0,4), ],
                  aes(x = as.Date("2015-04-01"), y = y.min/1e3, label = "$11 \nmin \nwage "), size = 2.5, vjust = 0, hjust = 1) +
        # Label -- step-up 2				 
        geom_text(data = cohort.series[is.element(w,j) & is.element(stub,outcomes.list.abs[[i]]) & yearquarter==20162 & is.element(region0,4), ],
                  aes(x = as.Date("2016-01-01"), y = y.min/1e3, label = "$13 \nmin \nwage "), size = 2.5, vjust = 0, hjust = 1) +			 
        theme_bw(base_size = 10) 
      
      plot.matrix <- plot.matrix + 
        xlab("") + ylab("000s") +
        scale_color_manual(values = c("black","dark grey"), labels = c("Seattle divided by 5","PUMAs in WA\noutside of King County")) +
        scale_fill_manual(values = c("grey"), labels = "The Great Recession\n(nationwide)") +
        guides(color = guide_legend(""), fill = guide_legend("")) +
        theme(legend.position = "bottom", legend.direction = "horizontal",
              legend.margin = margin(t = -3, unit='mm'),
              legend.key = element_blank(), legend.key.size = unit(2, 'lines'),
              plot.margin = unit(x = c(5,5,5,5), units = "mm"))
      
      ggsave(plot.matrix, 
             file = paste0(path.to.save,"\\figure_a4_levels_",outcomes.list.abs[[i]],"_w",j,".png"), 
             units = "in", width = 6.5, height = 4)		
    }
  }	
}


#####################################
### Figure A5: Fit of SC          ###
#####################################

for (i in 1:length(outcomes.list)) {
  for (j in w.list) {
    
    y.max <- ceiling(100*max(cohort.series.forsynth[is.element(w,j) & is.element(stub,outcomes.list[[i]]) & is.element(region0,region.list), y.max], na.rm = TRUE))
    y.min <- floor(100*min(cohort.series.forsynth[is.element(w,j) & is.element(stub,outcomes.list[[i]]) & is.element(region0,region.list), y.min], na.rm = TRUE))  	  
    
    plot.matrix <- ggplot() + 
      # Great Recession (shaded)
      geom_ribbon(data = cohort.series.forsynth[is.element(w,j) & is.element(stub,outcomes.list[[i]]) & is.element(region0,region.list), ],
                  aes(x = yq, ymin = recession_min, ymax = recession_max, fill = "The Great Recession\n(nationwide)"), alpha = 0.5) +
      # MW passage (vertical line)
      geom_vline(data = cohort.series.forsynth[is.element(w,j) & is.element(stub,outcomes.list[[i]]), ],
                 aes(xintercept = as.numeric(as.Date("2014-06-10"))), color = "red", linetype = "solid") +			 
      # MW step-up 1 (vertical line)
      geom_vline(data = cohort.series.forsynth[is.element(w,j) & is.element(stub,outcomes.list[[i]]) & is.element(region0,region.list), ],
                 aes(xintercept = as.numeric(as.Date("2015-04-01"))), color = "red", linetype = "dashed") +			 
      # MW step-up 2 (vertical line)
      geom_vline(data = cohort.series.forsynth[is.element(w,j) & is.element(stub,outcomes.list[[i]]) & is.element(region0,region.list), ],
                 aes(xintercept = as.numeric(as.Date("2016-01-01"))), color = "red", linetype = "dashed") +			 
      # Zero level (horizontal line)
      geom_hline(data = cohort.series.forsynth[is.element(w,j) & is.element(stub,outcomes.list[[i]]) & is.element(region0,region.list), ],
                 aes(yintercept = 0), color = "#444444") +	
      # Observed and predicted Seattle									
      geom_line(data = cohort.series.forsynth[is.element(w,j) & is.element(stub,outcomes.list[[i]]) & is.element(region0,region.list), ],
                aes(x = yq, y = 100*(dY-1), color = region.labeled), size = 1) +
      # Label -- passage
      geom_text(data = cohort.series.forsynth[is.element(w,j) & is.element(stub,outcomes.list[[i]]) & yearquarter==20142 & is.element(region0,region.list), ],
                aes(x = as.Date("2014-06-10"), y = 100*y.min, label = "Min \n wage \npassed "), size = 2.5, vjust = 0, hjust = 1) +	
      # Label -- step-up 1		 
      geom_text(data = cohort.series.forsynth[is.element(w,j) & is.element(stub,outcomes.list[[i]]) & yearquarter==20152 & is.element(region0,region.list), ],
                aes(x = as.Date("2015-04-01"), y = 100*y.min, label = "$11 \nmin \nwage "), size = 2.5, vjust = 0, hjust = 1) +
      # Label -- step-up 2				 
      geom_text(data = cohort.series.forsynth[is.element(w,j) & is.element(stub,outcomes.list[[i]]) & yearquarter==20162 & is.element(region0,region.list), ],
                aes(x = as.Date("2016-01-01"), y = 100*y.min, label = "$13 \nmin \nwage "), size = 2.5, vjust = 0, hjust = 1) +			 
      theme_bw(base_size = 10) + scale_y_continuous(breaks = pretty_breaks(n = 5), limits = c(y.min,y.max))
    
    plot.matrix <- plot.matrix + 
      xlab("") + ylab("Year-over-year change, %") +
      scale_color_manual(values = c("black","dodgerblue")) +
      scale_fill_manual(values = c("grey")) +
      guides(color = guide_legend(""), fill = guide_legend("")) +
      theme(legend.position = "bottom", legend.direction = "horizontal",
            legend.margin = margin(t = -3, unit='mm'),
            legend.key = element_blank(), legend.key.size = unit(2, 'lines'),
            plot.margin = unit(x = c(5,5,5,5), units = "mm"))
    
    ggsave(plot.matrix, 
           file = paste0(path.to.save,"\\figure_a5_",outcomes.list[[i]],"_w",j,".png"), 
           units = "in", width = 6.5, height = 4)		
  }
}

#######################################
### Figure A6: Growth rates         ###
#######################################

for (i in 1:length(outcomes.list)) {
  for (j in w.list) {
    
    y.max <- ceiling(100*max(cohort.series[is.element(w,j) & is.element(stub,outcomes.list[[i]]) & is.element(region0,c(4,5)), y.max], na.rm = TRUE))
    y.min <- floor(100*min(cohort.series[is.element(w,j) & is.element(stub,outcomes.list[[i]]) & is.element(region0,c(4,5)), y.min], na.rm = TRUE))  	  
    
    plot.matrix <- ggplot() + 
      # Great Recession (shaded)
      geom_ribbon(data = cohort.series[is.element(w,j) & is.element(stub,outcomes.list[[i]]) & is.element(region0,4), ],
                  aes(x = yq, ymin = recession_min, ymax = recession_max, fill = "3"), alpha = 0.5) +
      # MW passage (vertical line)
      geom_vline(data = cohort.series.forsynth[is.element(w,j) & is.element(stub,outcomes.list[[i]]), ],
                 aes(xintercept = as.numeric(as.Date("2014-06-10"))), color = "red", linetype = "solid") +			 
      # MW step-up 1 (vertical line)
      geom_vline(data = cohort.series.forsynth[is.element(w,j) & is.element(stub,outcomes.list[[i]]) & is.element(region0,region.list), ],
                 aes(xintercept = as.numeric(as.Date("2015-04-01"))), color = "red", linetype = "dashed") +			 
      # MW step-up 2 (vertical line)
      geom_vline(data = cohort.series.forsynth[is.element(w,j) & is.element(stub,outcomes.list[[i]]) & is.element(region0,region.list), ],
                 aes(xintercept = as.numeric(as.Date("2016-01-01"))), color = "red", linetype = "dashed") +			 
      # Zero level (horizontal line)
      geom_hline(data = cohort.series.forsynth[is.element(w,j) & is.element(stub,outcomes.list[[i]]) & is.element(region0,region.list), ],
                 aes(yintercept = 0), color = "#444444") +	
      # Observed PUMAs outside of Seattle									
      geom_line(data = cohort.series[is.element(w,j) & is.element(stub,outcomes.list[[i]]) & is.element(region0,c(4,5)), ],
                aes(x = yq, y = 100*(dY-1), color = factor(2), group = region_puma_id0), size = 0.5) +
      # Observed Seattle									
      geom_line(data = cohort.series.forsynth[is.element(w,j) & is.element(stub,outcomes.list[[i]]) & is.element(region0,1), ],
                aes(x = yq, y = 100*(dY-1), color = factor(1)), size = 1) +
      # Label -- passage
      geom_text(data = cohort.series[is.element(w,j) & is.element(stub,outcomes.list[[i]]) & yearquarter==20142 & is.element(region0,4), ],
                aes(x = as.Date("2014-06-10"), y = 100*y.min, label = "Min \n wage \npassed "), size = 2.5, vjust = 0, hjust = 1) +	
      # Label -- step-up 1		 
      geom_text(data = cohort.series[is.element(w,j) & is.element(stub,outcomes.list[[i]]) & yearquarter==20152 & is.element(region0,4), ],
                aes(x = as.Date("2015-04-01"), y = 100*y.min, label = "$11 \nmin \nwage "), size = 2.5, vjust = 0, hjust = 1) +
      # Label -- step-up 2				 
      geom_text(data = cohort.series[is.element(w,j) & is.element(stub,outcomes.list[[i]]) & yearquarter==20162 & is.element(region0,4), ],
                aes(x = as.Date("2016-01-01"), y = 100*y.min, label = "$13 \nmin \nwage "), size = 2.5, vjust = 0, hjust = 1) +			 
      theme_bw(base_size = 10) + scale_y_continuous(breaks = pretty_breaks(n = 5), limits = c(y.min,y.max))
    
    plot.matrix <- plot.matrix + 
      xlab("") + ylab("Year-over-year change, %") +
      scale_color_manual(values = c("black","dark grey"), labels = c("Seattle","PUMAs in WA\noutside of King County")) +
      scale_fill_manual(values = c("grey"), labels = "The Great Recession\n(nationwide)") +
      guides(color = guide_legend(""), fill = guide_legend("")) +
      theme(legend.position = "bottom", legend.direction = "horizontal",
            legend.margin = margin(t = -3, unit='mm'),
            legend.key = element_blank(), legend.key.size = unit(2, 'lines'),
            plot.margin = unit(x = c(5,5,5,5), units = "mm"))
    
    ggsave(plot.matrix, 
           file = paste0(path.to.save,"\\figure_a6_rates_",outcomes.list[[i]],"_w",j,".png"), 
           units = "in", width = 6.5, height = 4)		
  }
}
