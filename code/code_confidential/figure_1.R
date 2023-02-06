

####################
### Choose stubs ###
####################

stub.save <- "transitions_to_multisite"

outcomes.list <- c("P_multiest_cond")

########################################
###                                  ###
### Transitions to multi-site firms  ###
### for jobs which paid <$19 in t-4  ###
###                                  ###
########################################

  cohort.series <- read.dta13(paste0(path.to.data,"transitions_to_multisite_v6.dta"))
  cohort.series <- data.table(cohort.series, key = c("region","puma_id","year","quarter","yearquarter"))
  cohort.series <- cohort.series[yearquarter >= 20061 & yearquarter<= 20163, ]

  # Aggregate at region level (for plots)
  result.region <- cohort.series[ , lapply(.SD, sum), by = c("region","year","quarter","yearquarter")]
  result.region[ , P_withjob := with_job_F4 / N]
  result.region[ , P_multiest_uncond := multiest_F4 / N]
  result.region[ , P_multiest_cond := multiest_F4 / with_job_F4]
  result.region[ , puma_id := NULL]
  
  # Melt for analysis  
  cohort.series <- melt(cohort.series, id.vars = c("region","puma_id","yearquarter","N"))
  cohort.series <- data.table(cohort.series, key = c("region","puma_id","yearquarter","variable"))
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
  
###########################################  
###                                     ###  
### Plot probability by region and time ###
###                                     ###
###########################################
  
  outcomes.list <- c("P_multiest_cond")
  region.list <- c(1,3,4)
  
  result.region <- melt(result.region, id.vars = c("region","year","quarter","date_q","yearquarter"))

  result.region[ , yq := as.Date(as.yearqtr(year + 0.25*(quarter-1)))]
  result.region[is.element(region,region.list), y.min:= min(value, na.rm = TRUE), by = c("variable")]
  result.region[is.element(region,region.list), y.max := max(value, na.rm = TRUE), by = c("variable")]
  
  result.region[ , yq := as.Date(as.yearqtr(year + 0.25*(quarter-1)))]
  result.region[yq >= as.Date(as.yearqtr(2007.75)) & yq <= as.Date(as.yearqtr(2009.50)), recession_max := 100*y.max ]
  result.region[yq >= as.Date(as.yearqtr(2007.75)) & yq <= as.Date(as.yearqtr(2009.50)), recession_min := 0 ]
  
  # Shift everything to the middle of the quarter instead of beginning of the quarter
  #result.region[ , yq := yq + 365/(4*2)]
  
  ### Add labels 
  result.region[ , region.labeled := ordered(10-region, labels = c("NA","Washington State excl.\nKing, Snohomish, Pierce,\nand Kitsap Counties",
                                                             "Snohomish, Pierce,\nand Kitsap Counties","King County excl.\nSeattle and SeaTac","SeaTac","Seattle"))]

  for (i in 1:length(outcomes.list)) {
       

      plot.matrix <- ggplot() + 
        geom_ribbon(data = result.region[is.element(variable,outcomes.list[[i]]) & is.element(region,region.list), ],
                     aes(x = yq, ymin = recession_min, ymax = recession_max, fill = "The Great Recession\n(nationwide)"), alpha = 0.5) +
        geom_vline(data = result.region[is.element(variable,outcomes.list[[i]]), ],
                   aes(xintercept = as.numeric(as.Date("2014-06-10"))), color = "red", linetype = "solid") +
        geom_vline(data = result.region[is.element(variable,outcomes.list[[i]]) & is.element(region,region.list), ],
                   aes(xintercept = as.numeric(as.Date("2015-04-01"))), color = "red", linetype = "dashed") +
        geom_vline(data = result.region[is.element(variable,outcomes.list[[i]]) & is.element(region,region.list), ],
                   aes(xintercept = as.numeric(as.Date("2016-01-01"))), color = "red", linetype = "dashed") +
        geom_line(data = result.region[is.element(variable,outcomes.list[[i]]) & is.element(region,region.list), ],
                  aes(x = yq, y = 100*(value), color = region.labeled, linetype = region.labeled), size = 1) + 
        geom_text(data = result.region[is.element(variable,outcomes.list[[i]]) & yearquarter==20142 & is.element(region,region.list), ],
                   aes(x = as.Date("2014-06-10"), y = 0, label = "Min \n wage \npassed "), size = 2.5, vjust = 0, hjust = 1) +			 
        geom_text(data = result.region[is.element(variable,outcomes.list[[i]]) & region == 1 & year==2015 & quarter == 2, ],
                  aes(x = as.Date("2015-04-01"), y = 0, label = "$11 \nmin \nwage "), size = 2.5, vjust = 0, hjust = 1) +
        geom_text(data = result.region[is.element(variable,outcomes.list[[i]]) & region == 1 & year==2016 & quarter == 1, ],
                  aes(x = as.Date("2016-01-01"), y = 0, label = " $13 \nmin \nwage "), size = 2.5, vjust = 0, hjust = 1) +
        theme_bw(base_size = 10) +
        scale_y_continuous(breaks = pretty_breaks(n = 5), limits = c(0,35))
      
      
      plot.matrix <- plot.matrix + 
        xlab("") + ylab("Probability, %") +
        scale_linetype_manual(name ="", values = c("solid","twodash","solid"), guide = FALSE) + 
        scale_color_manual(name = "", values = c("black","forestgreen","mediumpurple"), breaks=c("Seattle", "King County excl.\nSeattle and SeaTac","Snohomish, Pierce,\nand Kitsap Counties")) +
        scale_fill_manual(name = "", values = c("grey")) +
        guides(color = guide_legend("", nrow = 2, byrow = TRUE), fill = guide_legend("")) + 
        theme(legend.position = "bottom", legend.direction = "horizontal",
              legend.margin = margin(t = -3, unit='mm'),
              legend.key = element_blank(), legend.key.size = unit(1, 'lines'),
              plot.margin = unit(x = c(5,5,5,5), units = "mm")) +
      guides(color = guide_legend(override.aes = 
                                    list(color = c("black","forestgreen","mediumpurple"), 
                                         linetype = c("solid","twodash","solid"))))
     
        
      ggsave(plot.matrix, file = paste0(path.to.save,"\\figure_1.png"), units = "in", width = 6, height = 3.5)		
  }
    