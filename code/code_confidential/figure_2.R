

######################
### Load the data ###
######################

	hist.detailed <- read.dta13(paste0(path.to.data,"WageDistribution_NBER_WP_v2.dta"))
	hist.detailed <- data.table(hist.detailed, key = c("yearquarter","region","series_type","MW_stepup","period_type","w_grid"))
	
	# Share of total hours (for plotting distributions)
	hist.detailed[, s_hrs := hours / sum(hours), by = c("yearquarter","region","series_type","MW_stepup","period_type")]
	# Cumulative share (for CDF)
	hist.detailed[, cum_s_hrs := cumsum(s_hrs), by = c("yearquarter","region","series_type","MW_stepup","period_type")]
	# Cumulative hours
	hist.detailed[, cum_hrs := cumsum(hours), by = c("yearquarter","region","series_type","MW_stepup","period_type")]
	
	# Extra label -- combine entry and exit into 1
	hist.detailed[ , series_type2 := series_type ]
	hist.detailed[series_type==1, series_type2 := 3]
	
	# Interact step-up with series type
	# (for pretty facetting)
	hist.detailed[ , MW_series := 100*series_type2 + MW_stepup]
			
	# Create data for pretty histograms (10 cent bins)	
	hist.10_3 = as.data.frame(hist.detailed)
	hist.10_2 = as.data.frame(hist.detailed)
	hist.10_3$w_grid_10 <- hist.10_3$w_grid - 0.1 - 0.05
	hist.10_2$w_grid_10 <- hist.10_2$w_grid - 0.1 + 0.05
	hist.10_3$type <- 2
	hist.10_2$type <- 1
	
	hist.10.forstep <- rbind(hist.10_3,hist.10_2)
	hist.10.forstep <- data.table(hist.10.forstep, key = c("yearquarter","region","series_type","series_type2",
			"MW_stepup","period_type","w_grid_10","w_grid","type"))
	hist.10.forstep[ , w_grid_10 := round(w_grid_10,2)]
	setorder(hist.10.forstep,yearquarter,region,series_type,series_type2,MW_stepup,period_type,w_grid_10,w_grid,type)
	
	hist.10.forstep[ , period.type.labeled := ordered(period_type, 
		labels = c("Before\nthe Minimum\nWage Hike","After\nthe Minimum\nWage Hike"))]
	hist.10.forstep[ , stepup.labeled := ordered(MW_stepup, 
		labels = c("Before Passage of the Minimum Wage:\n2012.2 vs 2013.2","$11 Minimum Wage:\n2014.2 vs 2015.2",
					"$13 Minimum Wage:\n2015.2 vs 2016.2","$15 Minimum Wage:\n2016.2 vs 2017.2"))]
	hist.10.forstep[ , stepup.labeled.alt := ordered(MW_stepup, 
		labels = c("2012.2 vs 2013.2","2014.2 vs 2015.2","2015.2 vs 2016.2","2016.2 vs 2017.2"))]
	hist.10.forstep[ , region.labeled := ordered(region, labels = c("Seattle","SeaTac",
						"Outlying King County\n(excl. Seattle and SeaTac)","Snohomish, Kitsap, and Pierce Counties",
						"Washington outside of\nSnohomish, Kitsap, and Pierce Counties","Not geocoded"))]
	
	# MW under different schedules
	# Note: Min Wage schedule has been adjusted using CPI-W
	# to be in 2015.2 prices
	hist.10.forstep[is.na(MW_stepup)==FALSE , MW1 := 10*(MW_stepup==11) + (10.50-0.1)*(MW_stepup==13) + (11.00-0.3)*(MW_stepup==15)]
	hist.10.forstep[is.na(MW_stepup)==FALSE , MW2 := 11*(MW_stepup==11) + (12.00-0.1)*(MW_stepup==13) + (13.00-0.3)*(MW_stepup==15)]
	hist.10.forstep[is.na(MW_stepup)==FALSE , MW3 := 11*(MW_stepup==11) + (12.50-0.1)*(MW_stepup==13) + (13.50-0.3)*(MW_stepup==15)]
	hist.10.forstep[is.na(MW_stepup)==FALSE , MW4 := 11*(MW_stepup==11) + (13.00-0.1)*(MW_stepup==13) + (15.00-0.4)*(MW_stepup==15)]

######################
### Histograms     ###
### All businesses ###
######################
			
	####################################################
	### Histogram + cumulative dist. of hours worked ###
	### Seattle                                      ###
	### (10 cent bins)                               ###
	### absolute number of hours, (000s)             ###
	####################################################

	note.abs <- paste(c("Source: UI records from WA, 2006-2016. Sample: Locatable employers",
			"Left panel ($11 Minimum Wage) shows the change in the wage distribution between 2014.2 and 2015.2.\n",
			"Right panel ($13 Minimum Wage) shows the change in the wage distribution between 2015.2 and 2016.2.\n",
			"Wages have been adjusted for inflation using CPI-W to represent the price level of 2015.2.",
			"Dotted lines show the minimum wage schedules."), collapse = "")	
			
	plot.hist.10 <- ggplot(data = hist.10.forstep[region==1 & w_grid_10 <= 25 & 
							is.na(period_type)==FALSE  & is.na(MW_stepup)==FALSE &
							MW_stepup != 15 & series_type2 == 0, ]) +
	      geom_area(aes(x = w_grid_10, y = hours/1000, 
	                color = factor(period.type.labeled), fill = factor(period.type.labeled)), position = "identity") +
	      geom_vline(aes(xintercept = MW1), linetype = "dotted", color = "black", size = 0.75) +
	      geom_vline(aes(xintercept = MW2), linetype = "dotted", color = "black", size = 0.75) +
	      geom_vline(aes(xintercept = MW3), linetype = "dotted", color = "black", size = 0.75) +
	      geom_vline(aes(xintercept = MW4), linetype = "dotted", color = "black", size = 0.75) +
				facet_wrap(~ stepup.labeled, nrow = 1) + theme_bw(base_size = 10) +
				scale_x_continuous(breaks = seq(9,25,1), limits = c(9,25)) +
				#scale_y_continuous(breaks = seq(0,1.5e3,500), limits = c(0,1.5e3)) +
				scale_color_manual(name = "", values = c("grey","red")) +
				scale_fill_manual(name = "", values = c("grey","NA")) +
				ylab("Quarterly hours worked, 000s") +
				xlab("Wage bin, $") +
				theme(legend.position = "right", legend.direction = "vertical",
					legend.key = element_blank(), legend.key.size = unit(2, 'lines'),
					legend.text.align = 0.5, legend.margin = margin(t=-3, r=-1, b=-1, l=-1, unit = "mm"),
					plot.margin = unit(x = c(1,1,1,1), units = "mm"))
	
	ggsave(plot.hist.10, file = paste0(path.to.save,"figure_2_hist.png"), width = 11, height = 3.25, units = "in")					

	plot.hist.10 <- ggplot(data = hist.10.forstep[region==1 & w_grid_10 <= 25 & 
							is.na(period_type)==FALSE  & is.na(MW_stepup)==FALSE &
							MW_stepup != 15 & series_type2 == 0, ]) +
	      geom_area(aes(x = w_grid_10, y = cum_hrs/1000, 
	                color = factor(period.type.labeled), fill = factor(period.type.labeled)), position = "identity") +
	      geom_vline(aes(xintercept = MW1), linetype = "dotted", color = "black", size = 0.75) +
	      geom_vline(aes(xintercept = MW2), linetype = "dotted", color = "black", size = 0.75) +
	      geom_vline(aes(xintercept = MW3), linetype = "dotted", color = "black", size = 0.75) +
	      geom_vline(aes(xintercept = MW4), linetype = "dotted", color = "black", size = 0.75) +
				facet_wrap(~ stepup.labeled, nrow = 1) + theme_bw(base_size = 10) +
				scale_x_continuous(breaks = seq(9,25,1), limits = c(9,25)) +
				#scale_y_continuous(breaks = seq(0,1.5e3,500), limits = c(0,1.5e3)) +
				scale_color_manual(name = "", values = c("grey","red")) +
				scale_fill_manual(name = "", values = c("grey","NA")) +
				ylab("Quarterly hours worked, 000s") +
				xlab("Wage bin, $") +
				theme(legend.position = "right", legend.direction = "vertical",
					legend.key = element_blank(),legend.key.size = unit(2, 'lines'),
					legend.text.align = 0.5, legend.margin = margin(t=-3, r=-1, b=-1, l=-1, unit = "mm"),
					plot.margin = unit(x = c(1,1,1,1), units = "mm"))
	
	ggsave(plot.hist.10, file = paste0(path.to.save,"figure_2_cum.png"), width = 11, height = 3.25, units = "in")


