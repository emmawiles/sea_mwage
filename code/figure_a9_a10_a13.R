# Data stubs
stub.save <- "jobs_cumulative_all"
mode.changes<- "growth_rates"

# Define outcomes and labels
outcomes.stubs <- c("dcumhours")
outcomes.stubs.w <- c("dcummeanwagerate")
outcomes.labels.measure <- c("000s hrs",", number of jobs",", $")
methods.preferred <- c("Synth_exclKing")

########################################
### Load data and shape for plotting ###
########################################

# Load data
results.coefs.all <- readRDS(paste0(path.to.save,stub.save,"_coef_CI_", mode.changes, ".Rds"))

# Choose number of factors which minimizes MSE for each outcome
results.coefs.all <- data.table(results.coefs.all, key = c("variable_name","method","N_factors"))
results.coefs.all$method_long[results.coefs.all$method == "DiD_King"] <- "1: Diff-in-Diffs,\ncp. to King County"
results.coefs.all$method_long[results.coefs.all$method == "DiD_SKP"] <- "2: Diff-in-Diffs,\ncp. to Snohomish,\nKitsap and Pierce Counties"
results.coefs.all$method_long[results.coefs.all$method == "Synth_exclKing"] <- "3: Synthetic Control\nwith s.e. based on ''placebo in space'',\ncp. to WA excl. King County"
results.coefs.all$method_long[results.coefs.all$method == "Int_FE_exlKing"] <- "4: Interactive Effects\nwith optimal number factors,\ncp. to WA excl. King County"

# Keep only those which minimize MSE
results.coefs.best <- results.coefs.all[N_optimal_IC2 == 1, ]

# Make confidence intervals numbers instead of logical variables
results.coefs.best <- results.coefs.best %>% mutate(CIelast_lower95 = as.numeric(CIelast_lower95),
                                                    CIelast_upper90 = as.numeric(CIelast_upper90),
                                                    CIelast_lower50 = as.numeric(CIelast_lower50))

# Bounds for CI for elasticity 
results.coefs.best[ , coefcum := 100*coefcum ]
results.coefs.best[ , CIcum_lower50 := 100*CIcum_lower50 ]
results.coefs.best[ , CIcum_upper50 := 100*CIcum_upper50 ]
results.coefs.best[ , CIcum_lower90 := 100*CIcum_lower90 ]
results.coefs.best[ , CIcum_upper90 := 100*CIcum_upper90 ]
results.coefs.best[ , CIcum_lower95 := 100*CIcum_lower95 ]
results.coefs.best[ , CIcum_upper95 := 100*CIcum_upper95 ]

results.coefs.best[ , coefcum.abs := coefcum * Y0/100 ]
results.coefs.best[ , CIcum_lower50.abs := CIcum_lower50 * Y0/100  ]
results.coefs.best[ , CIcum_upper50.abs := CIcum_upper50 * Y0/100  ]
results.coefs.best[ , CIcum_lower90.abs := CIcum_lower90 * Y0/100  ]
results.coefs.best[ , CIcum_upper90.abs := CIcum_upper90 * Y0/100  ]
results.coefs.best[ , CIcum_lower95.abs := CIcum_lower95 * Y0/100  ]
results.coefs.best[ , CIcum_upper95.abs := CIcum_upper95 * Y0/100  ]

# Add a variable for wagerate cuttoff
results.coefs.best[ , variable_name := gsub("\\_","",as.character(variable_name))]
results.coefs.best[ , variable_name := gsub("beg","",as.character(variable_name))]
results.coefs.best[ , variable_name := gsub("flow","",as.character(variable_name))]
results.coefs.best[ , w := as.integer(str_sub(variable_name,-2,-1)) + 1]
results.coefs.best[ , stub := str_sub(variable_name,1,-3)]
results.coefs.best[is.na(w) , stub := str_sub(variable_name,1,-2)]
results.coefs.best[is.na(w) , w := as.integer(str_sub(variable_name,-1,-1)) + 1]

results.coefs.best[stub=="dcumhours", coefcum.abs := coefcum.abs / 1e3]
results.coefs.best[stub=="dcumhours", CIcum_lower50.abs := CIcum_lower50.abs / 1e3]
results.coefs.best[stub=="dcumhours", CIcum_upper50.abs := CIcum_upper50.abs / 1e3]
results.coefs.best[stub=="dcumhours", CIcum_lower90.abs := CIcum_lower90.abs / 1e3]
results.coefs.best[stub=="dcumhours", CIcum_upper90.abs := CIcum_upper90.abs / 1e3]
results.coefs.best[stub=="dcumhours", CIcum_lower95.abs := CIcum_lower95.abs / 1e3]
results.coefs.best[stub=="dcumhours", CIcum_upper95.abs := CIcum_upper95.abs / 1e3]

results.coefs.best[T < 4 , MW.step := 0 ]
results.coefs.best[T >= 4 & T <= 6 , MW.step := 11 ]
results.coefs.best[T >= 7 & T <= 9 , MW.step := 13 ]

results.coefs.best[ , MW.step.labeled := ordered(MW.step, labels = c("Pre-MW increase","$11 min wage","$13 min wage"))]
results.coefs.best[ , T.labeled := ordered(T, 
                                           labels = c("2014.3","2014.4","2015.1",
                                                      "$11 Minimum Wage,\n2015.2","$11 Minimum Wage,\n2015.3","$11 Minimum Wage,\n2015.4",
                                                      "$13 Minimum Wage,\n2016.1","$13 Minimum Wage,\n2016.2","$13 Minimum Wage,\n2016.3"))]

results.coefs.best[ MW.step == 11 & w < 12, coefcum := NA]
results.coefs.best[ MW.step == 11 & w < 12, CIcum_lower50 := NA]
results.coefs.best[ MW.step == 11 & w < 12, CIcum_upper50 := NA] 
results.coefs.best[ MW.step == 11 & w < 12, CIcum_lower90 := NA]
results.coefs.best[ MW.step == 11 & w < 12, CIcum_upper90 := NA]
results.coefs.best[ MW.step == 11 & w < 12, CIcum_lower95 := NA] 
results.coefs.best[ MW.step == 11 & w < 12, CIcum_upper95 := NA]

results.coefs.best[ MW.step == 11 & w < 12, coefcum.abs := NA]
results.coefs.best[ MW.step == 11 & w < 12, CIcum_lower50.abs := NA]
results.coefs.best[ MW.step == 11 & w < 12, CIcum_upper50.abs := NA]
results.coefs.best[ MW.step == 11 & w < 12, CIcum_lower90.abs := NA]
results.coefs.best[ MW.step == 11 & w < 12, CIcum_upper90.abs := NA]
results.coefs.best[ MW.step == 11 & w < 12, CIcum_lower95.abs := NA]
results.coefs.best[ MW.step == 11 & w < 12, CIcum_upper95.abs := NA]

results.coefs.best[ MW.step == 13 & w < 14, coefcum := NA]
results.coefs.best[ MW.step == 13 & w < 14, CIcum_lower50 := NA]
results.coefs.best[ MW.step == 13 & w < 14, CIcum_upper50 := NA]
results.coefs.best[ MW.step == 13 & w < 14, CIcum_lower90 := NA]
results.coefs.best[ MW.step == 13 & w < 14, CIcum_upper90 := NA]
results.coefs.best[ MW.step == 13 & w < 14, CIcum_lower95 := NA] #sttill numeric
results.coefs.best[ MW.step == 13 & w < 14, CIcum_upper95 := NA]

results.coefs.best[ MW.step == 13 & w < 14, coefcum.abs := NA]
results.coefs.best[ MW.step == 13 & w < 14, CIcum_lower50.abs := NA]
results.coefs.best[ MW.step == 13 & w < 14, CIcum_upper50.abs := NA]
results.coefs.best[ MW.step == 13 & w < 14, CIcum_lower90.abs := NA]
results.coefs.best[ MW.step == 13 & w < 14, CIcum_upper90.abs := NA]
results.coefs.best[ MW.step == 13 & w < 14, CIcum_lower95.abs := NA]
results.coefs.best[ MW.step == 13 & w < 14, CIcum_upper95.abs := NA]


###################################################
###                                             ###
### Plot coefficients and s.e. -- all estimates ###
### As a function of cutoff                     ###
###                                             ###
###################################################

colors.CI <- c("#3182bd","#9ecae1","#dee6f7")

###############################
# Figure A9
# Percentage change in wages
###############################

for (j in 1:length(outcomes.stubs.w)) {
  y.min = results.coefs.best[is.element(results.coefs.best$stub,outcomes.stubs.w[[j]]) &
                               is.element(results.coefs.best$method,methods.preferred)  & w >= 11 & w <= 25 & MW.step>0, min(CIcum_lower95)]  
  y.max = results.coefs.best[is.element(results.coefs.best$stub,outcomes.stubs.w[[j]]) &
                               is.element(results.coefs.best$method,methods.preferred)  & w >= 11 & w <= 25 & MW.step>0, max(CIcum_upper95)]  
  
  for (m in 1:length(methods.preferred)) {
    plot.coef <- ggplot(data = results.coefs.best[is.element(results.coefs.best$stub,outcomes.stubs.w[[j]]) &
                                                    is.element(results.coefs.best$method,methods.preferred[[m]])  & w >= 11& w <= 25 & MW.step>0, ]) + 
      geom_hline(aes(yintercept = 0), color = "black") + 
      geom_vline(aes(xintercept = 19), color = "black", linetype = "longdash", size = 0.75) + 
      geom_ribbon(aes(x = w, ymin = CIcum_lower95, ymax = CIcum_upper95, fill = factor(395)), alpha = 1) +		  
      geom_ribbon(aes(x = w, ymin = CIcum_lower90, ymax = CIcum_upper90, fill = factor(290)), alpha = 1) +		  
      geom_ribbon(aes(x = w, ymin = CIcum_lower50, ymax = CIcum_upper50, fill = factor(150)), alpha = 1) +		  
      geom_line(aes(x = w, y = coefcum, color = "Point estimate"), size = 0.8) + 
      facet_wrap(~T.labeled, nrow = 2)
    plot.coef <- plot.coef + theme_bw(base_size = 12) + 
      xlab("Jobs Paying Less than this Amount") + ylim(y.min,y.max) +
      ylab("Estimated % Change Relative to Quarter Before Passage (2014.2)") +
      scale_x_continuous(breaks = unique(results.coefs.best$w), minor_breaks = NULL) +
      scale_fill_manual(name = "", values = colors.CI, labels = c("50% CI","90% CI","95% CI")) +
      scale_color_manual(name = "", values = c("black")) +
      theme(legend.position = "bottom", legend.direction = "horizontal",
            legend.margin = margin(t = -3, unit='mm'),
            legend.key = element_blank(), legend.key.size = unit(1.5, 'lines'),
            plot.margin = unit(x = c(2,2,2,2), units = "mm"))
    
    ggsave(plot.coef, file = paste0(path.to.save,"figure_a9_sensitivity_to_wagebin_",outcomes.stubs.w[[j]],"_percentage.png"),
           width = 9, height = 5.5, units = "in")
  }						
}

#############################
# Figure A10
# Absolute changes in hours
#############################
for (j in 1:length(outcomes.stubs)) {
  y.min = results.coefs.best[is.element(results.coefs.best$stub,outcomes.stubs[[j]]) &
                               is.element(results.coefs.best$method,methods.preferred)  & w >= 11 & w <= 25 & MW.step>0, min(CIcum_lower95.abs)]  
  y.max = results.coefs.best[is.element(results.coefs.best$stub,outcomes.stubs[[j]]) &
                               is.element(results.coefs.best$method,methods.preferred)  & w >= 11 & w <= 25 & MW.step>0, max(CIcum_upper95.abs)]  
  
  for (m in 1:length(methods.preferred)) {
    plot.coef <- ggplot(data = results.coefs.best[is.element(results.coefs.best$stub,outcomes.stubs[[j]]) &
                                                    is.element(results.coefs.best$method,methods.preferred[[m]])  & w >= 11 & w <= 25 & MW.step>0, ]) + 
      geom_hline(aes(yintercept = 0), color = "black") + 
      geom_vline(aes(xintercept = 19), color = "black", linetype = "longdash", size = 1) + 
      geom_ribbon(aes(x = w, ymin = CIcum_lower95.abs, ymax = CIcum_upper95.abs, fill = factor(395)), alpha = 1) +		  
      geom_ribbon(aes(x = w, ymin = CIcum_lower90.abs, ymax = CIcum_upper90.abs, fill = factor(290)), alpha = 1) +		  
      geom_ribbon(aes(x = w, ymin = CIcum_lower50.abs, ymax = CIcum_upper50.abs, fill = factor(150)), alpha = 1) +		  
      geom_line(aes(x = w, y = coefcum.abs, color = "Point estimate"), size = 0.8) + 
      facet_wrap(~T.labeled, nrow = 2)
    plot.coef <- plot.coef + theme_bw(base_size = 12) + 
      xlab("Jobs Paying Less than this Amount") + ylim(y.min,y.max) +
      ylab(paste0("Estimated Impact Relative to Quarter Before Passage (2014.2)\n",outcomes.labels.measure[j])) +
      scale_x_continuous(breaks = unique(results.coefs.best$w), minor_breaks = NULL) +
      scale_fill_manual(name = "", values = colors.CI, labels = c("50% CI","90% CI","95% CI")) +
      scale_color_manual(name = "", values = c("black")) +
      theme(legend.position = "bottom", legend.direction = "horizontal",
            legend.margin = margin(t = -3, unit='mm'),
            legend.key = element_blank(), legend.key.size = unit(1.5, 'lines'),
            plot.margin = unit(x = c(2,2,2,2), units = "mm"))
    
    ggsave(plot.coef, file = paste0(path.to.save,"figure_a10_sensitivity_to_wagebin_",outcomes.stubs[[j]],"_absolute.png"),
           width = 9, height = 5.5, units = "in")
  }						
}

##############################
# Figure A13
# Elasticity of labor demand
##############################

results.coefs.best[w < 12 & MW.step == 11, elast := NA ]
results.coefs.best[w < 12 & MW.step == 11, CIelast_lower50 := NA ]
results.coefs.best[w < 12 & MW.step == 11, CIelast_upper50 := NA ]
results.coefs.best[w < 12 & MW.step == 11, CIelast_lower90 := NA ]
results.coefs.best[w < 12 & MW.step == 11, CIelast_upper90 := NA ]
results.coefs.best[w < 12 & MW.step == 11, CIelast_lower95 := NA ]
results.coefs.best[w < 12 & MW.step == 11, CIelast_upper95 := NA ]

results.coefs.best[w < 14 & MW.step == 13, elast := NA ]
results.coefs.best[w < 14 & MW.step == 13, CIelast_lower50 := NA ]
results.coefs.best[w < 14 & MW.step == 13, CIelast_upper50 := NA ]
results.coefs.best[w < 14 & MW.step == 13, CIelast_lower90 := NA ]
results.coefs.best[w < 14 & MW.step == 13, CIelast_upper90 := NA ]
results.coefs.best[w < 14 & MW.step == 13, CIelast_lower95 := NA ]
results.coefs.best[w < 14 & MW.step == 13, CIelast_upper95 := NA ]


#############################
### Figure A13 Panel A    ###
### 50, 90, and 95 % CI   ###
#############################

for (j in 1:(length(outcomes.stubs))) {
  for (m in 1:(length(methods.preferred))) {
    plot.coef <- ggplot(data = results.coefs.best[is.element(results.coefs.best$stub,outcomes.stubs[[j]]) &
                                                    is.element(results.coefs.best$method,methods.preferred[[m]]) & w >= 11 & w <= 25 & MW.step>0 & T>=7, ]) +
      geom_hline(aes(yintercept = 0), color = "black") +
      geom_vline(aes(xintercept = 19), color = "dark grey", size = 1) +
      geom_ribbon(aes(x = w, ymin = CIelast_lower95, ymax = CIelast_upper95, fill = factor(395)), alpha = 1) +
      geom_ribbon(aes(x = w, ymin = CIelast_lower90, ymax = CIelast_upper90, fill = factor(290)), alpha = 1) +
      geom_ribbon(aes(x = w, ymin = CIelast_lower50, ymax = CIelast_upper50, fill = factor(150)), alpha = 1) +
      geom_line(aes(x = w, y = elast, color = "Point estimate"), size = 0.8) +
      geom_hline(aes(yintercept = -1), color = "black", linetype = "dashed") +
      annotate("text", x = 13.2, y = -2.3, label = "Elasticity = -1", size = 3) +
      facet_wrap(~T.labeled, nrow = 1)
      
       plot.coef <- plot.coef + theme_bw(base_size = 12) +
      xlab("Jobs Paying Less than this Amount") + ylim(y.min,y.max) +
      ylab("Estimated Elasticity") +
      scale_x_continuous(breaks = unique(results.coefs.best$w), minor_breaks = NULL) +
      scale_fill_manual(name = "", values = colors.CI, labels = c("50% CI","90% CI","95% CI")) +
      scale_color_manual(name = "", values = c("black")) +
      theme(legend.position = "bottom", legend.direction = "horizontal",
            legend.margin = margin(t = -3, unit='mm'),
            legend.key = element_blank(), legend.key.size = unit(1.5, 'lines'),
            plot.margin = unit(x = c(2,2,2,2), units = "mm"))
    
    ggsave(plot.coef, file = paste0(path.to.save,"figure_a13_elasticity_by_wagebin_",outcomes.stubs[[j]],"_50_90_95.png"),
           width = 9, height = 3, units = "in")
  }
}

########################################
### Figure A13 Panel B               ###
###  50% CI only                     ###
########################################

for (j in 1:(length(outcomes.stubs))) {
  for (m in 1:(length(methods.preferred))) {
    plot.coef <- ggplot(data = results.coefs.best[is.element(results.coefs.best$stub,outcomes.stubs[[j]]) &
                                                    is.element(results.coefs.best$method,methods.preferred[[m]]) & w >= 11 & w <= 25 & MW.step>0 & T>=7, ]) +
      geom_hline(aes(yintercept = 0), color = "black") +
      geom_vline(aes(xintercept = 19), color = "dark grey", size = 1) +
      geom_ribbon(aes(x = w, ymin = CIelast_lower50, ymax = CIelast_upper50, fill = factor(150)), alpha = 1) +
      geom_line(aes(x = w, y = elast, color = "Point estimate"), size = 0.8) +
      geom_hline(aes(yintercept = -1), color = "black", linetype = "dashed") +
      annotate("text", x = 13.5, y = -1.6, label = "Elasticity = -1", size = 3) +
      facet_wrap(~T.labeled, nrow = 1) 
 
    plot.coef <- plot.coef + theme_bw(base_size = 12) +
      xlab("Jobs Paying Less than this Amount") + ylim(y.min,y.max) +
      ylab("Estimated Elasticity") +
      scale_x_continuous(breaks = unique(results.coefs.best$w), minor_breaks = NULL) +
      scale_fill_manual(name = "", values = colors.CI, labels = c("50% CI","90% CI","95% CI")) +
      scale_color_manual(name = "", values = c("black")) +
      theme(legend.position = "bottom", legend.direction = "horizontal",
            legend.margin = margin(t = -3, unit='mm'),
            legend.key = element_blank(), legend.key.size = unit(1.5, 'lines'),
            plot.margin = unit(x = c(2,2,2,2), units = "mm"))
    
    ggsave(plot.coef, file = paste0(path.to.save,"figure_a13_elasticity_by_wagebin_",outcomes.stubs[[j]],"_50.png"),
           width = 9, height = 3, units = "in")
  }
}



