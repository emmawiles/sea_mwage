###########################
### Paths and filenames ###
###########################

# Shape file
path.to.maps <- paste0(path.to.data,"\\tl_2010_53_puma10")

# Data stubs 
stub.save <- "jobs_cumulative_all" 

#######################
### Define outcomes ###
#######################

# Wage bins -- from jobs paying (<=$9.99) to (<=$39.99)
wge = 18 # Jobs paying <$19

# Stubs for outcomes -- variables 
var <- c("d_cum_hours_flow","d_cum_nworkers_beg" ,"d_cum_mean_wagerate","d_cum_payroll_flow")
# Note on variable notation:
# d_cum_hours_flow -- year-over-year change in hours worked in jobs paying <=w.99, where w=18 for example
# d_cum_nworkers_beg -- year-over-year change in number of beginning-of-quarter jobs paying <=w.99, where w=18 for example
# d_cum_mean_wagerate -- year-over-year change in average wage rate (weighted by hours) in jobs paying <=w.99, where w=18 for example
# d_cum_payroll_flow -- year-over-year change in total payroll to jobs paying <=w.99, where w=18 for example

# Build a vector of outcomes to use in regressions
outcomes <- c()
for(i in var){
  for(j in wge){
    outcomes <- c(outcomes,paste0(i,j))
  }
}

outcomes.labels <- c("Quarterly hours worked",
                     "Number of beginning of quarter jobs","Average hourly wage rate",
                     "Quarterly payroll")
outcomes.labels.measure <- c(", 000s hours",", number of jobs",", $",", $")

#################
### Load data ###
#################

# Load data

results.coefs.all <- readRDS(paste0(path.to.save,stub.save,"_coef_CI_growth_rates.Rds"))

# Choose number of factors which minimimizes MSE for each outcome
results.coefs.all <- data.table(results.coefs.all, key = c("variable_name","method","N_factors"))

results.coefs.all$method_long[results.coefs.all$method == "DiD_King"] <- "1: Diff-in-Diffs,\ncp. to King County"
results.coefs.all$method_long[results.coefs.all$method == "DiD_SKP"] <- "2: Diff-in-Diffs,\ncp. to Snohomish,\nKitsap and Pierce Counties"
results.coefs.all$method_long[results.coefs.all$method == "Synth_exclKing"] <- "3: Synthetic Control\nwith s.e. based on ''placebo in space'',\ncp. to WA excl. King County"
results.coefs.all$method_long[results.coefs.all$method == "Int_FE_exlKing"] <- "4: Interactive Effects\nwith optimal number factors,\ncp. to WA excl. King County"

results.coefs.all[ , variable_name := gsub("\\_","",as.character(variable_name))]
results.coefs.all[ , variable_name := gsub("beg","",as.character(variable_name))]
results.coefs.all[ , variable_name := gsub("flow","",as.character(variable_name))]
results.coefs.all[ , w := as.integer(str_sub(variable_name,-2,-1)) + 1]
results.coefs.all[ , stub := str_sub(variable_name,1,-3)]
results.coefs.all[is.na(w) , stub := str_sub(variable_name,1,-2)]
results.coefs.all[is.na(w) , w := as.integer(str_sub(variable_name,-1,-1)) + 1]

results.coefs.all[ , T.labeled := ordered(T, 
                                          labels = c("2014.3","2014.4","2015.1",
                                                     "$11 Minimum Wage,\n2015.2","$11 Minimum Wage,\n2015.3","$11 Minimum Wage,\n2015.4",
                                                     "$13 Minimum Wage,\n2016.1","$13 Minimum Wage,\n2016.2","$13 Minimum Wage,\n2016.3"))]

results.coefs.all[ , coefcum := coefcum ]
results.coefs.all[ , secum := secum  ]
results.coefs.all[ , CIcum_lower := coefcum + qnorm(0.025)*secum ]
results.coefs.all[ , CIcum_upper := coefcum + qnorm(0.975)*secum ]

results.coefs.all[ , N_optimal_IC2.labeled := ordered(N_optimal_IC2, labels = c("No","Yes"))]

# Keep only those which minimize MSE
results.coefs.best <- results.coefs.all[N_optimal_IC2 == 1, ]


results.coefs.best$tstat <- results.coefs.best$coef / results.coefs.best$se
results.coefs.best[ , coefcum := coefcum ]
results.coefs.best[ , secum := secum  ]
results.coefs.best[ , CIcum_lower := coefcum + qnorm(0.025)*secum ]
results.coefs.best[ , CIcum_upper := coefcum + qnorm(0.975)*secum ]

# Add a variable for wagerate cuttoff
results.coefs.best[ , variable_name := gsub("\\_","",as.character(variable_name))]
results.coefs.best[ , variable_name := gsub("beg","",as.character(variable_name))]
results.coefs.best[ , variable_name := gsub("flow","",as.character(variable_name))]
results.coefs.best[ , w := as.integer(str_sub(variable_name,-2,-1)) + 1]
results.coefs.best[ , stub := str_sub(variable_name,1,-3)]
results.coefs.best[is.na(w) , stub := str_sub(variable_name,1,-2)]
results.coefs.best[is.na(w) , w := as.integer(str_sub(variable_name,-1,-1)) + 1]

# MW. stepup
results.coefs.best[T < 4 , MW.step := 0 ]
results.coefs.best[T >= 4 & T <= 6 , MW.step := 11 ]
results.coefs.best[T >= 7 & T <= 9 , MW.step := 13 ]

results.coefs.best[ , MW.step.labeled := ordered(MW.step, labels = c("Pre-MW increase","$11 min wage","$13 min wage"))]
results.coefs.best[ , T.labeled := ordered(T, 
                                           labels = c("2014.3","2014.4","2015.1",
                                                      "$11 Minimum Wage,\n2015.2","$11 Minimum Wage,\n2015.3","$11 Minimum Wage,\n2015.4",
                                                      "$13 Minimum Wage,\n2016.1","$13 Minimum Wage,\n2016.2","$13 Minimum Wage,\n2016.3"))]

###########################################################
### Interactive Fixed effects --                        ###
### how much the coef varies with the number of factors	###
###########################################################

w.toplot = 19
outcomes.stubs <- c("dcumhours","dcummeanwagerate","dcumnworkers","dcumpayroll")

for (j in 1:length(outcomes.stubs)) {
  plot.coef <- ggplot(data = results.coefs.all[is.element(stub,outcomes.stubs[[j]]) &
                                                 is.element(method,c("Int_FE_exlKing")) & is.element(w,w.toplot) & is.element(T,c(6,9)), ]) + 
    geom_hline(aes(yintercept = 0), color = "black") +
    geom_errorbar(aes(x = N_factors, ymin = CIcum_lower, ymax = CIcum_upper, color = N_optimal_IC2.labeled), width = 0.2) +
    geom_point(aes(x = N_factors, y = coefcum, color = N_optimal_IC2.labeled, shape = N_optimal_IC2.labeled), size = 2) + 
    facet_wrap(~T.labeled, ncol = 1)
  
  plot.coef <- plot.coef + theme_bw(base_size = 12) + 
    xlab("Number of factors") + 
    ylab("Coefficient and 95% Confidence Interval") +
    scale_x_continuous(breaks = seq(1,30,2)) +
    theme(legend.position = "bottom", legend.direction = "horizontal",
          legend.margin = margin(t = -3, unit='mm'),
          legend.key = element_blank(), legend.key.size = unit(1.5, 'lines'),
          plot.margin = unit(x = c(5,5,5,5), units = "mm")) +
    scale_color_manual(name = "Optimal number of factors?", 
                       values = c("black","orange"), labels = c("No","Yes")) +
    guides(shape = FALSE,
           color = guide_legend(override.aes = 
                                  list(color = c("black","orange"),
                                       shape = c(16,17))))	
  
  ggsave(plot.coef, file = paste0(path.to.save,"figure_a2_sensitivity_N_factors_",outcomes.stubs[[j]],".png"),
         width = 6, height = 7.5, units = "in")			
}


########################################
### Figure A1                        ###
########################################

### Load weights for Synthetic control ###
weights.synth.exclKing <- readRDS(paste0(path.to.save,stub.save,"_synth_weights_exclKing_growth_rates.Rds"))

### Keep only main outcomes ###
weights.synth.exclKing <- weights.synth.exclKing[is.element(weights.synth.exclKing$variable,outcomes) & grepl("18",weights.synth.exclKing$variable) , ]

weights.synth.exclKing <- reshape2::dcast(weights.synth.exclKing, region_puma_id0 ~ variable, value.var = "weights")

corr.table <- psych::corr.test(weights.synth.exclKing[ , grepl("cum",names(weights.synth.exclKing))])

weights.synth.exclKing.long <- reshape2::melt(weights.synth.exclKing, id.vars = "region_puma_id0")
weights.synth.exclKing.long$puma_id <- as.integer(substr(weights.synth.exclKing.long$region_puma_id0,2,6))

# Load the map of PUMAS
pumas = rgdal::readOGR(path.to.maps, "tl_2010_53_puma10", stringsAsFactors=FALSE)
pumas$puma_id <- as.integer(pumas$PUMACE10)
pumas.names <- pumas@data[ , c("puma_id","NAMELSAD10")]
rm(pumas)

# Merge names back to weights
weights.synth.exclKing.long <- merge(weights.synth.exclKing.long, pumas.names, by = "puma_id", all.x = TRUE, all.y = FALSE)
weights.synth.exclKing.long$name.id <- paste0(weights.synth.exclKing.long$puma_id, ", ", weights.synth.exclKing.long$NAMELSAD10)

# Label variables
weights.synth.exclKing.long$var.labeled[weights.synth.exclKing.long$variable=="d_cum_payroll_flow18"] <- "Payroll"
weights.synth.exclKing.long$var.labeled[weights.synth.exclKing.long$variable=="d_cum_mean_wagerate18"] <- "Wages"
weights.synth.exclKing.long$var.labeled[weights.synth.exclKing.long$variable=="d_cum_hours_flow18"] <- "Hours"
weights.synth.exclKing.long$var.labeled[weights.synth.exclKing.long$variable=="d_cum_nworkers_beg18"] <- "Number of jobs"

vars.colors <- brewer.pal(length(outcomes), "Set1")

plot.synth <- ggplot(weights.synth.exclKing.long[is.element(weights.synth.exclKing.long$variable, outcomes), ], 
                     aes(y = as.factor(name.id), x = 100*value, 
                         color = var.labeled, shape = var.labeled)) +
  geom_point(size = 1.5)  + 
  theme_bw(base_size = 8)

plot.synth <- plot.synth + 
  xlab("Weight in synthetic control, %") + ylab("") +
  scale_x_continuous(breaks = seq(0,30,5), limits = c(0,35)) + 
  scale_color_manual(values = vars.colors) +
  scale_shape_manual(values = c(15,16,17,18)) +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.key = element_blank(), legend.key.size = unit(1, 'lines'),
        plot.margin = unit(x = c(0,0,0,0), units = "mm")) +
  guides(shape = FALSE, 
         color = guide_legend("", 
                              override.aes = list(color = vars.colors,
                                                  shape = c(15,16,17,18)),
                              nrow = 2, byrow = TRUE))              

ggsave(plot.synth, file = paste0(path.to.save,"figure_a1_synthetic_control_weights.png"), 
       width = 6.5, height = 7.5, units = "in")

