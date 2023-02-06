# Data stubs 
stub <- "aggregate_establishments_cumulative_all"
stub.DiD <- "region_level_cumulative"

############################
### Choose specification ###
############################

var <- c("d_cum_hours_flow","d_cum_nworkers_beg" ,"d_cum_mean_wagerate","d_cum_payroll_flow")
wge = seq(11,24)  
mode.changes = "growth_rates"
stub.save <- "jobs_cumulative_all"

# Employment variable? Then estimate wage effect as well
est.elast = 1

# Choose the number of factors to use in Interactive Fixed Effects estimation
# Note: cannot be larger than n_period - n_coefficients - 1 (i.e. 33)
N_factors = 30

# Build a vector of outcomes to use in regressions
outcomes <- c()
for(i in var){
  for(j in wge){
    outcomes <- c(outcomes,paste0(i,j))
  }
}

# ###########################
# ### Tables with results ###
# ### Elasticities        ###
# ###########################

results.coefs.all<- readRDS(paste0(path.to.save,stub.save,"_coef_CI_",mode.changes,".Rds"))

results.coefs.all<- data.table(results.coefs.all)

results.coefs.best <- results.coefs.all[method == "Synth_exclKing" & grepl("18",variable_name), ]

# Elasticity using the change in statutory minimum wage
results.coefs.best[T <= 6 , elast.dMW := coefcum / ((11-9.47)/9.47)  ]
results.coefs.best[T > 6 , elast.dMW := coefcum / ((13-9.47)/9.47)  ]

results.coefs.best[T <= 6 , CIelast_lower.dMW := CIcum_lower95 / ((11-9.47)/9.47)  ]
results.coefs.best[T > 6 , CIelast_lower.dMW := CIcum_lower95 / ((13-9.47)/9.47)  ]
results.coefs.best[T <= 6 , CIelast_upper.dMW := CIcum_upper95 / ((11-9.47)/9.47)  ]
results.coefs.best[T > 6 , CIelast_upper.dMW := CIcum_upper95 / ((13-9.47)/9.47)  ]

results.coefs.best[ , elast_text := as.character(round(elast,3))]
results.coefs.best[ , CIelast_lower_text := as.character(round(CIelast_lower95,3))]
results.coefs.best[ , CIelast_upper_text := as.character(round(CIelast_upper95,3))]
results.coefs.best[ , elast_dMW_text := as.character(round(elast.dMW,3))]
results.coefs.best[ , CIelast_dMW_lower_text := as.character(round(CIelast_lower.dMW,3))]
results.coefs.best[ , CIelast_dMW_upper_text := as.character(round(CIelast_upper.dMW,3))]

table.to.save <- results.coefs.best[variable_name=="d_cum_hours_flow18" & T > 3, 
                                    c("variable_name","T","elast_text","CIelast_lower_text","CIelast_upper_text",
                                      "elast_dMW_text","CIelast_dMW_lower_text","CIelast_dMW_upper_text")]

write.csv(as.matrix(table.to.save), file = paste0(path.to.save,"table_10d.csv"))

