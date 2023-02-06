
# Functions used to estimate the impact of minimum wage

############################################
###                                      ###
### Synthetic Control                    ###
###                                      ###
############################################

####################################################
# Procedure used to find synthetic control weights #
# (from Gobillon, Magnac 2016)                     #
####################################################

wipop <- function(Z0,Z1) {
  E <- matrix(1,1,ncol(Z0))
  F <- 1
  
  b <- 1
  
  G <- diag(ncol(Z0))
  H <- matrix(0,ncol(Z0),1)
  
  res <- lsei(A=Z0, B=Z1, E=E, F=F, G=G, H=H, type=2)
  w <- res$X
  
  return(w)
}


SynthTE <- function(y, treated.PUMAS, treated.periods, aggr.method = "average", standardize = FALSE, weights.pre = NULL) {
  
  ### Get variable
  j = unique(y$variable)
  
  ### Control and treatment groups ###
  y0 <- y[is.element(region_puma_id0,treated.PUMAS) == FALSE, ]
  y1 <- y[is.element(region_puma_id0,treated.PUMAS) == TRUE, ]
  
  if (aggr.method == "average") {
    y11 <- y1[ , sum(dY * N)/sum(N), by = c("yearquarter","variable" )] 
    setnames(y11,"V1","dY")
    y11 <- y11[ , list(yearquarter, dY)]
  }
  if (aggr.method == "sum") {
    y11 <- y1[ , sum(dY), by = c("yearquarter","variable" )] 
    setnames(y11,"V1","dY")
    y11 <- y11[ , list(yearquarter, dY)]
  }
  if (aggr.method == "SumDividedByFive") {
    y11 <- y1[ , sum(dY)/5, by = c("yearquarter","variable" )] 
    setnames(y11,"V1","dY")
    y11 <- y11[ , list(yearquarter, dY)]
  }
  
  if (standardize) {
    # Standardize
    y11[is.element(yearquarter,treated.periods)==FALSE , preMW_mean_tmp := mean(dY)]
    y11[ , max_preMW_mean := max(preMW_mean_tmp, na.rm = TRUE)]
    y11[ , preMW_mean := max_preMW_mean]
    y11[ , preMW_mean_tmp := NULL]
    y11[ , max_preMW_mean := NULL]
    y11[is.element(yearquarter,treated.periods)==FALSE , preMW_sd_tmp := sd(dY)]
    y11[ , max_preMW_sd := max(preMW_sd_tmp, na.rm = TRUE)]
    y11[ , preMW_sd := max_preMW_sd]
    y11[ , preMW_sd_tmp := NULL]
    y11[ , max_preMW_sd := NULL]
    y11[ , Y := dY]
    y11[ , dY := (dY - preMW_mean) / preMW_sd]
    
    y0[is.element(yearquarter,treated.periods)==FALSE , preMW_mean_tmp := mean(dY), by = c("region_puma_id0")]
    y0[ , max_preMW_mean := max(preMW_mean_tmp, na.rm = TRUE), by = c("region_puma_id0")]
    y0[ , preMW_mean := max_preMW_mean, by = c("region_puma_id0")]
    y0[ , preMW_mean_tmp := NULL]
    y0[ , max_preMW_mean := NULL]
    y0[is.element(yearquarter,treated.periods)==FALSE , preMW_sd_tmp := sd(dY), by = c("region_puma_id0")]
    y0[ , max_preMW_sd := max(preMW_sd_tmp, na.rm = TRUE), by = c("region_puma_id0")]
    y0[ , preMW_sd := max_preMW_sd, by = c("region_puma_id0")]
    y0[ , preMW_sd_tmp := NULL]
    y0[ , max_preMW_sd := NULL]		
    y0[ , Y := dY]
    y0[ , dY := (dY - preMW_mean) / preMW_sd]		
  }		
  
  ### Reshape data for control units into matrices ###
  y00 <- melt(y0[ , list(region_puma_id0,yearquarter,dY)], id.vars = c("region_puma_id0","yearquarter"))
  y00 <- dcast(y00, yearquarter ~ variable + region_puma_id0)
  
  # Make sure cohorts are ordered correctly
  y00 <- y00[order(yearquarter), ]
  y11 <- y11[order(yearquarter), ]	  
  
  ### Find weights for synthetic controls
  if (is.null(weights.pre)==TRUE) {
    if (aggr.method=="average" | standardize) {
      w00 <- try(wipop(as.matrix(y00[is.element(yearquarter, treated.periods) == FALSE, tail(names(y00),-1), with = F ]), # control units
                       as.matrix(y11[is.element(yearquarter, treated.periods) == FALSE, "dY", with = F]))  # treatment unit
      )
    }
    if ((aggr.method=="sum" | aggr.method=="SumDividedByFive") & standardize==FALSE) {
      if (grepl("nworkers",j)) {
        w00 <- try(wipop(1e-3*as.matrix(y00[is.element(yearquarter, treated.periods) == FALSE, tail(names(y00),-1), with = F ]), # control units
                         1e-3*as.matrix(y11[is.element(yearquarter, treated.periods) == FALSE, "dY", with = F]))  # treatment unit
        )   
      }	
      if (grepl("hours",j) | grepl("payroll",j)) {	
        w00 <- try(wipop(1e-6*as.matrix(y00[is.element(yearquarter, treated.periods) == FALSE, tail(names(y00),-1), with = F ]), # control units
                         1e-6*as.matrix(y11[is.element(yearquarter, treated.periods) == FALSE, "dY", with = F]))  # treatment unit
        )   
      }			   
    }
  }
  
  
  if (is.null(weights.pre)==FALSE) w00 <- as.numeric(weights.pre)
  
  if (class(w00) != "try-error") {
    
    ### Control PUMAS ###
    PUMAS00 <- as.integer(substr(tail(names(y00),-1),4,9))
    
    ### Treatment effect ###
    TE <- c()
    for (i in treated.periods) {
      TE <- c(TE,y11[yearquarter== i , "dY", with = F] - 
                sum(w00*y00[yearquarter== i, tail(names(y00),-1), with = F]))
    }
    
    ### Fitted values ###
    y11hat <- as.matrix(y00[is.element(yearquarter, treated.periods)==FALSE, tail(names(y00),-1), with = F]) %*% w00
    
    ### In-sample prediction (to calculate MSRE) ###
    resid <- as.numeric(y11[is.element(yearquarter,treated.periods)==FALSE, dY]) - y11hat  
    
    ### Abs values (if standardized) ###
    if (standardize) TEabs <- as.numeric(TE)*unique(as.numeric(y11$preMW_sd))
    if (standardize==FALSE) TEabs <- NA
    if (aggr.method=="SumDividedByFive") TE <- as.numeric(TE)*5
    
    ### Cumulative percent -- if est. in abs terms ###
    if (substr(j,1,1)!="d") { 
      Y0 <- tail(as.numeric(y11[is.element(yearquarter,treated.periods)==FALSE, dY]), 1)
      if (aggr.method=="SumDividedByFive") Y0 <- 5*Y0
      if (standardize==FALSE) TEcum <- as.numeric(TE)/Y0
      if (standardize==TRUE) TEcum <- as.numeric(TE)*unique(as.numeric(y11$preMW_sd))/(Y0*unique(as.numeric(y11$preMW_sd)) + unique(as.numeric(y11$preMW_mean)))
    }
    if (substr(j,1,1)=="d") TEcum <- NA
    
    ### Return treatment effect and weights for control units ###
    return(list(TE = as.numeric(TE),
                TEabs = TEabs,
                TEcum = TEcum,
                weights = as.matrix(cbind(PUMAS00,w00)),
                dY = data.frame(y11[is.element(yearquarter,treated.periods)==FALSE, list(yearquarter,dY)]),
                dYhat = y11hat,
                resid = resid,
                RMSE = sqrt(mean(resid^2))))
  }
  
  if (class(w00) == "try-error") {
    
    ### Control PUMAS ###
    PUMAS00 <- as.integer(substr(tail(names(y00),-1),4,9))
    
    ### Return treatment effect and weights for control units ###
    return(list(TE = rep(NA,9), 
                weights = as.matrix(cbind(PUMAS00,rep(NA,length(PUMAS00)))),
                dY = data.frame(y11[is.element(yearquarter, treated.periods)==FALSE,list(yearquarter,dY)], 
                                dYhat = NA,
                                resid = NA,
                                RMSE = NA)))
  }
}	

EstimateSynth <- function(j,df,df_w,est.elast,method.label, PUMAs.Seattle, AllCombinations.ContigPUMAs, treated.periods,
                          aggr.method = "average", standardize = FALSE, CIlevels = 0.95, weights.pre.Seattle = NULL, weights.pre.placebo = NULL) {					
  
  # Estimate TE
  if (is.null(weights.pre.Seattle)==TRUE) results.SynthTE <- SynthTE(df, PUMAs.Seattle, treated.periods, aggr.method, standardize)
  if (is.null(weights.pre.Seattle)==FALSE) results.SynthTE <- SynthTE(df, PUMAs.Seattle, treated.periods, aggr.method, standardize, weights.pre = as.numeric(weights.pre.Seattle$weights))
  
  # Employment variable? Then estimate wage effect as well
  if (est.elast == 1) {
    results.SynthTE.w <- SynthTE(df_w, PUMAs.Seattle, treated.periods, aggr.method, standardize)
  }
  
  ### Bootstrapped CI ###	
  # Note: all possible combinations of continuous PUMAs have been saved before
  
  #	n_cores <- detectCores() 
  #	start.time <- proc.time()
  #	BootCluster <- makeCluster(n_cores) # CSDE Sim has up to 40 (!) cores
  #	clusterExport(BootCluster, c("wipop","SynthTE"))
  #	clusterEvalQ(BootCluster, {
  #		library(data.table)
  #		library(limSolve)
  #	})
  #	Synth.PUMAs.all <- parLapply(BootCluster, 1:B, function(b) SynthTE(y_hyp, bootstrap.PUMAs.inspace[[b]], treated.periods))
  #	if (est.elast == 1) Synth.PUMAs.all.w <- parLapply(BootCluster, 1:B, function(PUMAs) SynthTE(y_hyp.w, bootstrap.PUMAs.inspace[[b]], treated.periods))
  B = length(AllCombinations.ContigPUMAs)
  
  if (is.null(weights.pre.placebo)==TRUE) Synth.PUMAs.all <- lapply( 1:B, function(b) SynthTE(df[is.element(region_puma_id0,PUMAs.Seattle)==FALSE, ], AllCombinations.ContigPUMAs[[b]], treated.periods, aggr.method, standardize))
  if (is.null(weights.pre.placebo)==FALSE) {
    Synth.PUMAs.all <- lapply( 1:B, function(b) 
      SynthTE(df[is.element(region_puma_id0,PUMAs.Seattle)==FALSE, ], 
              AllCombinations.ContigPUMAs[[b]], treated.periods, aggr.method, standardize, 
              weights.pre = as.numeric(weights.pre.placebo$weights[weights.pre.placebo$combinationNo==b])))
  }			
  if (est.elast == 1) Synth.PUMAs.all.w <- lapply( 1:B, function(b) SynthTE(df_w[is.element(region_puma_id0,PUMAs.Seattle)==FALSE, ], AllCombinations.ContigPUMAs[[b]], treated.periods, aggr.method, standardize))
  #stopCluster(BootCluster)
  #	time.inspaceSE.parlapply <- (proc.time() - start.time) / 60 
  
  # Get s.e. and CI using random sample of these estimates
  bootstrap.TE.inspace <- sapply(1:B, function(b) Synth.PUMAs.all[[b]]$TE)
  bootstrap.MSE.inspace <- sapply(1:B, function(b) Synth.PUMAs.all[[b]]$RMSE^2)
  bootstrap.resid.inspace <- sapply(1:B, function(b) Synth.PUMAs.all[[b]]$resid)
  if (est.elast == 1) bootstrap.TE.w.inspace <- sapply(1:B, function(b) Synth.PUMAs.all.w[[b]]$TE)
  
  # Cumulative effects (for CI)
  if (substr(j,1,1)=="d") {
    coef.cum <- results.SynthTE$TE
    coef.cum[5:8] <- (1 + results.SynthTE$TE[1:4]) * (1 + results.SynthTE$TE[5:8]) - 1
    coef.cum[9] <- (1 + results.SynthTE$TE[1]) * (1 + results.SynthTE$TE[5]) * (1 + results.SynthTE$TE[9]) - 1
  }
  if (substr(j,1,1)!="d") coef.cum <- results.SynthTE$TEcum 
  if (est.elast == 1) {
    coef.cum.w <- results.SynthTE.w$TE
    coef.cum.w[5:8] <- (1 + results.SynthTE.w$TE[1:4]) * (1 + results.SynthTE.w$TE[5:8]) - 1
    coef.cum.w[9] <- (1 + results.SynthTE.w$TE[1]) * (1 + results.SynthTE.w$TE[5]) * (1 + results.SynthTE.w$TE[9]) - 1
    elast <- coef.cum / coef.cum.w	
  }
  
  if (substr(j,1,1)=="d") {
    bootstrap.TEcum.inspace <- bootstrap.TE.inspace
    for (b in 1:B) { 	
      bootstrap.TEcum.inspace[5:8,b] <- (1 + bootstrap.TE.inspace[1:4,b]) * (1 + bootstrap.TE.inspace[5:8,b]) - 1 
      bootstrap.TEcum.inspace[9,b] <- (1 + bootstrap.TE.inspace[1,b]) * (1 + bootstrap.TE.inspace[5,b]) * (1 + bootstrap.TE.inspace[9,b]) - 1 
    }
    if (est.elast == 1) {
      bootstrap.TEcum.w.inspace <- bootstrap.TE.w.inspace
      for (b in 1:B) { 	
        bootstrap.TEcum.w.inspace[5:8,b] <- (1 + bootstrap.TE.w.inspace[1:4,b]) * (1 + bootstrap.TE.w.inspace[5:8,b]) - 1 
        bootstrap.TEcum.w.inspace[9,b] <- (1 + bootstrap.TE.w.inspace[1,b]) * (1 + bootstrap.TE.w.inspace[5,b]) * (1 + bootstrap.TE.w.inspace[9,b]) - 1 
      }
    }
  }
  if (substr(j,1,1)!="d") bootstrap.TEcum.inspace <- sapply(1:B, function(b) Synth.PUMAs.all[[b]]$TEcum)
  
  # P-value for coefficients 
  p_val.inspace <- c()
  p_val.rmspe.inspace <- c()
  p_val.cum.inspace <- c()
  p_val.cum.w.inspace <- c()
  
  for (t in 1:9) {
    p_val.inspace[[t]] <- mean(abs(bootstrap.TE.inspace[t,]) > abs(results.SynthTE$TE[t]), na.rm = TRUE)
    p_val.rmspe.inspace[[t]] <- mean(bootstrap.TE.inspace[t,]^2/bootstrap.MSE.inspace > (results.SynthTE$TE[t]^2)/(results.SynthTE$RMSE^2), na.rm = TRUE)
    p_val.cum.inspace[[t]] <- mean(abs(bootstrap.TEcum.inspace[t,]) > abs(coef.cum[t]), na.rm = TRUE)
    if(est.elast==1) p_val.cum.w.inspace[[t]] <- mean(abs(bootstrap.TEcum.w.inspace[t,]) > abs(coef.cum.w[t]), na.rm = TRUE)
  }
  
  # Confidence set for coefficients 	
  ranksCI <- length(AllCombinations.ContigPUMAs) - floor((1-CIlevels)*length(AllCombinations.ContigPUMAs)) - 1
  TEcum_ME <- sapply(1:9, function(t) sort(abs(bootstrap.TEcum.inspace[t,]))[ranksCI])
  TE_ME <- sapply(1:9, function(t) sort(abs(bootstrap.TE.inspace[t,]))[ranksCI])
  CIcum.lower <- as.matrix(rep(1,length(CIlevels)))%*%t(as.matrix(coef.cum)) - TEcum_ME
  CIcum.upper <- as.matrix(rep(1,length(CIlevels)))%*%t(as.matrix(coef.cum)) + TEcum_ME
  CI.lower <- as.matrix(rep(1,length(CIlevels)))%*%t(as.matrix(results.SynthTE$TE)) - TE_ME 
  CI.upper <- as.matrix(rep(1,length(CIlevels)))%*%t(as.matrix(results.SynthTE$TE)) + TE_ME
  df.CI <- as.data.frame(cbind(t(CIcum.lower),t(CIcum.upper),t(CI.lower),t(CI.upper)))
  names(df.CI) <- c(paste0("CIcum_lower",100*CIlevels),paste0("CIcum_upper",100*CIlevels),paste0("CI_lower",100*CIlevels),paste0("CI_upper",100*CIlevels))
  
  
  # # Confidence set for elasticity
  if (est.elast == 1) {
    # 95% Confidence set for wages
    TEcum_ME.w <- sapply(1:9, function(t) sort(abs(bootstrap.TEcum.w.inspace[t,]))[ranksCI])
    CIcum.lower.w <- as.matrix(rep(1,length(CIlevels)))%*%t(as.matrix(coef.cum.w)) - TEcum_ME.w
    CIcum.upper.w <- as.matrix(rep(1,length(CIlevels)))%*%t(as.matrix(coef.cum.w)) + TEcum_ME.w
    
    # # Draw M numbers from the possible set 
    wcum.star <- list()
    ecum.star <- list()
    ew.integral <- list()
    elast.set <- list()
    elast.lower <- list()
    elast.upper <- list()
    pmax.integral <- list()
    s.integral <- list()
    
    M = 1e4
    start.time <- proc.time()
   # BootCluster <- makeCluster(n_cores)
    for (i in 1:length(CIlevels)) {
      
      wcum.star[[i]] <- sapply(1:9, function(t) as.matrix(runif(M, min = CIcum.lower.w[i,t], max = CIcum.upper.w[i,t])))
      ecum.star[[i]] <- sapply(1:9, function(t) as.matrix(runif(M, min = CIcum.lower[i,t], max = CIcum.upper[i,t])))
      # Check each pair -- share of PUMA combinations which cannot reject the effect of this size in each variable
      ew.integral[[i]] <- sapply(1:M, function(m) sapply(1:t, function(t) mean(abs(bootstrap.TEcum.inspace[t,]) > abs(coef.cum[t] - ecum.star[[i]][m,t]) & 
                                                                                                 abs(bootstrap.TEcum.w.inspace[t,]) > abs(coef.cum.w[t] - wcum.star[[i]][m,t]), na.rm = TRUE)))
      ew.integral[[i]] <- t(ew.integral[[i]])
      # What pairs of w and e are acceptable?
      elast.set[[i]] <- lapply(1:9, function(t) ecum.star[[i]][,t][ew.integral[[i]][,t]<CIlevels[[i]]]/wcum.star[[i]][,t][ew.integral[[i]][,t]<CIlevels[[i]]])
      elast.lower[[i]] <- sapply(1:9, function(t) min(elast.set[[i]][[t]]))
      elast.upper[[i]] <- sapply(1:9, function(t) max(elast.set[[i]][[t]]))		
      pmax.integral[[i]] <- sapply(1:9, function(t) max(ew.integral[[i]][,t][ew.integral[[i]][,t]<CIlevels[[i]]]))
      s.integral[[i]] <- sapply(1:9, function(t) mean(ew.integral[[i]][,t]<CIlevels[[i]]))		
    }
  #  stopCluster(BootCluster)
    time.elast.CI <- (proc.time() - start.time)/60
    
    df.CIelast <- as.data.frame(cbind(do.call("cbind",elast.lower),do.call("cbind",elast.upper),do.call("cbind",pmax.integral),do.call("cbind",s.integral)))
    names(df.CIelast) <- c(paste0("CIelast_lower",100*CIlevels),paste0("CIelast_upper",100*CIlevels),paste0("elast_p_max",100*CIlevels),paste0("elast_s_used",100*CIlevels))
  }
  
  ### Return results ###
  
  # 1 -- data.frame with coefficients
  results <- data.frame(
    variable = j,
    method = method.label,
    Obs = nrow(df),
    RMSE = results.SynthTE$RMSE,		
    T = 1:9,
    coef = results.SynthTE$TE,
    coefcum = coef.cum,
    coefcum.abs = results.SynthTE$TEabs,
    pval = p_val.inspace,
    pval_rmspe = p_val.rmspe.inspace,
    pval.cum = p_val.cum.inspace)
  if (length(CIlevels)==1) {
    results$CIcum_lower = as.numeric(CIcum.lower)
    results$CIcum_upper = as.numeric(CIcum.upper)
    results$CI_lower = as.numeric(CI.lower)
    results$CI_upper = as.numeric(CI.upper)
  }
  if (length(CIlevels) > 1) {
    results <- cbind(results, df.CI)
  }
  if (est.elast == 0 & length(CIlevels)==1) {
    results$coefw = NA
    results$coefwcum = NA
    results$pval_cumw = NA
    results$elast = NA
    results$CIelast_lower = NA
    results$CIelast_upper = NA
    results$pmax_elast = NA
    results$s_elast = NA
  }
  if (est.elast == 0 & length(CIlevels)>1) {
    results$coefw = NA
    results$coefwcum = NA
    results$pval_cumw = NA
    results$elast = NA
    results$CIelast_lower50 = NA
    results$CIelast_lower90 = NA
    results$CIelast_lower95 = NA
    results$CIelast_upper50 = NA
    results$CIelast_upper90 = NA
    results$CIelast_upper95 = NA
    results$elast_p_max50 = NA
    results$elast_p_max90 = NA
    results$elast_p_max95 = NA
    results$elast_s_used90 = NA
    results$elast_s_used50 = NA
    results$elast_s_used95 = NA
  }		
  if (est.elast == 1) {
    results$coefw = results.SynthTE.w$TE			
    results$coefwcum = coef.cum.w
    results$pval_cumw = p_val.cum.w.inspace
    results$elast = elast
    results <- cbind(results, df.CIelast)
  }  		
  
  # 4 - weights data.frame with weights for Seattle
  weights <- data.frame(variable = j, 
                        region_puma_id0 = results.SynthTE$weights[,1],
                        weights = results.SynthTE$weights[,2])
  
  # 4 - weights data.frame with weights for Seattle
  bootstrap.Weights <- do.call("rbind", lapply(1:B, function(b) data.frame(variable = j, 
                                                                           region_puma_id0 = Synth.PUMAs.all[[b]]$weights[,1],
                                                                           weights = Synth.PUMAs.all[[b]]$weights[,2],
                                                                           combinationNo = b)))
  
  # Return everything as a list	
  return(list(coeff = results, weights = weights,
              bootstrapPUMAs = AllCombinations.ContigPUMAs, 
              bootstrapTE = bootstrap.TE.inspace, bootstrapTEcum = bootstrap.TEcum.inspace, 
              bootstrapResid = bootstrap.resid.inspace,
              bootstrapWeights = bootstrap.Weights))
}		

############################################
###                                      ###
### Interactive Fixed effects            ###
###                                      ###
############################################

########################################################################
### Programs to calculate coefficients from Gobillon, Magnac (2016) ###
########################################################################

# Step for parameters of explanatory variables

coefX <- function(yp,Xp,iXpXp,betaL,betaF) {
  betaX <- iXpXp %*% crossprod(Xp,yp-matrix(betaF %*% t(betaL),nrow(yp),1))
  betaX
}

# Step for constant, individual and time additive effects
# where there is no explanatory variable

coefCit0 <- function(ym,yi,yt) {
  coefC <- as.numeric(ym)
  coefi <- yi - coefC
  coeft <- yt - coefC
  coeff <- list(coefC,coefi,coeft)
  names(coeff) <- c("coefC","coefi","coeft")
  coeff
}

# Step for time interactive effects
# where there is no explanatory variable

coefF0 <- function(yp,nbf) {
  ww <- matrix(0,T,T)
  for (i in 1:(nrow(yp)/T)) {
    wi <- yp[((i-1)*T+1):(i*T)]
    ww <- ww+crossprod(t(wi))
  }
  oww <- order(eigen(ww)$values,decreasing=TRUE)
  oww <- oww[1:nbf]
  betaF <- matrix(sqrt(T)*eigen(ww)$vectors[,t(oww)],T,nbf)
  if (betaF[1]<0) {betaF=-betaF}
  betaF
}

# Step for individual interactive effects
# where there is no explanatory variable

coefL0 <- function(yp,betaF) {
  betaL <- matrix(0,N,ncol(betaF))
  for (i in 1:N) {
    betaL[i,] <- crossprod(yp[((i-1)*T+1):(i*T)],betaF)/T
  }
  betaL
}

# Step for constant, individual and time additive effects

coefCit <- function(ym,Xm,yi,Xi,yt,Xt,betaX) {
  coefC <- as.numeric(ym - Xm%*%betaX)
  coefi <- yi - Xi%*%betaX - coefC
  coeft <- yt - Xt%*%betaX - coefC
  coeff <- list(coefC,coefi,coeft)
  names(coeff) <- c("coefC","coefi","coeft")
  coeff
}

# Step for time interactive effects

coefF <- function(yp,Xp,betaX,nbf,N,T) {
  #		N <- nrow(yp)/T
  ww <- matrix(0,T,T)
  for (i in 1:N) {
    wi <- yp[((i-1)*T+1):(i*T)]-Xp[((i-1)*T+1):(i*T),]%*%betaX
    ww <- ww+crossprod(t(wi))
  }
  oww <- order(eigen(ww)$values,decreasing=TRUE)
  oww <- oww[1:nbf]
  betaF <- matrix(sqrt(T)*eigen(ww)$vectors[,t(oww)],T,nbf)
  if (betaF[1]<0) {betaF=-betaF}
  return(list(betaF = betaF, eigenv = eigen(ww)$values[oww]))
}

# Step for individual interactive effects

coefL <- function(yp,Xp,betaX,betaF,N,T) {
  #		N <- nrow(yp)/T
  betaL <- matrix(0,N,ncol(betaF))
  for (i in 1:N) {
    betaL[i,] <- crossprod(yp[((i-1)*T+1):(i*T)]-
                             Xp[((i-1)*T+1):(i*T),]%*%betaX,betaF)/T
  }
  betaL
}	

# Inte. FE coef

coefsbai <- function(y,X,nbf,initL,initF,iter,expl) {
  
  N <- nrow(initL)
  T <- nrow(initF)
  
  numi <- seq(1:N) %x% rep(1,T)
  numt <- rep(1,N) %x% seq(1:T)
  ym   <- apply(y,2,mean)
  yi   <- as.matrix(tapply(y,numi,mean),N,1)
  yt   <- as.matrix(tapply(y,numt,mean),T,1)
  yp   <- y-as.matrix(yi %x% rep(1,T))-as.matrix(rep(1,N) %x% yt)+ym
  
  if (expl==1) {
    Xm   <- t(apply(X,2,mean))
    Xi   <- matrix(0,N,ncol(X))
    for (k in 1:ncol(X)) { Xi[,k] <- tapply(X[,k],numi,mean) }
    Xt   <- matrix(0,T,ncol(X))
    for (k in 1:ncol(X)) { Xt[,k] <- tapply(X[,k],numt,mean) }
    Xp   <- X-Xi%x%rep(1,T)-rep(1,N)%x%Xt+Xm%x%rep(1,N*T)
    iXpXp <- solve(crossprod(Xp))
  }
  
  if (expl==0) {
    cf     <- coefCit0(ym,yi,yt)
    coefF  <- coefF0(yp,nbf)
    coefL  <- coefL0(yp,coefF)
  }
  
  #		browser()
  
  if (expl==1) {
    for (i in 1:iter) {
      
      if (i==1) { coefX <- coefX(yp,Xp,iXpXp,initL,initF) }
      else	  { coefX <- coefX(yp,Xp,iXpXp,coefL,coefF) }
      
      cf     <- coefCit(ym,Xm,yi,Xi,yt,Xt,coefX)
      coefF.full  <- coefF(yp,Xp,coefX,nbf,N,T)
      coefF  <- coefF.full$betaF 
      eigenF  <- coefF.full$eigenv
      coefL  <- coefL(yp,Xp,coefX,coefF,N,T)
      
      cf_all1 <- as.numeric(coefX)				
      if (i > 1) {
        cf_dist <- sum((cf_all0 - cf_all1)^2)
      }
      cf_all0 <- cf_all1
    }
  }
  
  if (expl==0) {
    coeff <- list(cf$coefC,cf$coefi,cf$coeft,coefF,coefL)
    names(coeff) <- c("coefC","coefi","coeft","coefF","coefL")
  }
  
  if (expl==1) {
    coeff <- list(coefX,cf$coefC,cf$coefi,cf$coeft,coefF,coefL,cf_dist,eigenF)
    names(coeff) <- c("coefX","coefC","coefi","coeft","coefF","coefL","cfdist","eigenF")
  }
  coeff
}

# Estimation of standard errors for Bai (2009) #
################################################

stdbai_iid <- function(y,X,coefX,coefC,coefi,coeft,coefL,coefF) {
  
  N <- nrow(coefL)
  T <- nrow(coefF)		
  
  # Quantities computed once and for all
  
  numi <- seq(1:N) %x% rep(1,T)
  numt <- rep(1,N) %x% seq(1:T)
  ym   <- apply(y,2,mean)
  Xm   <- t(apply(X,2,mean))
  ymi  <- as.matrix(tapply(y,numi,mean),N,1)
  Xmi  <- matrix(0,N,ncol(X))
  for (k in 1:ncol(X)) { Xmi[,k] <- tapply(X[,k],numi,mean) }
  ymt  <- as.matrix(tapply(y,numt,mean),T,1)
  Xmt  <- matrix(0,T,ncol(X))
  for (k in 1:ncol(X)) { Xmt[,k] <- tapply(X[,k],numt,mean) }
  
  yp <- y-as.matrix(ymi %x% rep(1,T))-as.matrix(rep(1,N) %x% ymt)+ym
  Xp <- X-Xmi%x%rep(1,T)-rep(1,N)%x%Xmt+Xm%x%rep(1,N*T)
  
  MF <- diag(T)-coefF%*%solve(crossprod(coefF))%*%t(coefF)
  
  llm1 <- solve(crossprod(coefL)/N)
  
  eps  <- matrix(0,N*T,1)
  som  <- 0
  
  for (i in 1:N) {
    somi <- 0
    Xi  <- Xp[((i-1)*T+1):(i*T),]
    li  <- matrix(coefL[i,],ncol(coefL),1)
    
    eps[((i-1)*T+1):(i*T),1] <- y[((i-1)*T+1):(i*T),1]-X[((i-1)*T+1):(i*T),]%*%coefX-
      coefF%*%li-coefC-coefi[i,1]-coeft
    
    for (k in 1:N) {
      Xk  <- Xp[((k-1)*T+1):(k*T),]
      lk <- matrix(coefL[k,],ncol(coefL),1)	
      aik <- as.numeric(t(li)%*%llm1%*%lk)
      somi <- somi + aik*MF%*%Xk/N
    }
    zi <- MF%*%Xi-somi
    som <- som + crossprod(zi)/(N*T)
  }
  sig2 <- as.numeric(crossprod(eps)/(N*T))
  
  Vcovbai <- as.matrix(sig2*solve(som))/(N*T)
  return(list(Vcov = Vcovbai, se = matrix(sqrt(diag(Vcovbai)),ncol(som),1)))
}

EstimateIFE <- function(j,y,X,yw = NULL, Xw = NULL,est.elast,method.label) {
  
  # Note: j indexes variables
  # K indexes number of factors used in Bai (2009) estimation procedure
  
  # Make sure that y and X are ordered correctly
  y <- y[order(region_puma_id0, yearquarter), ]			
  X <- X[order(region_puma_id0, yearquarter), ]	
  
  # Employment variable? Then estimate wage effect as well
  if (est.elast == 1) {
    yw <- yw[order(region_puma_id0, yearquarter), ]			
    Xw <- Xw[order(region_puma_id0, yearquarter), ]	
  }	        
  
  # Initialize matrices for factor loadings and factors
  initL <- matrix(0,length(unique(y[,region_puma_id0])),K)
  initF <- matrix(0,nrow(unique(y[,list(yearquarter)])),K)
  for (k in 1:K) {
    initF[k,k] <- sqrt(nrow(unique(y[,list(yearquarter)])))
  }
  
  # Estimate parameters and s.e.
  results.bai  	<- coefsbai(as.matrix(y[,dY]),
                            as.matrix(X[,list(treated1,treated2,treated3,treated4,treated5,treated6,treated7,treated8,treated9)]),
                            K,initL,initF,20,1)
  results.bai.se	<- stdbai_iid(as.matrix(y[,dY]),
                               as.matrix(X[,list(treated1,treated2,treated3,treated4,treated5,treated6,treated7,treated8,treated9)]),
                               results.bai$coefX,results.bai$coefC,results.bai$coefi,
                               results.bai$coeft,results.bai$coefL,results.bai$coefF)	
  if (est.elast == 1) {
    results.bai.w  	<- coefsbai(as.matrix(yw[,dY]),
                                as.matrix(Xw[,list(treated1,treated2,treated3,treated4,treated5,treated6,treated7,treated8,treated9)]),
                                K,initL,initF,20,1)
    results.bai.w.se	<- stdbai_iid(as.matrix(yw[,dY]),
                                   as.matrix(Xw[,list(treated1,treated2,treated3,treated4,treated5,treated6,treated7,treated8,treated9)]),
                                   results.bai.w$coefX,results.bai.w$coefC,results.bai.w$coefi,
                                   results.bai.w$coeft,results.bai.w$coefL,results.bai.w$coefF)	
  }  
  
  # Predicted y -- all the regions
  N <- dim(initL)[[1]]
  T <- dim(initF)[[1]]			
  y$dYhat  <- results.bai$coefC + 
    rep(results.bai$coefi, each = T) +
    rep(results.bai$coeft, times = N) + 
    matrix(results.bai$coefF %*% t(results.bai$coefL), nrow = N*T, ncol = 1)
  residuals <- y$dY - y$dYhat
  MSE.allt <- sum((residuals)^2)
  R2 <- 1 - sd(residuals)^2 / sd(y$dY)^2
  IC1 <- log(MSE.allt / (N*T)) + K * ((N+T)/(N*T)) * log((N*T / (N+T)))
  IC2 <- log(MSE.allt / (N*T)) + K * ((N+T)/(N*T)) * log(min(N,T))
  # Convergence criterion
  dist.iter <- results.bai$cfdist
  
  # Reshape factors
  df.coefF <- melt(results.bai$coefF)
  setnames(df.coefF,c("Var1","Var2","value"),c("Time","factorID","factor"))
  df.coefF$yearquarter <-  unique(y[,list(yearquarter)])[df.coefF$Time][,yearquarter]
  df.coefF$Time <- NULL
  
  df.coeft <- melt(results.bai$coeft)
  setnames(df.coeft,c("Var1","value"),c("Time","cohortFE"))
  df.coeft$yearquarter <-  unique(y[,yearquarter])[df.coeft$Time]
  df.coeft$Time <- NULL
  df.coeft$Var2 <- NULL	
  
  df.coefFt <- merge(df.coefF, df.coeft, by = "yearquarter")
  df.coefFt$variable <- j
  df.coefFt$N_factors <- K	
  df.coefFt$constant <- results.bai$coefC		
  
  df.coefFt$variable = j
  df.coefFt$method = method.label
  
  # Reshape factor loadings
  df.coefL <- melt(results.bai$coefL)
  setnames(df.coefL,c("Var1","Var2","value"),c("region","factorID","loadings"))
  df.coefL$region_puma_id0 <-  unique(y[,region_puma_id0])[df.coefL$region]
  df.coefL$region <- NULL
  
  df.coefi <- melt(results.bai$coefi)
  setnames(df.coefi,c("Var1","value"),c("region","regionFE"))
  df.coefi$region_puma_id0 <-  unique(y[,region_puma_id0])[df.coefi$region]
  df.coefi$region <- NULL
  df.coefi$Var2 <- NULL	
  
  df.coefLi <- merge(df.coefL, df.coefi, by = "region_puma_id0")
  df.coefLi$variable <- j
  df.coefLi$N_factors <- K	
  df.coefLi$constant <- results.bai$coefC
  
  df.coefi$variable = j
  df.coefi$method = method.label
  
  # 1 -- coefficients	    
  results <- data.frame(
    variable = j,
    method = method.label, 
    T = 1:9,
    coef = results.bai$coefX,
    se = sqrt(diag(results.bai.se$Vcov)),
    RMSE = sqrt(mean(residuals^2)),
    IC1 = IC1,
    IC2 = IC2,
    mineigenv = min(results.bai$eigenF),
    dist = dist.iter,
    N_factors = K,
    Obs = N*T,
    R2 = R2)
  if (est.elast == 0) results$coef.w = as.numeric(NA)  			
  if (est.elast == 1) results$coef.w = as.numeric(results.bai.w$coefX)			
  
  # 2 -- data.frame with main Vcov matrix
  if (est.elast == 1) Vcov1 <- cbind(results.bai.se$Vcov,results.bai.w.se$Vcov,array(0,c(9,9)))
  if (est.elast == 0) Vcov1 <- cbind(results.bai.se$Vcov,array(NA,c(9,9)),array(NA,c(9,9)))
  Vcov1 <- data.frame(Vcov1)
  names(Vcov1) <- c(paste0("VcovE",1:9),paste0("VcovW",1:9),paste0("VcovEw",1:9))
  Vcov1$variable = j
  Vcov1$method = method.label
  
  return(list(coeff = results, Vcov = Vcov1, weights = df.coefLi, factors = df.coefFt))	
  
}		

########################
###                  ###
### Additional coefs ###
###                  ###
########################

# Calculate cumulative coefficients (change since baseline = Q2 2014)
# and standard errors using delta method
# + Calculate elasticity and standard errors.
# if variable is an employment variable

CumulCoefs <- function(df.coef, VcovE, VcovW, VcovEw, est.elast) {
  
  # Cumulative coefficients
  coef.all <- df.coef
  coef.all$se <- sqrt(diag(VcovE))
  
  coef.all$coefcum <- coef.all$coef
  coef.all$coefcum[1:4] <- coef.all$coef[1:4]
  coef.all$coefcum[5:8] <- coef.all$coef[5:8] + coef.all$coef[1:4] + coef.all$coef[5:8] * coef.all$coef[1:4]
  coef.all$coefcum[9] <- coef.all$coef[9] + coef.all$coef[5] + coef.all$coef[1] +
    coef.all$coef[9] * coef.all$coef[5] + coef.all$coef[1] * coef.all$coef[9] +
    coef.all$coef[9] * coef.all$coef[5] + 
    coef.all$coef[1] * coef.all$coef[9] * coef.all$coef[5]
  
  # S.e. for cumulative coeffifients using delta method					
  coef.all$secum <- coef.all$coef
  coef.all$secum[1:4] <- sqrt(diag(VcovE[1:4,1:4]))									  
  coef.all$secum[5:8] <- sqrt((1 + coef.all$coef[5:8])^2 * diag(VcovE[1:4,1:4]) +
                                (1 + coef.all$coef[1:4])^2 * diag(VcovE[5:8,5:8]) +
                                2 * (1 + coef.all$coef[5:8]) * (1 + coef.all$coef[1:4]) * diag(VcovE[1:4,5:8]))									  
  # Gradient
  g <- c()
  g[1] = 1 + coef.all$coef[5] + coef.all$coef[9] + coef.all$coef[5] * coef.all$coef[9]
  g[2] = 1 + coef.all$coef[1] + coef.all$coef[9] + coef.all$coef[1] * coef.all$coef[9]
  g[3] = 1 + coef.all$coef[1] + coef.all$coef[5] + coef.all$coef[1] * coef.all$coef[5]
  g = as.matrix(g)
  
  coef.all$secum[9] <- sqrt(diag(t(g) %*% as.matrix(VcovE[c(1,5,9),c(1,5,9)]) %*% g))
  
  if (est.elast == 1) {
    # Cumulutive coefficients for wage rates (if variable measure employment)
    coef.all$se.w <- sqrt(diag(VcovW))
    
    coef.all$coefwcum <- coef.all$coef.w
    coef.all$coefwcum[1:4] <- coef.all$coef.w[1:4]
    coef.all$coefwcum[5:8] <- coef.all$coef.w[5:8] + coef.all$coef.w[1:4] + coef.all$coef.w[5:8] * coef.all$coef.w[1:4]
    coef.all$coefwcum[9] <- coef.all$coef.w[9] + coef.all$coef.w[5] + coef.all$coef.w[1] +
      coef.all$coef.w[9] * coef.all$coef.w[5] + coef.all$coef.w[1] * coef.all$coef.w[9] +
      coef.all$coef.w[9] * coef.all$coef.w[5] + 
      coef.all$coef.w[1] * coef.all$coef.w[9] * coef.all$coef.w[5]
    
    # S.e. for wage rates (if variable measure employment)
    coef.all$sewcum <- coef.all$coef
    coef.all$sewcum[1:4] <- sqrt(diag(VcovW[1:4,1:4]))									  
    coef.all$sewcum[5:8] <- sqrt((1 + coef.all$coef.w[5:8])^2 * diag(VcovW[1:4,1:4]) +
                                   (1 + coef.all$coef.w[1:4])^2 * diag(VcovW[5:8,5:8]) +
                                   2 * (1 + coef.all$coef.w[5:8]) * (1 + coef.all$coef.w[1:4]) * diag(VcovW[1:4,5:8]))									  
    # Gradient
    g <- c()
    g[1] = 1 + coef.all$coef.w[5] + coef.all$coef.w[9] + coef.all$coef.w[5] * coef.all$coef.w[9]
    g[2] = 1 + coef.all$coef.w[1] + coef.all$coef.w[9] + coef.all$coef.w[1] * coef.all$coef.w[9]
    g[3] = 1 + coef.all$coef.w[1] + coef.all$coef.w[5] + coef.all$coef.w[1] * coef.all$coef.w[5]
    g = as.matrix(g)
    
    coef.all$sewcum[9] <- sqrt(diag(t(g) %*% as.matrix(VcovW[c(1,5,9),c(1,5,9)]) %*% g))
    
    # Elasticity of labor demand
    coef.all$elast <- coef.all$coefcum / coef.all$coefwcum
    
    # Standard errors for elasticity of labor demand
    # Cross-correlation term
    coef.all$Cov.Ew.cum <- coef.all$se
    coef.all$Cov.Ew.cum[1:4] <- diag(VcovEw[1:4,1:4])
    coef.all$Cov.Ew.cum[5:8] <- (1 + coef.all$coef[5:8]) * (1 + coef.all$coef.w[5:8]) * diag(VcovEw[1:4,1:4]) +
      (1 + coef.all$coef[1:4]) * (1 + coef.all$coef.w[1:4]) * diag(VcovEw[5:8,5:8])
    coef.all$Cov.Ew.cum[5:8] <- (1 + coef.all$coef[5:8]) * (1 + coef.all$coef.w[5:8]) * diag(VcovEw[1:4,1:4]) +
      (1 + coef.all$coef[1:4]) * (1 + coef.all$coef.w[1:4]) * diag(VcovEw[5:8,5:8])
    coef.all$Cov.Ew.cum[9] <- (1 + coef.all$coef[5] + coef.all$coef[9] + coef.all$coef[5]*coef.all$coef[9]) *
      (1 + coef.all$coef.w[5] + coef.all$coef.w[9] + coef.all$coef.w[5]*coef.all$coef.w[9]) * VcovEw[1,1] +
      (1 + coef.all$coef[1] + coef.all$coef[9] + coef.all$coef[1]*coef.all$coef[9]) *
      (1 + coef.all$coef.w[1] + coef.all$coef.w[9] + coef.all$coef.w[1]*coef.all$coef.w[9]) * VcovEw[5,5] +
      (1 + coef.all$coef[1] + coef.all$coef[5] + coef.all$coef[1]*coef.all$coef[5]) *
      (1 + coef.all$coef.w[1] + coef.all$coef.w[5] + coef.all$coef.w[1]*coef.all$coef.w[5]) * VcovEw[9,9]
    
    coef.all$se.elast <- sqrt((1/coef.all$coefwcum)^2 * coef.all$secum^2 +
                                (coef.all$coefcum)^2/(coef.all$coefwcum)^4 * coef.all$sewcum^2 - 
                                (coef.all$coefcum)/(coef.all$coefwcum)^3 * coef.all$Cov.Ew.cum)
    
    coef.all$Cov.Ew.cum = NULL
  }
  if (est.elast == 0) {
    coef.all$se.w = NA
    coef.all$coefwcum = NA
    coef.all$sewcum = NA
    coef.all$elast = NA
    coef.all$se.elast = NA
  }  
  
  # Return transformed coefficients and standard errors
  coef.all
}

############################################
###                                      ###
### Synthetic Control, Common Weights    ###
###                                      ###
############################################

# Procedure used to find synthetic control weights #
# (from Gobillon, Magnac 2016)                     #
####################################################

wipop <- function(Z0,Z1) {
  E <- matrix(1,1,ncol(Z0))
  F <- 1
  
  b <- 1
  
  G <- diag(ncol(Z0))
  H <- matrix(0,ncol(Z0),1)
  
  res <- lsei(A=Z0, B=Z1, E=E, F=F, G=G, H=H, type=2)
  w <- res$X
  
  return(w)
}
weightTE_common <- function(ac,  treated.PUMAS, treated.periods) {
  
  ### Control and treatment groups ###
  y0 <- ac[is.element(region_puma_id0,treated.PUMAS) == FALSE, ]
  y1 <- ac[is.element(region_puma_id0,treated.PUMAS) == TRUE, ]
  ### Standardize variables for weights  
  y1[is.element(yearquarter,treated.periods)==FALSE , preMW_mean_tmp := mean(dY) , by=c("variable")]
  y1[ , max_preMW_mean := max(preMW_mean_tmp, na.rm = TRUE), by=c("variable")]
  y1[ , preMW_mean := max_preMW_mean]
  y1[ , preMW_mean_tmp := NULL]
  y1[ , max_preMW_mean := NULL]
  y1[is.element(yearquarter,treated.periods)==FALSE , preMW_sd_tmp := sd(dY), by=c("variable")]
  y1[ , max_preMW_sd := max(preMW_sd_tmp, na.rm = TRUE), by=c("variable")]
  y1[ , preMW_sd := max_preMW_sd]
  y1[ , preMW_sd_tmp := NULL]
  y1[ , max_preMW_sd := NULL]
  y1[ , Y := dY]
  y1[ , dY := (dY - preMW_mean) / preMW_sd]
  
  y0[is.element(yearquarter,treated.periods)==FALSE , preMW_mean_tmp := mean(dY), by = c( "variable", "region_puma_id0")]
  y0[ , max_preMW_mean := max(preMW_mean_tmp, na.rm = TRUE), by = c("variable", "region_puma_id0")]
  y0[ , preMW_mean := max_preMW_mean, by = c("region_puma_id0")]
  y0[ , preMW_mean_tmp := NULL]
  y0[ , max_preMW_mean := NULL]
  y0[is.element(yearquarter,treated.periods)==FALSE , preMW_sd_tmp := sd(dY), by = c("variable","region_puma_id0")]
  y0[ , max_preMW_sd := max(preMW_sd_tmp, na.rm = TRUE), by = c("variable","region_puma_id0")]
  y0[ , preMW_sd := max_preMW_sd, by = c("variable","region_puma_id0")]
  y0[ , preMW_sd_tmp := NULL]
  y0[ , max_preMW_sd := NULL]		
  y0[ , Y := dY]
  y0[ , dY := (dY - preMW_mean) / preMW_sd]		
  #Take the average over the standardized treatment outcomes 
  y11 <- y1[ , sum(dY * N)/sum(N), by = c("yearquarter" )] 
  setnames(y11,"V1","dY")
  y11 <- y11[ , list(yearquarter, dY)]
  ### Reshape data for control units into matrices ###
  y00 <- melt(y0[ , list(region_puma_id0,yearquarter,dY)], id.vars = c("region_puma_id0","yearquarter"))
  y00 [, dY := sum (value)/4, by = c("yearquarter","region_puma_id0" )]
  y00 <- melt(y00[ , list(region_puma_id0,yearquarter,dY)], id.vars = c("region_puma_id0","yearquarter"))
  y00 <- subset(unique(y00))
  ##drop duplicates
  y00 <- dcast(y00, yearquarter ~ variable + region_puma_id0)
  
  # Make sure cohorts are ordered correctly
  y00 <- y00[order(yearquarter), ]
  y11 <- y11[order(yearquarter), ]	  
  y11 <- as.matrix(y11[is.element(yearquarter, treated.periods) == FALSE, "dY", with = F])
  y00<- as.matrix(y00[is.element(yearquarter, treated.periods) == FALSE, tail(names(y00),-1), with = F ])
  
  w00 <- try(wipop(y00, y11)  # treatment unit
  )
  
  return(w00)
  
}

SynthTE_common <- function(y, treated.PUMAS, treated.periods, aggr.method = "average", standardize = FALSE, weights.pre = NULL, ac ) {
  
  ### Get variable
  j = unique(y$variable)
  
  ### Control and treatment groups ###
  y0 <- y[is.element(region_puma_id0,treated.PUMAS) == FALSE, ]
  y1 <- y[is.element(region_puma_id0,treated.PUMAS) == TRUE, ]
  
  if (aggr.method == "average") {
    y11 <- y1[ , sum(dY * N)/sum(N), by = c("yearquarter","variable" )] 
    setnames(y11,"V1","dY")
    y11 <- y11[ , list(yearquarter, dY)]
  }
  if (aggr.method == "sum") {
    y11 <- y1[ , sum(dY), by = c("yearquarter","variable" )] 
    setnames(y11,"V1","dY")
    y11 <- y11[ , list(yearquarter, dY)]
  }
  if (aggr.method == "SumDividedByFive") {
    y11 <- y1[ , sum(dY)/5, by = c("yearquarter","variable" )] 
    setnames(y11,"V1","dY")
    y11 <- y11[ , list(yearquarter, dY)]
  }
  
  if (standardize) {
    # Standardize
    y11[is.element(yearquarter,treated.periods)==FALSE , preMW_mean_tmp := mean(dY)]
    y11[ , max_preMW_mean := max(preMW_mean_tmp, na.rm = TRUE)]
    y11[ , preMW_mean := max_preMW_mean]
    y11[ , preMW_mean_tmp := NULL]
    y11[ , max_preMW_mean := NULL]
    y11[is.element(yearquarter,treated.periods)==FALSE , preMW_sd_tmp := sd(dY)]
    y11[ , max_preMW_sd := max(preMW_sd_tmp, na.rm = TRUE)]
    y11[ , preMW_sd := max_preMW_sd]
    y11[ , preMW_sd_tmp := NULL]
    y11[ , max_preMW_sd := NULL]
    y11[ , Y := dY]
    y11[ , dY := (dY - preMW_mean) / preMW_sd]
    
    y0[is.element(yearquarter,treated.periods)==FALSE , preMW_mean_tmp := mean(dY), by = c("region_puma_id0")]
    y0[ , max_preMW_mean := max(preMW_mean_tmp, na.rm = TRUE), by = c("region_puma_id0")]
    y0[ , preMW_mean := max_preMW_mean, by = c("region_puma_id0")]
    y0[ , preMW_mean_tmp := NULL]
    y0[ , max_preMW_mean := NULL]
    y0[is.element(yearquarter,treated.periods)==FALSE , preMW_sd_tmp := sd(dY), by = c("region_puma_id0")]
    y0[ , max_preMW_sd := max(preMW_sd_tmp, na.rm = TRUE), by = c("region_puma_id0")]
    y0[ , preMW_sd := max_preMW_sd, by = c("region_puma_id0")]
    y0[ , preMW_sd_tmp := NULL]
    y0[ , max_preMW_sd := NULL]		
    y0[ , Y := dY]
    y0[ , dY := (dY - preMW_mean) / preMW_sd]		
  }		
  
  ### Reshape data for control units into matrices ###
  y00 <- melt(y0[ , list(region_puma_id0,yearquarter,dY)], id.vars = c("region_puma_id0","yearquarter"))
  y00 <- dcast(y00, yearquarter ~ variable + region_puma_id0)
  
  # Make sure cohorts are ordered correctly
  y00 <- y00[order(yearquarter), ]
  y11 <- y11[order(yearquarter), ]	  
  
  if (is.null(weights.pre)==FALSE) w00 <- as.numeric(weights.pre)
  
  ### Run weighting code to generate w00
  if (is.null(weights.pre)==TRUE) {
    w00 <- weightTE_common(ac , treated.PUMAS, treated.periods)
  }
  #HW: if SynthTE is treatment, if SynthTE is weights, do this
  if (class(w00) != "try-error") {
    
    ### Control PUMAS ###
    PUMAS00 <- as.integer(substr(tail(names(y00),-1),4,9))
    
    ### Treatment effect ###
    TE <- c()
    for (i in treated.periods) {
      TE <- c(TE,y11[yearquarter== i , "dY", with = F] - 
                sum(w00*y00[yearquarter== i, tail(names(y00),-1), with = F]))
    }
    
    ### Fitted values ###
    y11hat <- as.matrix(y00[is.element(yearquarter, treated.periods)==FALSE, tail(names(y00),-1), with = F]) %*% w00
    
    ### In-sample prediction (to calculate MSRE) ###
    resid <- as.numeric(y11[is.element(yearquarter,treated.periods)==FALSE, dY]) - y11hat  
    
    ### Abs values (if standardized) ###
    if (standardize) TEabs <- as.numeric(TE)*unique(as.numeric(y11$preMW_sd))
    if (standardize==FALSE) TEabs <- NA
    if (aggr.method=="SumDividedByFive") TE <- as.numeric(TE)*5
    
    ### Cumulative percent -- if est. in abs terms ###
    if (substr(j,1,1)!="d") { 
      Y0 <- tail(as.numeric(y11[is.element(yearquarter,treated.periods)==FALSE, dY]), 1)
      if (aggr.method=="SumDividedByFive") Y0 <- 5*Y0
      if (standardize==FALSE) TEcum <- as.numeric(TE)/Y0
      if (standardize==TRUE) TEcum <- as.numeric(TE)*unique(as.numeric(y11$preMW_sd))/(Y0*unique(as.numeric(y11$preMW_sd)) + unique(as.numeric(y11$preMW_mean)))
    }
    if (substr(j,1,1)=="d") TEcum <- NA
    
    ### Return treatment effect and weights for control units ###
    return(list(TE = as.numeric(TE),
                TEabs = TEabs,
                TEcum = TEcum,
                weights = as.matrix(cbind(PUMAS00,w00)),
                dY = data.frame(y11[is.element(yearquarter,treated.periods)==FALSE, list(yearquarter,dY)]),
                dYhat = y11hat,
                resid = resid,
                RMSE = sqrt(mean(resid^2))))
  }
  
  if (class(w00) == "try-error") {
    
    ### Control PUMAS ###
    PUMAS00 <- as.integer(substr(tail(names(y00),-1),4,9))
    
    ### Return treatment effect and weights for control units ###
    return(list(TE = rep(NA,9), 
                weights = as.matrix(cbind(PUMAS00,rep(NA,length(PUMAS00)))),
                dY = data.frame(y11[is.element(yearquarter, treated.periods)==FALSE,list(yearquarter,dY)], 
                                dYhat = NA,
                                resid = NA,
                                RMSE = NA)))
  }
}	

EstimateSynth_common <- function(j, ac, df,df_w,est.elast,method.label, PUMAs.Seattle , AllCombinations.ContigPUMAs, treated.periods,
                          aggr.method = "average", standardize = FALSE, CIlevels = 0.95, weights.pre.Seattle = NULL, weights.pre.placebo = NULL) {					
  
  # Estimate TE
  if (is.null(weights.pre.Seattle)==TRUE) results.SynthTE <- SynthTE_common(df, treated.PUMAS=PUMAs.Seattle, treated.periods, aggr.method, standardize, ac =ac)
  if (is.null(weights.pre.Seattle)==FALSE) results.SynthTE <- SynthTE_common(df, treated.PUMAS=PUMAs.Seattle, treated.periods, aggr.method, standardize, weights.pre = as.numeric(weights.pre.Seattle$weights), ac =ac)
  
  # Employment variable? Then estimate wage effect as well
  if (est.elast == 1) {
    results.SynthTE.w <- SynthTE_common(df_w, treated.PUMAS=PUMAs.Seattle, treated.periods, aggr.method, standardize, ac =ac )
  }
  # 
  # # ### Bootstrapped CI ###	
  # # # Note: all possible combinations of continuous PUMAs have been saved before
  # 
  B = length(AllCombinations.ContigPUMAs)
  
  if (is.null(weights.pre.placebo)==TRUE) Synth.PUMAs.all <-lapply( 1:B, function(b) SynthTE_common(df[is.element(region_puma_id0,PUMAs.Seattle)==FALSE, ], treated.PUMAS= AllCombinations.ContigPUMAs[[b]], treated.periods, aggr.method, standardize, ac=ac[is.element(region_puma_id0,PUMAs.Seattle)==FALSE, ]))
  if (is.null(weights.pre.placebo)==FALSE) {
    Synth.PUMAs.all <- lapply( 1:B, function(b)
      SynthTE_common(df[is.element(region_puma_id0,PUMAs.Seattle)==FALSE, ],
              treated.PUMAS= AllCombinations.ContigPUMAs[[b]], treated.periods, aggr.method, standardize,
              weights.pre = as.numeric(weights.pre.placebo$weights[weights.pre.placebo$combinationNo==b]), ac=ac[is.element(region_puma_id0,PUMAs.Seattle)==FALSE, ]))
  }
  if (est.elast == 1) Synth.PUMAs.all.w <- lapply(1:B, function(b) SynthTE_common(df_w[is.element(region_puma_id0,PUMAs.Seattle)==FALSE, ], treated.PUMAS=AllCombinations.ContigPUMAs[[b]], treated.periods, aggr.method, standardize, ac=ac[is.element(region_puma_id0,PUMAs.Seattle)==FALSE, ]))

  # Get s.e. and CI using random sample of these estimates
  bootstrap.TE.inspace <- sapply(1:B, function(b) Synth.PUMAs.all[[b]]$TE)
  bootstrap.MSE.inspace <- sapply(1:B, function(b) Synth.PUMAs.all[[b]]$RMSE^2)
  bootstrap.resid.inspace <- sapply(1:B, function(b) Synth.PUMAs.all[[b]]$resid)
  if (est.elast == 1) bootstrap.TE.w.inspace <- sapply(1:B, function(b) Synth.PUMAs.all.w[[b]]$TE)
  
  # Cumulative effects (for CI)
  if (substr(j,1,1)=="d") {
    coef.cum <- results.SynthTE$TE
    coef.cum[5:8] <- (1 + results.SynthTE$TE[1:4]) * (1 + results.SynthTE$TE[5:8]) - 1
    coef.cum[9] <- (1 + results.SynthTE$TE[1]) * (1 + results.SynthTE$TE[5]) * (1 + results.SynthTE$TE[9]) - 1
  }
  if (substr(j,1,1)!="d") coef.cum <- results.SynthTE$TEcum 
  if (est.elast == 1) {
    coef.cum.w <- results.SynthTE.w$TE
    coef.cum.w[5:8] <- (1 + results.SynthTE.w$TE[1:4]) * (1 + results.SynthTE.w$TE[5:8]) - 1
    coef.cum.w[9] <- (1 + results.SynthTE.w$TE[1]) * (1 + results.SynthTE.w$TE[5]) * (1 + results.SynthTE.w$TE[9]) - 1
    elast <- coef.cum / coef.cum.w	
  }
  
  if (substr(j,1,1)=="d") {
    bootstrap.TEcum.inspace <- bootstrap.TE.inspace
    for (b in 1:B) { 	
      bootstrap.TEcum.inspace[5:8,b] <- (1 + bootstrap.TE.inspace[1:4,b]) * (1 + bootstrap.TE.inspace[5:8,b]) - 1 
      bootstrap.TEcum.inspace[9,b] <- (1 + bootstrap.TE.inspace[1,b]) * (1 + bootstrap.TE.inspace[5,b]) * (1 + bootstrap.TE.inspace[9,b]) - 1 
    }
    if (est.elast == 1) {
      bootstrap.TEcum.w.inspace <- bootstrap.TE.w.inspace
      for (b in 1:B) { 	
        bootstrap.TEcum.w.inspace[5:8,b] <- (1 + bootstrap.TE.w.inspace[1:4,b]) * (1 + bootstrap.TE.w.inspace[5:8,b]) - 1 
        bootstrap.TEcum.w.inspace[9,b] <- (1 + bootstrap.TE.w.inspace[1,b]) * (1 + bootstrap.TE.w.inspace[5,b]) * (1 + bootstrap.TE.w.inspace[9,b]) - 1 
      }
    }
  }
  if (substr(j,1,1)!="d") bootstrap.TEcum.inspace <- sapply(1:B, function(b) Synth.PUMAs.all[[b]]$TEcum)
  
  # P-value for coefficients 
  p_val.inspace <- c()
  p_val.rmspe.inspace <- c()
  p_val.cum.inspace <- c()
  p_val.cum.w.inspace <- c()
  
  for (t in 1:9) {
    p_val.inspace[[t]] <- mean(abs(bootstrap.TE.inspace[t,]) > abs(results.SynthTE$TE[t]), na.rm = TRUE)
    p_val.rmspe.inspace[[t]] <- mean(bootstrap.TE.inspace[t,]^2/bootstrap.MSE.inspace > (results.SynthTE$TE[t]^2)/(results.SynthTE$RMSE^2), na.rm = TRUE)
    p_val.cum.inspace[[t]] <- mean(abs(bootstrap.TEcum.inspace[t,]) > abs(coef.cum[t]), na.rm = TRUE)
    if(est.elast==1) p_val.cum.w.inspace[[t]] <- mean(abs(bootstrap.TEcum.w.inspace[t,]) > abs(coef.cum.w[t]), na.rm = TRUE)
  }
  
  # Confidence set for coefficients 	
  ranksCI <- length(AllCombinations.ContigPUMAs) - floor((1-CIlevels)*length(AllCombinations.ContigPUMAs)) - 1
  TEcum_ME <- sapply(1:9, function(t) sort(abs(bootstrap.TEcum.inspace[t,]))[ranksCI])
  TE_ME <- sapply(1:9, function(t) sort(abs(bootstrap.TE.inspace[t,]))[ranksCI])
  CIcum.lower <- as.matrix(rep(1,length(CIlevels)))%*%t(as.matrix(coef.cum)) - TEcum_ME
  CIcum.upper <- as.matrix(rep(1,length(CIlevels)))%*%t(as.matrix(coef.cum)) + TEcum_ME
  CI.lower <- as.matrix(rep(1,length(CIlevels)))%*%t(as.matrix(results.SynthTE$TE)) - TE_ME 
  CI.upper <- as.matrix(rep(1,length(CIlevels)))%*%t(as.matrix(results.SynthTE$TE)) + TE_ME
  df.CI <- as.data.frame(cbind(t(CIcum.lower),t(CIcum.upper),t(CI.lower),t(CI.upper)))
  names(df.CI) <- c(paste0("CIcum_lower",100*CIlevels),paste0("CIcum_upper",100*CIlevels),paste0("CI_lower",100*CIlevels),paste0("CI_upper",100*CIlevels))
  
  
  # # Confidence set for elasticity
  if (est.elast == 1) {
    # 95% Confidence set for wages
    TEcum_ME.w <- sapply(1:9, function(t) sort(abs(bootstrap.TEcum.w.inspace[t,]))[ranksCI])
    CIcum.lower.w <- as.matrix(rep(1,length(CIlevels)))%*%t(as.matrix(coef.cum.w)) - TEcum_ME.w
    CIcum.upper.w <- as.matrix(rep(1,length(CIlevels)))%*%t(as.matrix(coef.cum.w)) + TEcum_ME.w
    
    # # Draw M numbers from the possible set 
    wcum.star <- list()
    ecum.star <- list()
    ew.integral <- list()
    elast.set <- list()
    elast.lower <- list()
    elast.upper <- list()
    pmax.integral <- list()
    s.integral <- list()
    
    M = 1e4 
    start.time <- proc.time()
    # BootCluster <- makeCluster(n_cores)
    for (i in 1:length(CIlevels)) {
      
      wcum.star[[i]] <- sapply(1:9, function(t) as.matrix(runif(M, min = CIcum.lower.w[i,t], max = CIcum.upper.w[i,t])))
      ecum.star[[i]] <- sapply(1:9, function(t) as.matrix(runif(M, min = CIcum.lower[i,t], max = CIcum.upper[i,t])))
      # Check each pair -- share of PUMA combinations which cannot reject the effect of this size in each variable
      ew.integral[[i]] <- sapply( 1:M, function(m) sapply(1:t, function(t) mean(abs(bootstrap.TEcum.inspace[t,]) > abs(coef.cum[t] - ecum.star[[i]][m,t]) & 
                                                                                                 abs(bootstrap.TEcum.w.inspace[t,]) > abs(coef.cum.w[t] - wcum.star[[i]][m,t]), na.rm = TRUE)))
      ew.integral[[i]] <- t(ew.integral[[i]])
      # What pairs of w and e are acceptable?
      elast.set[[i]] <- lapply(1:9, function(t) ecum.star[[i]][,t][ew.integral[[i]][,t]<CIlevels[[i]]]/wcum.star[[i]][,t][ew.integral[[i]][,t]<CIlevels[[i]]])
      elast.lower[[i]] <- sapply(1:9, function(t) min(elast.set[[i]][[t]]))
      elast.upper[[i]] <- sapply(1:9, function(t) max(elast.set[[i]][[t]]))		
      pmax.integral[[i]] <- sapply(1:9, function(t) max(ew.integral[[i]][,t][ew.integral[[i]][,t]<CIlevels[[i]]]))
      s.integral[[i]] <- sapply(1:9, function(t) mean(ew.integral[[i]][,t]<CIlevels[[i]]))		
    }
   # stopCluster(BootCluster)
    time.elast.CI <- (proc.time() - start.time)/60
    
    df.CIelast <- as.data.frame(cbind(do.call("cbind",elast.lower),do.call("cbind",elast.upper),do.call("cbind",pmax.integral),do.call("cbind",s.integral)))
    names(df.CIelast) <- c(paste0("CIelast_lower",100*CIlevels),paste0("CIelast_upper",100*CIlevels),paste0("elast_p_max",100*CIlevels),paste0("elast_s_used",100*CIlevels))
  }
  
  ### Return results ###
  
  # 1 -- data.frame with coefficients
  results <- data.frame(
    variable = j,
    method = method.label,
    Obs = nrow(df),
    RMSE = results.SynthTE$RMSE,		
    T = 1:9,
    coef = results.SynthTE$TE,
    coefcum = coef.cum,
    coefcum.abs = results.SynthTE$TEabs,
    pval = p_val.inspace,
    pval_rmspe = p_val.rmspe.inspace,
    pval.cum = p_val.cum.inspace)
  if (length(CIlevels)==1) {
    results$CIcum_lower = as.numeric(CIcum.lower)
    results$CIcum_upper = as.numeric(CIcum.upper)
    results$CI_lower = as.numeric(CI.lower)
    results$CI_upper = as.numeric(CI.upper)
  }
  if (length(CIlevels) > 1) {
    results <- cbind(results, df.CI)
  }
  if (est.elast == 0 & length(CIlevels)==1) {
    results$coefw = NA
    results$coefwcum = NA
    results$pval_cumw = NA
    results$elast = NA
    results$CIelast_lower = NA
    results$CIelast_upper = NA
    results$pmax_elast = NA
    results$s_elast = NA
  }
  if (est.elast == 0 & length(CIlevels)>1) {
    results$coefw = NA
    results$coefwcum = NA
    results$pval_cumw = NA
    results$elast = NA
    results$CIelast_lower50 = NA
    results$CIelast_lower90 = NA
    results$CIelast_lower95 = NA
    results$CIelast_upper50 = NA
    results$CIelast_upper90 = NA
    results$CIelast_upper95 = NA
    results$elast_p_max50 = NA
    results$elast_p_max90 = NA
    results$elast_p_max95 = NA
    results$elast_s_used90 = NA
    results$elast_s_used50 = NA
    results$elast_s_used95 = NA
  }		
  if (est.elast == 1) {
    results$coefw = results.SynthTE.w$TE			
    results$coefwcum = coef.cum.w
    results$pval_cumw = p_val.cum.w.inspace
    results$elast = elast
    results <- cbind(results, df.CIelast)
  }  		
  
  # 4 - weights data.frame with weights for Seattle
  weights <- data.frame(variable = j, 
                        region_puma_id0 = results.SynthTE$weights[,1],
                        weights = results.SynthTE$weights[,2])
  
  # 4 - weights data.frame with weights for Seattle
  bootstrap.Weights <- do.call("rbind", lapply(1:B, function(b) data.frame(variable = j, 
                                                                           region_puma_id0 = Synth.PUMAs.all[[b]]$weights[,1],
                                                                           weights = Synth.PUMAs.all[[b]]$weights[,2],
                                                                           combinationNo = b)))
  
  # Return everything as a list	
  return(list(coeff = results, weights = weights,
              bootstrapPUMAs = AllCombinations.ContigPUMAs, 
              bootstrapTE = bootstrap.TE.inspace, bootstrapTEcum = bootstrap.TEcum.inspace, 
              bootstrapResid = bootstrap.resid.inspace,
              bootstrapWeights = bootstrap.Weights))
}		

