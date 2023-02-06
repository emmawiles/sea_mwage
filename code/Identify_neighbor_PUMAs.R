
# Find neighboring PUMAs
# to estimate s.e. for SCM using continuous PUMAs

# Stub to load the data
stub <- "aggregate_establishments_cumulative_all"

#####################
###               ###
### Load the data ###
###               ###
#####################

# Load shapefile of PUMA boundaries
puma_boundaries <- readOGR(paste0(path.to.data,"tl_2010_53_puma10"), "tl_2010_53_puma10", stringsAsFactors = FALSE)
puma_boundaries$puma_id <- as.integer(puma_boundaries$PUMACE10) 

# Get the list with "region"-"puma_id" from the collapsed data
cohort.series <- read.dta13(paste0(path.to.data,stub,".dta", collapse = ""))
cohort.series <- data.table(cohort.series, key = c("region0","yearquarter"))
cohort.series[ , region_puma_id0 := region0 * 1e5 + puma_id0]	
region_puma <- unique(cohort.series[is.element(cohort.series$region0,c(1,4,5)), list(region_puma_id0,puma_id0)])
setnames(region_puma,"puma_id0","puma_id")

# Exclude Kind County outside of Seattle 
# (to match SCM analysis where it is also excluded)
PUMAs.King_minus_Seattle <- c(11606,11607,11608,11609,11610,11611,11612,11613,11614,11615,11616)
puma_boundaries <- puma_boundaries[is.element(puma_boundaries@data$puma_id,PUMAs.King_minus_Seattle)==FALSE, ]
puma_boundaries@data <- merge(puma_boundaries@data, region_puma, by = "puma_id", all.x = TRUE, all.y = FALSE) 

plot(puma_boundaries, col = "grey")

######################
### Find neighbors ###
######################

neighbors.list <- gTouches(puma_boundaries, byid = TRUE, returnDense = FALSE)
for (i in 1:length(neighbors.list)){puma_boundaries$id.cw[i] = i}

id.cw <- data.frame(id_i = puma_boundaries$id.cw, region_puma_id0 = puma_boundaries@data$region_puma_id0)

# Neighbors -- list 
pumas.neighbors.list <- lapply(1:length(neighbors.list), function(i) as.numeric(id.cw[is.element(id.cw$id_i, neighbors.list[[i]]), "region_puma_id0"]))
names(pumas.neighbors.list) <- id.cw$region_puma_id0

# Save neighbors as a matrix 

puma.ids <- sort(unique(unlist(pumas.neighbors.list)))

# Matrix of neighbors 
neighbors.matrix <- array(NA, c(length(puma.ids), length(puma.ids)))
for (i in 1:length(puma.ids)) {
	for (j in 1:length(puma.ids)) {
		neighbors.matrix[i,j] <- as.integer(is.element(puma.ids[[j]], pumas.neighbors.list[[i]]))
	}
}

row.names(neighbors.matrix) <- puma.ids
colnames(neighbors.matrix) <- puma.ids

pumas.Seattle <- c(111601,111602,111603,111604,111605)
neighbors.matrix.noSeattle <- neighbors.matrix[is.element(row.names(neighbors.matrix),pumas.Seattle)==FALSE, is.element(colnames(neighbors.matrix),pumas.Seattle)==FALSE]


####################################
### All possible combinations of ### 
### 5 continuous PUMAs          ###
### in WA outside of King Co     ###
####################################

PUMAs.all <- setdiff(puma.ids,pumas.Seattle)

	B = 1e6
	start.time <- proc.time()
	n_cores <- detectCores() 
	start.time <- proc.time()
	BootCluster <- makeCluster(n_cores)
	# Pick these TE randomly to get a bootstrap distribution
	# Sample 5 PUMAs and average over them
	# Make it a list, where each element contains a vector of five elements
	# Element No. 1 -- 1 PUMA out of 40 (w/o replacement)
	clusterExport(BootCluster, c("PUMAs.all","pumas.neighbors.list"))
	bootstrap.PUMAs.inspace <- parLapply(BootCluster, 1:B, function(b) PUMAs.all[1+length(PUMAs.all)*runif(1)])
	bootstrap.correct <- parSapply(BootCluster, bootstrap.PUMAs.inspace, function(x) max(is.element(x,PUMAs.all)==FALSE))
	print(paste("Drawing PUMA no. 1, Number of draws with invalid PUMAs:",max(bootstrap.correct)))
	bootstrap.PUMAs.inspace <- parLapply(BootCluster, bootstrap.PUMAs.inspace, sort)	
	unique.draws <- unique(bootstrap.PUMAs.inspace)
	print(paste("Number of unique draws with 1 PUMA:",length(unique.draws)))
	# Elements from 2 to 5 -- draw from neighbors of previously drawn PUMAs
	for (i in seq(2,length(pumas.Seattle))) {
		neighbors.current <- parLapply(BootCluster, bootstrap.PUMAs.inspace, function(x) setdiff(as.numeric(unlist(pumas.neighbors.list[is.element(names(pumas.neighbors.list),x)])),x))
		# Make sure that neighbors are correct 
		neighbors.correct <- parSapply(BootCluster, neighbors.current, function(x) max(is.element(x,PUMAs.all)==FALSE))
		print(paste("Drawing PUMA no.",i," Number of neighbors with invalid PUMAs:",sum(neighbors.correct)))
		clusterExport(BootCluster, c("neighbors.current","bootstrap.PUMAs.inspace"))
		bootstrap.PUMAs.inspace <- parLapply(BootCluster, 1:B, function(b) c(bootstrap.PUMAs.inspace[[b]], neighbors.current[[b]][1+length(neighbors.current[[b]])*runif(1)]))
		bootstrap.correct <- parSapply(BootCluster, bootstrap.PUMAs.inspace, function(x) max(is.element(x,PUMAs.all)==FALSE))
		print(paste("Drawing PUMA no.",i," Number of draws with invalid PUMAs:",sum(bootstrap.correct)))
		bootstrap.PUMAs.inspace <- parLapply(BootCluster, bootstrap.PUMAs.inspace, sort)	
		unique.draws <- unique(bootstrap.PUMAs.inspace)
		print(paste("Number of unique draws with",i,"PUMAs:",length(unique.draws)))
	}
	
	# Check that all PUMAs are different
	n.unique <- parSapply(BootCluster, bootstrap.PUMAs.inspace, function(x) length(unique(x)))
	print(paste("Min number diff PUMAs:",min(n.unique),"Max number diff PUMAs:",max(n.unique)))
	
	stopCluster(BootCluster)
	print(paste("Simulation with",B,"draws took",as.numeric((proc.time() - start.time)[3]/60),"minutes"))	
	
	# Save all possible combinations of PUMAs (to be used in bootstrap)
	saveRDS(unique.draws, file = paste0(path.to.data,"PUMA_unique_combinations.RDS"))


	ud <- data.frame(matrix(unlist(unique.draws), nrow=2994, byrow=TRUE),stringsAsFactors=FALSE)

	ud<- ud %>% 
	  rename(
	    puma1 = X1,
	    puma2 = X2,
	    puma3 = X3,
	    puma4 = X4,
	    puma5 = X5
	  )
  
  write.csv(rbind(ud), file = paste0(path.to.data,"contiguous.csv"))
