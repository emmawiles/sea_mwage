

# Time MatchIt using ESD data 



# Start log 
sink(file=paste0(path.to.log,"Find_matches_",substr(trimws(Sys.time()),1,10),".log"))


#############################
### Choose the parameters ###
#############################

cohort = c(2012, 2015) # Cohort: 2015 or 2016 for treated periods, 2012 or 2013 for falsification test
nn_vect = c(1) # Number of neighbors of treated obs (all cases)
nn_se = 1 # Number of neighbors used to estimate standard errors

#########################
### Choose what to do ###
#########################

do_random_sample = TRUE
do_find_matches_treated = TRUE 
do_find_matches_control_neighbors = FALSE

# Set seed for replication
set.seed(890609)

##############################*
###                         ###
### Load and clean the data ###
###                         ###
##############################*

for (cohort in cohort) {

  
### Load the data ###
ESDdata <- read.dta13(paste0(path.to.data,"mw",cohort,"_cohort_100.dta"))

### Set wage rate of workers without jobs to 0
### for periods used for matching
### (these people will be matched to other workers with "0" wagerate)
ESDdata$wagerate_l1[is.na(ESDdata$wagerate_l1)] = 0
ESDdata$wagerate_l2[is.na(ESDdata$wagerate_l2)] = 0

ESDdata$multiplejobs_0[is.na(ESDdata$multiplejobs_0)] = 0
ESDdata$multiplejobs_l1[is.na(ESDdata$multiplejobs_l1)] = 0
ESDdata$multiplejobs_l2[is.na(ESDdata$multiplejobs_l2)] = 0

### Rename variables withi cohort specific names ###
setnames(ESDdata,c(paste0("dur_acct",cohort),paste0("dur_age",cohort)),c("dur_acct","dur_age"))

# Flag obs with all non-missing variables 
varlist <- c("hours_0","hours_l1","hours_l2","wagerate_0","wagerate_l1","wagerate_l2","dur_acct","dur_age",
			 "multiplejobs_0","multiplejobs_l1","multiplejobs_l2",
			 "employed_0","employed_l1","employed_l2","new_entrant_0","new_entrant_l1","new_entrant_l2")
ESDdata$nonmis <- 1
for (v in varlist) ESDdata$nonmis[is.na(ESDdata[[v]])] <- 0

# Save order of the variables
ESDdata$matchID <- row.names(ESDdata)

# Summary statistics on the sample to make sure things make sense
print(paste("Summary statistics on the data from ",cohort,"cohort"))
print(summary(ESDdata$treat[ESDdata$nonmis==1])) 
print(summary(ESDdata$multiest_0[ESDdata$nonmis==1]))

#####################
###               ###
### Random sample ###
###               ###
#####################

if (do_random_sample==TRUE) {

	# Generate random number sequence
 # generate variables sampler_T and sampler C 
	ESDdata$sampler_T[ESDdata$treat==1 & ESDdata$nonmis==1] <- 100*runif(length(ESDdata$treat[ESDdata$treat==1 & ESDdata$nonmis==1]))
	ESDdata$sampler_C[ESDdata$treat==0 & ESDdata$nonmis==1] <- 100*runif(length(ESDdata$treat[ESDdata$treat==0 & ESDdata$nonmis==1]))
	ESDdata$sampler_T[ESDdata$treat==0] <- 0
	ESDdata$sampler_C[ESDdata$treat==1] <- 0

	# Check that samplers work
	#HW these don't quite make sense-- i'm not seeing output
	print(paste("Number of treated observations:",length(ESDdata$matchID[ESDdata$treat==1 & ESDdata$nonmis==1])))
	print(paste("1% sample of treated observations:",length(ESDdata$matchID[floor(ESDdata$sampler_T/1)==1 & ESDdata$treat==1 & ESDdata$nonmis==1])))
	print(paste("10% sample of treated observations:",length(ESDdata$matchID[floor(ESDdata$sampler_T/10)==1 & ESDdata$treat==1 & ESDdata$nonmis==1])))
	print(paste("50% sample of treated observations:",length(ESDdata$matchID[floor(ESDdata$sampler_T/50)==1 & ESDdata$treat==1 & ESDdata$nonmis==1])))

	print(paste("Number of control observations:",length(ESDdata$matchID[ESDdata$treat==0 & ESDdata$nonmis==1])))
	print(paste("1% sample of control observations:",length(ESDdata$matchID[floor(ESDdata$sampler_C/1)==1 & ESDdata$treat==0 & ESDdata$nonmis==1])))
	print(paste("10% sample of control observations:",length(ESDdata$matchID[floor(ESDdata$sampler_C/10)==1 & ESDdata$treat==0 & ESDdata$nonmis==1])))
	print(paste("50% sample of control observations:",length(ESDdata$matchID[floor(ESDdata$sampler_C/50)==1 & ESDdata$treat==0 & ESDdata$nonmis==1])))

	# Save sample with randlm numbers for testing in replication in Stata 
	save.dta13(ESDdata[ , c("personid","matchID","nonmis","treat","sampler_T","sampler_C")], file = paste0(path.to.save,"ESDdata_randomsample_",cohort,"cohort.dta"))
}

############################################
##                                       ###
## Find matches for treated observations ###
## WARNING! TAKES SEVERAL DAYS TO RUN    ###
##                                       ###
############################################

if (do_find_matches_treated==TRUE) {

	# Variables for exact matching
	ematch <- c("employed_0","employed_l1","employed_l2","new_entrant_0","new_entrant_l1","new_entrant_l2")

	# Set up the data frame with timing
	timing.matchit <- data.frame(treat_size = integer(), control_size = integer(), time.match = double(), time.save = double(), nn = integer())

	Matches <- list()
	MatchedSample <- list()
	MatchedMatrix <- list()

	for (i in 1:length(nn_vect)) {

		print(paste("Working on 100% of treated sample matched to control sample with",nn_vect[[i]],"neighbors"))

		start.time <- proc.time()
		# Find matches with the package MatchIt
		Matches[[i]] <- matchit(treat ~ hours_0 + hours_l1 + hours_l2 + wagerate_0 + wagerate_l1 + wagerate_l2 + dur_acct + dur_age + multiplejobs_0 + multiplejobs_l1 + multiplejobs_l2,
						data = ESDdata[ESDdata$nonmis == 1, c("personid","matchID","treat",varlist)],
						method = "nearest", exact = ematch,
						#HW you should change the replace from true to false-- you should check on the discard and the verbose
						distance = "mahalanobis", ratio = nn_vect[[i]], replace = FALSE,
						discard = "none", verbose = TRUE)
		# Get the matched sample
		MatchedSample[[i]] <- match.data(Matches[[i]], group = "all", distance = "distance", weights = "weights", subclass = "subclass")
		# Get the Ids of nearest neighbors
		MatchedMatrix[[i]] <- data.frame(matchID = row.names(Matches[[i]]$match.matrix), Matches[[i]]$match.matrix)
		setnames(MatchedMatrix[[i]],paste0("X",seq(1,nn_vect[[i]])),paste0("nn",seq(1,nn_vect[[i]])))
		MatchedSample[[i]] <- merge(MatchedSample[[i]], MatchedMatrix[[i]], by = "matchID", all.x = TRUE, all.y = TRUE)
		# Convert match IDs into String (for proper reading in Stata)
		for (j in 1:nn_vect[[i]]) {
			MatchedSample[[i]][[paste0("nn",j)]] <- as.character(MatchedSample[[i]][[paste0("nn",j)]])
		}
		start.time2 <- proc.time()

		# Save the matched sample in RDS format (takes little space)
		saveRDS(MatchedSample, file = paste0(path.to.save,"ESD_matched_sample_treated_",cohort,"cohort_wo_rep_100pct.RDS"))
		# Save time to find matches
		timing.matchit <- rbind(timing.matchit,
						data.frame(treat_size = 100, control_size = 100, nn = nn_vect[[i]],
								   time.match = (start.time2 - start.time)[3]/(60*60),
								   time.save = (proc.time() - start.time2)[3]/(60*60)))
		saveRDS(timing.matchit, file = paste0(path.to.log,"timing_matched_sample_treated_worep_",cohort,"cohort.RDS"))
		# Save matched sample in Stata format (takes more space)
		save.dta13(MatchedSample[[i]], file = paste0(path.to.save,"ESD_matched_sample_treated_",cohort,"cohort_wo_rep_100pct_nn",nn_vect[[i]],".dta"))
		# Show current timing and KEEP GOING!
		print(timing.matchit)
	}
}

sink()

