# Parameters:
# args[1] = {"pso", "rs", "dfs"}	// PSO, RS, DF (w.r.t. SVM)
# args[2] = {3,5,10} // folds for cross-validation (will try only 5)
# 
# number of experiments = 3 * 1 = 3

# -----------------------------------------------------------------------------
# Main program
# -----------------------------------------------------------------------------

# args <- commandArgs(TRUE)

# args = c("pso", 3, "svm")
ALGO = args[3]

my.files = list.files(path = "R", full.names = TRUE)
for(file in my.files) {
  source(file)
  cat(" - loading file: ", file, "\n")
}

result.matrix = fillParamsDrp(result.matrix = result.matrix, args = args)

# for each dataset
for (i in 1:length(datafile.names)) {
	print(datafile.names[i])

	start.time <- System$currentTimeMillis()
	
	# aggregate the best found hyper-parameters of the nearest neighbors
	aggregated.hp = get.aggregated.best.hp(c(dataset.names[i]), args[1], 1)

	# compute results of SVM using cross-validation
	for (j in 1:30) {
		result.matrix[i,j] <- compute.svm.cv(datafile.names[i], aggregated.hp[j,1], aggregated.hp[j,2],
																				 as.numeric(args[2]))
		result.matrix[i,"mean.acc"] <- mean(result.matrix[i,c(1:30)])
	}
	result.matrix[i,"time.comp"] <- System$currentTimeMillis() - start.time
}

# save the result.matrix to the disk
file.name <- paste(paste("exp-drp",paste("-", args, sep="", collapse=""),sep=""), "rda", sep=".")
dput(result.matrix, file = paste(results.dir,file.name,sep="/"))

