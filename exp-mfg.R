# Parameters:
# args[1] = {1, 2, ..., 256} // meta-feature group combination corresponding to
#						{"sl", "st", "it", "lm", "mb", "ti", "dc", "cn"}
# args[2] = {"rv", "nv"}	// real or normalized meta-feature vectors
# args[3] = {"ed", "ip", "cs", "pc"} // euclidean dist., inner prod., cosine sim., pearson corr.
# args[4] = {1,2,3} // k for nearest neighbors
# args[5] = {"pso", "rs", "dfs"}	// PSO, RS, DF (w.r.t. SVM)
# args[6] = {3,5,10} // folds for cross-validation (will try only 5)
#
# number of experiments = 256*2*4*3*3*1 = 18432

suppressMessages(library(mlr))
suppressMessages(library(foreign))
suppressMessages(library(kernlab))
suppressMessages(library(e1071))
suppressMessages(library(R.utils))

source("R/config.R")
source("R/utils.R")

# -----------------------------------------------------------------------------
# Function definitions
# -----------------------------------------------------------------------------

# fill the parameter settings to the result.matrix
# arg[1] = the index of the row the settings are filled to
fill.parameter.settings <- function(ind) {
	result.matrix[ind,"MF.group.comb"] <<- as.numeric(args[1])
	
	if (args[2] == "rv") {
		result.matrix[ind,"DTW.MF.rv.nv"] <<- 1
	} else {
			result.matrix[ind,"DTW.MF.rv.nv"] <<- 2
		}
		
	if (args[3] == "ed") {
		result.matrix[ind,"MF.dist"] <<- 1
	} else {
			if (args[3] == "ip") {
				result.matrix[ind,"MF.dist"] <<- 2
			} else {
					if (args[3] == "cs") {
						result.matrix[ind,"MF.dist"] <<- 3
					} else {
							result.matrix[ind,"MF.dist"] <<- 4
						}
				}
		}
		
	result.matrix[ind,"NN.k"] <<- as.numeric(args[4])
	
	if (args[5] == "pso") {
		result.matrix[ind,"BL.alg"] <<- 1
	} else {
			if (args[5] == "rs") {
				result.matrix[ind,"BL.alg"] <<- 2
			} else {
					if (args[5] == "dfs") {
						result.matrix[ind,"BL.alg"] <<- 3
					} else {
							result.matrix[ind,"BL.alg"] <<- 4
						}
				}
		}

	result.matrix[ind,"EXP.folds"] <<- as.numeric(args[6])
}

# -----------------------------------------------------------------------------
# Main program
# -----------------------------------------------------------------------------

args <- commandArgs(TRUE)

meta.feature.groups <- convert.mf.group.combination.to.vector(as.numeric(args[1]))

# create the meta-feature matrix (rows as mf vectors) and get the corresponding computation times
feature.matrix <- NULL

computation.times <- matrix(0, nrow = length(rownames(obj$simple)[,1]), ncol = 1)
dimnames(computation.times) <- list(rownames(obj$simple), NULL)

if (meta.feature.groups[1]) {
	feature.matrix <- cbind(feature.matrix,obj$simple)
	computation.times[,1] <- computation.times[,1] + obj$subGroupTimes[,"simple_time"]
}
if (meta.feature.groups[2]) {
	feature.matrix <- cbind(feature.matrix,obj$statistical)
	computation.times[,1] <- computation.times[,1] + obj$subGroupTimes[,"statistical_time"]
}
if (meta.feature.groups[3]) {
	feature.matrix <- cbind(feature.matrix,obj$inftheo)
	computation.times[,1] <- computation.times[,1] + obj$subGroupTimes[,"inftheo_time"]
}
if (meta.feature.groups[4]) {
	feature.matrix <- cbind(feature.matrix,obj$landmarking)
	computation.times[,1] <- computation.times[,1] + obj$timeFeat[,"naive_bayes_time"] +
											 obj$timeFeat[,"lda_time"] + obj$timeFeat[,"stump_time"] +
											 obj$timeFeat[,"nn_time"]
}
if (meta.feature.groups[5]) {
	feature.matrix <- cbind(feature.matrix,obj$modelbased)
	computation.times[,1] <- computation.times[,1] + obj$timeFeat[,"tree_time"]
}
if (meta.feature.groups[6]) {
	feature.matrix <- cbind(feature.matrix,obj$timeFeat)
	
	time.to.add <- matrix(0, nrow = length(rownames(obj$simple)[,1]), ncol = 1)	
	if (meta.feature.groups[4] == FALSE) {
		time.to.add[,1] <- time.to.add[,1] + obj$timeFeat[,"naive_bayes_time"] +
											 obj$timeFeat[,"lda_time"] + obj$timeFeat[,"stump_time"] +
											 obj$timeFeat[,"nn_time"]
	}
	if (meta.feature.groups[5] == FALSE) {
		time.to.add[,1] <- time.to.add[,1] + obj$timeFeat[,"tree_time"]
	}
	
	computation.times[,1] <- computation.times[,1] + time.to.add[,1]
}
if (meta.feature.groups[7]) {
	feature.matrix <- cbind(feature.matrix,as.matrix(obj$dataComplex))
	computation.times[,1] <- computation.times[,1] + obj$subGroupTimes[,"dcol_time"]
}
if (meta.feature.groups[8]) {
	feature.matrix <- cbind(feature.matrix,obj$complexNet)
	computation.times[,1] <- computation.times[,1]	 + obj$subGroupTimes[,"cnet_time"]
}

# normalize the feature matrix if required
if (args[2] == "nv")
	feature.matrix <- scale(feature.matrix)

result.matrix[,"time.FE"] <- computation.times

# compute the distance matrix between datasets from meta-feature vectors
distance.matrix <- compute.distance.matrix.mf.vectors(feature.matrix, args[3])

# for each dataset compute its k nearest neighbors from the distance matrix
nearest.neighbors <- list()
for (i in 1:length(datafile.names)) {
	nearest.neighbors[[i]] <- compute.k.nn(i, distance.matrix[i,], as.numeric(args[4]))
}

# for each dataset
for (i in 1:length(datafile.names)) {
	print(datafile.names[i])

	start.time <- System$currentTimeMillis()
	
	# aggregate the best found hyper-parameters of the nearest neighbors
	aggregated.hp <- get.aggregated.best.hp(nearest.neighbors[[i]], args[5], as.numeric(args[4]))
	
	# compute results of SVM using cross-validation
	for (j in 1:30) {
		# result.matrix[i,j] <- compute.svm.cv(datafile.names[i], aggregated.hp[j,1], aggregated.hp[j,2],
																				 # as.numeric(args[6]))
		result.matrix[i,j] = runSVM(datafile.names[i], aggregated.hp[j,1], aggregated.hp[j,2], as.numeric(args[6]))
		result.matrix[i,"mean.acc"] <- mean(result.matrix[i,c(1:30)])
	}

	result.matrix[i,"time.comp"] <- System$currentTimeMillis() - start.time

  fill.parameter.settings(i)
}


# save the result.matrix to the disk
file.name <- paste(paste("exp-mfg",paste("-", args, sep="", collapse=""),sep=""), "rda", sep=".")
dput(result.matrix, file = paste(results.dir,file.name,sep="/"))

