# Parameters:
# args[1] = {"in", "ex"} // include, exclude class in PCA
# args[2] = {-10, ..., 10}	// gamma for the RBF kernel for KPCA (if <-10 or >10 then PCA computed)
# args[3] = {"rv", "nv"}	// resulting DTW, normalized DTW
# args[4] = {1,2,3} // k for nearest neighbors
# args[5] = {"pso", "rs", "dfs"}	// PSO, RS, DF (w.r.t. SVM)
# args[6] = {3,5,10} // folds for cross-validation (will try only 5)
#
# number of experiments = 2*22*2*3*3*1 = 792

suppressMessages(library(mlr))
suppressMessages(library(foreign))
suppressMessages(library(kernlab))
suppressMessages(library(dtw))
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
	if (args[1] == "in") {
		result.matrix[ind,"PCA.in.ex"] <<- 1
	} else {
			result.matrix[ind,"PCA.in.ex"] <<- 2
		}
	
	result.matrix[ind,"PCA.gamma"] <<- as.numeric(args[2])
	
	if (args[3] == "rv") {
		result.matrix[ind,"DTW.MF.rv.nv"] <<- 1
	} else {
			result.matrix[ind,"DTW.MF.rv.nv"] <<- 2	
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

# for each dataset compute PCA/KPCA after binarizing them
eigenvalues <- list()
for (i in 1:length(datafile.names)) {
	start.time <- System$currentTimeMillis()

	pp.data <- read.pre.process.data.pca(paste(data.dir, datafile.names[i], sep="/"), args[1])
	
	gamma <- as.numeric(args[2])	
	if (gamma < -10 || gamma > 10) {
		pca <- prcomp(pp.data, center = TRUE, scale. = TRUE)
		eigenvalues[[i]] <- pca$sdev^2
	} else {
		pca <- kpca(~., data=pp.data, kernel = "rbfdot", kpar = list(sigma=2^gamma), features=0, th=0)
		eigenvalues[[i]] <- pca@eig
	}

	result.matrix[i,"time.FE"] <- System$currentTimeMillis() - start.time
}

# compute the distance matrix between datasets from eigenvalues
distance.matrix <- compute.distance.matrix.dtw(eigenvalues, args[3])

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
		result.matrix[i,j] <- compute.svm.cv(datafile.names[i], aggregated.hp[j,1], aggregated.hp[j,2],
																				 as.numeric(args[6]))
		result.matrix[i,"mean.acc"] <- mean(result.matrix[i,c(1:30)])
	}

	result.matrix[i,"time.comp"] <- System$currentTimeMillis() - start.time

  fill.parameter.settings(i)
}

# save the result.matrix to the disk
file.name <- paste(paste("exp-dtw",paste("-", args, sep="", collapse=""),sep=""), "rda", sep=".")
dput(result.matrix, file = paste(results.dir,file.name,sep="/"))

