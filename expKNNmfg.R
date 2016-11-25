# Parameters:
# args[1] = {1, 2, ..., 256} // meta-feature group combination corresponding to
#						{"sl", "st", "it", "lm", "mb", "ti", "dc", "cn"}
# args[2] = {"rv", "nv"}	// real or normalized meta-feature vectors
# args[3] = {"ed", "ip", "cs", "pc"} // euclidean dist., inner prod., cosine sim., pearson corr.
# args[4] = {1,2,3} // k for nearest neighbors
# args[5] = {"pso", "rs", "dfs"}	// PSO, RS, DF (w.r.t. SVM)
# args[6] = {3,5,10} // folds for cross-validation (will try only 5)
# args[7] = {"svm", "J48"} // algorithm which hyper-parameters are predicted
#
# number of experiments = 256*2*4*3*3*1 = 18432

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

args <- commandArgs(TRUE)
# args = c("1", "rv", "ed", "1", "pso", "3", "svm")
ALGO = args[7]

cat(" @Algorithm: ", ALGO, "\n")
cat("Loading files ... \n")

my.files = list.files(path = "R", full.names = TRUE)
for(file in my.files) {
  source(file)
  cat(" - file: ", file, "\n")
}

# -----------------------------------------------------------------------------
# Main program
# -----------------------------------------------------------------------------

meta.feature.groups = convert.mf.group.combination.to.vector(as.numeric(args[1]))
ret = getComputationTimes(meta.feature.groups = meta.feature.groups, obj = obj)
sel.ids = which(rownames(ret$feature.matrix) %in% COMMON.DATA)

# normalize the feature matrix if required
if (args[2] == "nv") {
  cat(" - Scaling features.\n")
  feature.matrix = scale(ret$feature.matrix[sel.ids, ])
} else {
  cat(" - Using normal features.\n")
  feature.matrix = ret$feature.matrix[sel.ids, ]
}

result.matrix = fillParamsMfgKNN(result.matrix = result.matrix, args = args)
result.matrix[, "time.FE"] = ret$computation.times[sel.ids]

# compute the distance matrix between datasets from meta-feature vectors
distance.matrix = compute.distance.matrix.mf.vectors(feature.matrix, args[3])

# for each dataset compute its k nearest neighbors from the distance matrix
nearest.neighbors = lapply(1:length(datafile.names), function(i) {
	nearests = compute.k.nn(i, distance.matrix[i,], as.numeric(args[4]))
	return(nearests)
})

# for each dataset
for (i in 1:length(datafile.names)) {
	
	print(datafile.names[i])
	start.time = System$currentTimeMillis()
	
	# aggregate the best found hyper-parameters of the nearest neighbors
	aggregated.hp = get.aggregated.best.hp(nearest.neighbors[[i]], args[5], as.numeric(args[4]))
	
	# compute results of SVM using cross-validation
	for (j in 1:30) {
		set.seed(j)
		result.matrix[i,j] = runSVM(datafile = datafile.names[i], svm.cost = aggregated.hp[j,1], 
			svm.gamma = aggregated.hp[j,2], folds = as.numeric(args[6]))
		result.matrix[i,"mean.acc"] = mean(result.matrix[i,c(1:30)])
	}
	result.matrix[i,"time.comp"] = System$currentTimeMillis() - start.time
}

cat(" - Saving results \n")
file.name = paste(paste("expkNNmfg",paste("-", args, sep="", collapse=""),sep=""), "rda", sep=".")
dput(result.matrix, file = paste(results.dir,file.name,sep="/"))

