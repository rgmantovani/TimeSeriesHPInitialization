# Parameters:
# args[1]  = {"in", "ex"} // include, exclude class in PCA
# args[2]  = {-10, ..., 10}	// gamma for the RBF kernel for KPCA (if <-10 or >10 then PCA computed)
# args[3]  = {"qu", "hi"}	// quantiles or histogram
# args[4]  = {5,10,20} // bins for quantiles or histograms
# args[5]  = {"ed", "ip", "cs", "pc"} // euclidean dist., inner prod., cosine sim., pearson corr. 
# args[6]  = {1,2,3} // k for nearest neighbors
# args[7]  = {"pso", "rs", "dfs", "smbo"}	// PSO, RS, DF (w.r.t. SVM)
# args[8]  = {3,5,10} // folds for cross-validation (will try only 5)
# args[9]  = {"rv", "nv"}	// real or normalized quantile vectors (! only for quantiles, ... added only later)
# args[10] = {"svm", "J48"} // algo to be analyzed

# number of experiments = 2*22*2*3*4*3*3*1 = 9504 * 2

#--------------------------------------------------------------------------------------------------
# Fucntion definitions
#--------------------------------------------------------------------------------------------------

args = commandArgs(TRUE)
# args = c("in", "100", "qu", "5", "ed", "1", "pso", "3", "rv", "svm")
ALGO = args[10]

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

result.matrix = fillParamsPqhKNN(result.matrix = result.matrix, args = args)
result.matrix[ , "MTL.alg"] = 1 # 1 for kNN, 2 for RF

# for each dataset compute PCA/KPCA after binarizing them
cat(" @ Calculating eigenvalues \n")
eigenvalues = list()
for (i in 1:length(datafile.names)) {
	
	start.time = System$currentTimeMillis()
	data.file  = paste(data.dir, datafile.names[i], sep="/")

	pp.data = read.pre.process.data.pca(data.file = data.file, inex = args[1])
	
	gamma = as.numeric(args[2])	
	if (gamma < -10 || gamma > 10) {
		pca = prcomp(pp.data, center = TRUE, scale. = TRUE)
		eigenvalues[[i]] = pca$sdev^2
	} else {
		pca = kpca(~., data=pp.data, kernel = "rbfdot", kpar = list(sigma=2^gamma), features=0, th=0)
		eigenvalues[[i]] = pca@eig
	}
	result.matrix[i,"time.FE"] = System$currentTimeMillis() - start.time
}

# compute the distance matrix between datasets from eigenvalues
cat(" @ Computing distance matrix \n")
distance.matrix = compute.distance.matrix.qu.hi(input.data = eigenvalues, qu.hi = args[3], 
	num.bins = as.numeric(args[4]), distance.measure = args[5], rv.nv = args[9])

# for each dataset compute its k nearest neighbors from the distance matrix
nearest.neighbors = lapply(1:length(datafile.names), function(i) {
	nearests = compute.k.nn(dataset.number = i, distances = distance.matrix[i,], 
		k = as.numeric(args[6]))
	return(nearests)
})

# for each dataset
for (i in 1:length(datafile.names)) {
	
	print(datafile.names[i])
	start.time = System$currentTimeMillis()
	
	# aggregate the best found hyper-parameters of the nearest neighbors
	aggregated.hp = get.aggregated.best.hp(datasets.to.consider = nearest.neighbors[[i]], 
		hp.tuning = args[7], k = as.numeric(args[6]))
	
	# compute results of SVM using cross-validation
	for (j in 1:30) {
		set.seed(j)
		result.matrix[i,j] = runSVM(datafile = datafile.names[i], svm.cost = aggregated.hp[j,1], 
			svm.gamma = aggregated.hp[j,2], folds = as.numeric(args[8]))
		result.matrix[i,"mean.acc"] = mean(result.matrix[i,c(1:30)])
	}
	result.matrix[i,"time.comp"] = System$currentTimeMillis() - start.time
}

# save the result.matrix to the disk
cat(" - Saving results \n")
file.name = paste(paste("expKNNpqh",paste("-", args, sep="", collapse=""),sep=""), "rda", sep=".")
dput(result.matrix, file = paste(results.dir,file.name,sep="/"))

