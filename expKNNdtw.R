# Parameters:
# args[1] = {"in", "ex"} // include, exclude class in PCA
# args[2] = {-10, ..., 10}	// gamma for the RBF kernel for KPCA (if <-10 or >10 then PCA computed)
# args[3] = {"rv", "nv"}	// resulting DTW, normalized DTW
# args[4] = {1,2,3} // k for nearest neighbors
# args[5] = {"pso", "rs", "df", "smbo"}	// PSO, RS, DF (w.r.t. SVM)
# args[6] = {3,5,10} // folds for cross-validation (will try only 5)
# args[7] = {"svm", "J48"} // algo to be analyzed
# 
# number of experiments = 2*22*2*3*3*1*2 = 792 * 2

#--------------------------------------------------------------------------------------------------
# Fucntion definitions
#--------------------------------------------------------------------------------------------------

args = commandArgs(TRUE)

K         = as.numeric(args[4])
HP.TUNING = args[5]
FOLDS     = as.numeric(args[6])
ALGO      = args[7]

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

result.matrix = fillParamsDtw(result.matrix = result.matrix, args = args)

# for each dataset compute PCA/KPCA after binarizing them
cat(" @ Calculating eigenvalues \n")
eigenvalues <- list()
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
distance.matrix = compute.distance.matrix.dtw(input.data = eigenvalues, rv.nv = args[3])

# for each dataset compute its k nearest neighbors from the distance matrix
nearest.neighbors = lapply(1:length(datafile.names), function(i) {
	nearests = compute.k.nn(dataset.number = i, distances = distance.matrix[i,], k = K)
	return(nearests)
})

# for each dataset
for (i in 1:length(datafile.names)) {
	
	cat(i, "/", length(datafile.names), " - Dataset: ", dataset.names[i], "\n")
	datafile = datafile.names[i]
		
	# aggregate the best found hyper-parameters of the nearest neighbors
	params = getHPSolutions(datasets = nearest.neighbors[[i]], 
		hp.technique = HP.TUNING, algo = ALGO)
  aggr.params = aggregateHyperParams(params = params, algo = ALGO)

	# compute results of SVM using cross-validation
	cat("/")
  tmp = lapply(1:30, function(rep.id) {
    set.seed(rep.id)
    cat("=")
    hp.setting = aggr.params[rep.id, ]
    response = hp.setting[grepl(pattern = "response", x = colnames(aggr.params))]
    
    names(response) = gsub(x = names(response), 
      pattern = paste0(toupper(HP.TUNING), "\\.|\\.response"), replacement = "")

    params = as.list(response)
    inner.time = System$currentTimeMillis()

    if(ALGO == "svm" & HP.TUNING != "df") {
      trafo = function(x) return(2^x)
    } else {
      trafo = NULL
    }

    perf = runBaseLearner(datafile = datafile, algo = ALGO, 
      params = params, folds = FOLDS, trafo = trafo)
 
    pred.time = System$currentTimeMillis() - inner.time
    return(c(perf, pred.time))
  })
  cat("/\n")
  
	results = do.call("rbind", tmp)
  result.matrix[i, 1:30] = results[,1]
  result.matrix[i, "mean.acc"] = mean(results[,1]) 
  result.matrix[i, "time.comp"] = mean(results[,2]) 
}

# save the result.matrix to the disk
file.name = paste(paste("expKNNdtw",paste("-", args, sep="", collapse=""),sep=""), "rda", sep=".")
dput(result.matrix, file = paste(results.dir,file.name,sep="/"))

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
