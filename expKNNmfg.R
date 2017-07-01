#--------------------------------------------------------------------------------------------------
# Command line parameters:
#--------------------------------------------------------------------------------------------------

# args[1] = {1, 2, ..., 256} // meta-feature group combination corresponding to
#						{"sl", "st", "it", "lm", "mb", "ti", "dc", "cn"}
# args[2] = {"rv", "nv"}	// real or normalized meta-feature vectors
# args[3] = {"ed", "ip", "cs", "pc"} // euclidean dist., inner prod., cosine sim., pearson corr.
# args[4] = {1,2,3} // k for nearest neighbors
# args[5] = {"pso", "rs", "df", "smbo"}	// PSO, RS, DF (w.r.t. SVM)
# args[6] = {3,5,10} // folds for cross-validation (will try only 5)
# args[7] = {"svm", "J48"} // algorithm which hyper-parameters are predicted
#
# number of experiments = 256*2*4*3*3*1 = 18432

#--------------------------------------------------------------------------------------------------
# Fucntion definitions
#--------------------------------------------------------------------------------------------------

args = c(1, "rv", "ed", "1", "smbo", "3", "svm")
# args = commandArgs(TRUE)

DIST      = args[3]
K         = as.numeric(args[4])
HP.TUNING = args[5]
FOLDS     = as.numeric(args[6])
ALGO      = args[7]

cat(" ========================================== \n")
cat(" * Running expkNNmfg with following parameters: \n")
for(i in 1:length(args)) {
  cat("    - arg[", i, "]:", args[i], "\n")
}
cat(" ========================================== \n")

# -----------------------------------------------------------------------------
# Required data
# -----------------------------------------------------------------------------

devtools::load_all()

dirs = getDataDirs(algo = ALGO, tuning = HP.TUNING)
ret  = getResultMatrix(dirs = dirs, algo = ALGO, tuning = HP.TUNING)

obj = ret$obj
result.matrix  = ret$mat
dataset.names  = ret$dataset.names
datafile.names = ret$datafile.names 

# -----------------------------------------------------------------------------
# Main program
# -----------------------------------------------------------------------------

meta.feature.groups = convert.mf.group.combination.to.vector(as.numeric(args[1]))
ret = getComputationTimes(meta.feature.groups = meta.feature.groups, obj = obj)

# normalize the feature matrix if required
if (args[2] == "nv") {
  cat(" - Scaling features.\n")
  feature.matrix = scale(ret$feature.matrix)
} else {
  cat(" - Using normal features.\n")
  feature.matrix = ret$feature.matrix
}

result.matrix = fillParamsMfgKNN(result.matrix = result.matrix, args = args)
result.matrix[, "time.FE"] = ret$computation.times

# compute the distance matrix between datasets from meta-feature vectors
distance.matrix = compute.distance.matrix.mf.vectors(input.data = feature.matrix, 
  distance.measure = DIST)

# for each dataset compute its k nearest neighbors from the distance matrix
nearest.neighbors = lapply(1:length(datafile.names), function(i) {
	nearests = compute.k.nn(dataset.number = i, distances = distance.matrix[i,], k = K)
	return(nearests)
})

# for each dataset
for (i in 1:length(datafile.names)) {
	
  datafile = datafile.names[i]
  cat(i, "/", length(datafile.names), " - Dataset: ", dataset.names[i], "\n")
	
	params = getHPSolutions(datasets = nearest.neighbors[[i]], 
    hp.technique = HP.TUNING, algo = ALGO, dirs = dirs)

  aggr.params = aggregateHyperParams(params = params, algo = ALGO)

	# compute results of SVM using cross-validation
  cat("/")
  tmp = lapply(1:REPETITIONS, function(rep.id) {
    
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
      params = params,  dirs = dirs, folds = FOLDS, trafo = trafo)
 
    pred.time = System$currentTimeMillis() - inner.time
    return(c(perf, pred.time))
  })
  cat("/\n")

  results = do.call("rbind", tmp)
  result.matrix[i, 1:30] = results[,1]
  result.matrix[i, "mean.acc"] = mean(results[,1]) 
  result.matrix[i, "time.comp"] = mean(results[,2]) 
}


cat(" - Saving results \n")
file.name = paste(paste("expkNNmfg",paste("-", args, sep="", collapse=""),sep=""), "rda", sep=".")
dput(result.matrix, file = paste(dirs$results.dir, file.name, sep="/"))

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------