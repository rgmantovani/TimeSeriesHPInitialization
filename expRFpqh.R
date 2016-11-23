#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

# Parameters:
# args[1] = {"in", "ex"} // include, exclude class in PCA
# args[2] = {-10, ..., 10}	// gamma for the RBF kernel for KPCA (if < -10 or >10 then PCA computed)
# args[3] = {"qu", "hi"}	// quantiles or histogram
# args[4] = {5,10,20} // bins for quantiles or histograms
# args[5] = {"ed", "ip", "cs", "pc"} // euclidean dist., inner prod., cosine sim., pearson corr. 
# args[6] = {"pso", "rs", "df", "smbo"}	// PSO, RS, DF (w.r.t. SVM)
# args[7] = {3,5,10} // folds for cross-validation (will try only 5)
# args[8] = {"rv", "nv"}	// real or normalized quantile vectors (! only for quantiles, ... added only later)
# args[9] = {"svm", "J48"} // algo to be analyzed
#
# number of experiments = 2*22*2*3*4*3*3*1 = 9504 * 2

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
# Main program
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

# args <- commandArgs(TRUE)
args = c("in", -10, "qu", 5, "ed", "rs", 3, "rv", "svm")    
# args = c("in", -10, "qu", 5, "ed", "rs", 3, "rv", "J48")    
ALGO = args[9]

# Check parameters with checkmate?

cat(" @Algorithm: ", ALGO, "\n")
cat("Loading files ... \n")
my.files = list.files(path = "R", full.names = TRUE)
for(file in my.files) {
  source(file)
  cat(" - file: ", file, "\n")
}

result.matrix = fillParamsPqh(result.matrix = result.matrix, args = args)
result.matrix[ , "MTL.alg"] = 2 # 1 for kNN, 2 for RF

# for each dataset compute PCA/KPCA after binarizing them
eigenvalues = list()

for (i in 1:length(datafile.names)) {
	start.time = System$currentTimeMillis()
	data.file = paste(data.dir, datafile.names[i], sep="/")
	# print(datafile.names[i])
	pp.data = read.pre.process.data.pca(data.file = data.file, inex = args[1])
	# print(head(pp.data))
	# browser()
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
# distance.matrix = compute.distance.matrix.qu.hi(eigenvalues, args[3], 
	# as.numeric(args[4]), args[5], args[9])

# Use eigen values as meta-features or distance matrix ?
common.size = min(unlist(lapply(eigenvalues, length)))
pca.meta.features = data.frame(
	do.call("rbind", 
		lapply(eigenvalues, function(pos) return(pos[1:common.size]))
	)
)

row.names(pca.meta.features) = dataset.names
hp.solutions = getHPSolutions(datasets = dataset.names, hp.technique = args[6], algo = ALGO)

reps = 30
outer.aux = lapply(1:reps, function(rep.id) {
	
	# seed is the rep id
	set.seed(seed = rep.id)
  cat(" @ Repetition: ", rep.id, " ... \n")
  targets = do.call("rbind", lapply(hp.solutions, function(pos) return(pos[rep.id, ])))

  hp.predicted = multitargetRF(feature.matrix = pca.meta.features, targets = targets)

  inner.aux = lapply(1:length(datafile.names), function(i) {

    datafile   = datafile.names[i]
  	hp.setting = hp.predicted[i,]

    response = hp.setting[grepl(pattern = "response", x = colnames(hp.setting))]
    colnames(response) = gsub(x = colnames(response), 
      pattern = paste0(toupper(args[6]), "\\.|\\.response"), replacement = "")
    params = as.list(response)
   
    inner.time = System$currentTimeMillis()

    if(ALGO == "svm") {
    	trafo = function(x) return(2^x)
    } else {
    	trafo = NULL
    }
    perf = runBaseLearner(datafile = datafile, algo = ALGO, 
    	params = params, folds = as.numeric(args[7]), trafo = trafo)
 
    pred.time = System$currentTimeMillis() - inner.time #+ model.time
 
    return(c(perf, pred.time))
  })

  tmp = do.call("rbind", inner.aux)
  colnames(tmp) = c(paste0("acc.", rep.id), "runtime") 
  return(tmp) 

})

# updating result.matrix
accs  = do.call("cbind", lapply(outer.aux, function(pos) return(pos[,1]))) 
times = do.call("cbind", lapply(outer.aux, function(pos) return(pos[,2]))) 

colnames(accs) = paste0("acc.", 1:ncol(accs))
result.matrix[ , 1:ncol(accs)] = accs

result.matrix[, "mean.acc"] = rowMeans(accs)
result.matrix[, "time.comp"] = rowMeans(times)

# save the result.matrix to the disk
file.name = paste(paste("expRFpqh",paste("-", args, sep="", collapse=""),sep=""), "rda", sep=".")
dput(result.matrix, file = paste(results.dir,file.name,sep="/"))

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

