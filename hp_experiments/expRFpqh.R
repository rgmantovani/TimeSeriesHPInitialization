#--------------------------------------------------------------------------------------------------
# Command line parameters:
#--------------------------------------------------------------------------------------------------

# Parameters:
# args[1] = {"in", "ex"} // include, exclude class in PCA
# args[2] = {-10, ..., 10}	// gamma for the RBF kernel for KPCA (if < -10 or >10 then PCA computed)
# args[3] = {"qu", "hi"}	// quantiles or histogram
# args[4] = {5,10,20} // bins for quantiles or histograms
# args[5] = {"pso", "rs", "df", "smbo"}	// PSO, RS, DF (w.r.t. SVM)
# args[6] = {3,5,10} // folds for cross-validation (will try only 5)
# args[7] = {"rv", "nv"}	// real or normalized quantile vectors (! only for quantiles, ... added only later)
# args[8] = {"svm", "J48"} // algo to be analyzed
#
# number of experiments = 2*22*2*3*4*3*3*1 = 9504 * 2

#--------------------------------------------------------------------------------------------------
# *** Args examples
#--------------------------------------------------------------------------------------------------

# args = c("in", 2, "hi", 10, "pso", 3, "rv", "svm")
# args = c("in", 2, "hi", 10, "smbo", 3, "rv", "J48")

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

args = commandArgs(TRUE)

if(length(args) != 8) {
  stop("Please, provide all the eight args:\n <in-ex> <-10:10> <qu-hi> <5-10-20> <pso-rs-df-smbo> <3-5-10> <rv-nv> <svm-J48>")
}

BINS      = as.numeric(args[4])
HP.TUNING = args[5]
FOLDS     = as.numeric(args[6])
RV.NV     = args[7]
ALGO      = args[8]

cat(" ========================================== \n")
cat(" * Running expRFpqh with following parameters: \n")
for(i in 1:length(args)) {
  cat("    - arg[",i,"]:", args[i], "\n")
}
cat(" ========================================== \n")

#--------------------------------------------------------------------------------------------------
# Required data
#--------------------------------------------------------------------------------------------------

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

result.matrix = fillParamsPqhRF(result.matrix = result.matrix, args = args)

# for each dataset compute PCA/KPCA after binarizing them
cat(" @ Calculating eigenvalues \n")
eigenvalues = list()
for (i in 1:length(datafile.names)) {
	
  start.time = System$currentTimeMillis()
	data.file  = paste(dirs$data.dir, datafile.names[i], sep="/")
  pp.data    = read.pre.process.data.pca(data.file = data.file, inex = args[1])

	gamma = as.numeric(args[2])	
	if (gamma < -10 || gamma > 10) {
		pca = prcomp(pp.data, center = TRUE, scale. = TRUE)
		eigenvalues[[i]] = pca$sdev^2
	} else {
		pca = kpca(~., data=pp.data, kernel = "rbfdot", kpar = list(sigma=2^gamma), 
      features=0, th=0)
		eigenvalues[[i]] = pca@eig
	}
	result.matrix[i,"time.FE"] = System$currentTimeMillis() - start.time
}

cat(" @ Creating meta-features \n")
aux = lapply(eigenvalues, function(elem) {
	if(args[3] == "qu") {
		value = find.quantiles(input.data = elem, quantiles = BINS, rv.nv = RV.NV)
	} else {
		value = find.histogram(input.data = elem, bins = BINS, rv.nv = RV.NV)
	}
	return(value)
})

cat(" @ Retrieving HP solutions \n")
pca.meta.features = data.frame(do.call("rbind", aux), row.names = dataset.names)
hp.solutions = getHPSolutions(datasets = dataset.names, hp.technique = HP.TUNING, 
  algo = ALGO, dirs = dirs)

# doing predictions
outer.aux = lapply(1:REPETITIONS, function(rep.id) {
  	
	set.seed(seed = rep.id)
  cat(" @ Repetition: ", rep.id, " ... \n")

  targets = do.call("rbind", lapply(hp.solutions, function(pos) return(pos[rep.id, ])))
  hp.predicted = multitargetRF(feature.matrix = pca.meta.features, targets = targets)

  inner.aux = lapply(1:length(datafile.names), function(i) {

    datafile   = datafile.names[i]
  	hp.setting = hp.predicted[i,]

    response = hp.setting[grepl(pattern = "response", x = colnames(hp.setting))]
    colnames(response) = gsub(x = colnames(response), 
      pattern = paste0(toupper(HP.TUNING), "\\.|\\.response"), replacement = "")
    params = as.list(response)
   
    inner.time = System$currentTimeMillis()

	  if(ALGO == "svm" & HP.TUNING != "df") {
    	trafo = function(x) return(2^x)
    } else {
    	trafo = NULL
    }

    perf = runBaseLearner(datafile = datafile, algo = ALGO, 
    	params = params, dirs = dirs, folds = FOLDS, trafo = trafo)
 
    pred.time = System$currentTimeMillis() - inner.time
 
    return(c(perf, pred.time))
  })

  tmp = do.call("rbind", inner.aux)
  colnames(tmp) = c(paste0("acc.", rep.id), "runtime") 
  return(tmp) 
})

accs  = do.call("cbind", lapply(outer.aux, function(pos) return(pos[,1]))) 
times = do.call("cbind", lapply(outer.aux, function(pos) return(pos[,2]))) 

colnames(accs) = paste0("acc.", 1:ncol(accs))
result.matrix[ , 1:ncol(accs)] = accs

result.matrix[, "mean.acc"] = rowMeans(accs)
result.matrix[, "time.comp"] = rowMeans(times)

# save the result.matrix to the disk
cat(" - Saving results \n")
file.name = paste(paste("expRFpqh",paste("-", args, sep="", collapse=""),sep=""), "rda", sep=".")
dput(result.matrix, file = paste(dirs$results.dir,file.name,sep="/"))

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------