#--------------------------------------------------------------------------------------------------
# Command line parameters:
#--------------------------------------------------------------------------------------------------

# Parameters:
# args[1] = {"pso", "rs", "df", "smbo"}  // PSO, RS, DF (w.r.t. SVM)
# args[2] = {3,5,10} // folds for cross-validation (will try only 5)
# args[3] = {"svm", "J48"}
# number of experiments = 3 * 1 * 2 = 6

#--------------------------------------------------------------------------------------------------
# Function definitions
#--------------------------------------------------------------------------------------------------

# args = c("df", 3, "J48")
args = commandArgs(TRUE)

HP.TUNING = args[1]
FOLDS     = as.numeric(args[2])
ALGO      = args[3]

cat(" ========================================== \n")
cat(" * Running expDrp with following parameters: \n")
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

#--------------------------------------------------------------------------------------------------
# Main program
#--------------------------------------------------------------------------------------------------

# Filling parameters to the result matrix
result.matrix = fillParamsDrp(result.matrix = result.matrix, args = args)

# for each dataset
for (i in 1:length(datafile.names)) {
  
  datafile = datafile.names[i]
  cat(i,"/", length(datafile.names), "-", datafile,"\n")
  
  settings = getHPSolutions(datasets = dataset.names[i], 
    hp.technique = HP.TUNING, algo = ALGO, dirs = dirs)[[1]]

  colnames(settings) = paste(colnames(settings), "response", sep=".")  
  aggregated.hp = checkParams(settings = settings, algo = ALGO)

  # compute results of SVM using cross-validation
  cat("/")
  aux = lapply(1:REPETITIONS, function(j) {

    cat("=")
    set.seed(j)
    response = aggregated.hp[j, ]
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
      params = params, dirs = dirs, folds = FOLDS, trafo = trafo)

    pred.time = System$currentTimeMillis() - inner.time

    return(c(perf, pred.time))
  })
  cat("/\n")
  
  tmp = do.call("rbind", aux)
  
  result.matrix[i, "time.comp"] = mean(tmp[,2])
  result.matrix[i, 1:30] = tmp[,1]
}

result.matrix[, "mean.acc"] = rowMeans(result.matrix[, 1:30])

cat(" - Saving results \n")
file.name = paste(paste("expDrp",paste("-", args, sep="", collapse=""),sep=""), "rda", sep=".")
dput(result.matrix, file = paste(dirs$results.dir,file.name,sep="/"))

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------