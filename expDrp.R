# Parameters:
# args[1] = {"pso", "rs", "dfs"}  // PSO, RS, DF (w.r.t. SVM)
# args[2] = {3,5,10} // folds for cross-validation (will try only 5)
# args[3] = {"svm", "J48"}
# number of experiments = 3 * 1 * 2 = 6

# -----------------------------------------------------------------------------
# Main program
# -----------------------------------------------------------------------------

args <- commandArgs(TRUE)
ALGO = args[3]

my.files = list.files(path = "R", full.names = TRUE)
for(file in my.files) {
  source(file)
  cat(" - loading file: ", file, "\n")
}

# Filling parameters to the result matrix
result.matrix = fillParamsDrp(result.matrix = result.matrix, args = args)

# for each dataset
for (i in 1:length(datafile.names)) {
  
  print(datafile.names[i])
  datafile = datafile.names[i]

  start.time = System$currentTimeMillis()

  settings = getHPSolutions(datasets = dataset.names[i], 
    hp.technique = args[1], algo = args[3])[[1]]

  colnames(settings) = paste(colnames(settings), "response", sep=".")  
  aggregated.hp = checkParams(settings = settings, algo = args[3])

  # compute results of SVM using cross-validation
  aux = lapply(1:30, function(j) {

    response = aggregated.hp[j, ]
    names(response) = gsub(x = names(response), 
      pattern = paste0(toupper(args[1]), "\\.|\\.response"), replacement = "")
    params = as.list(response)

    if(args[3] == "svm") {
      perf = runBaseLearner(datafile = datafile, algo = "svm", params = params, 
        folds = as.numeric(args[2]), trafo = function(x) return(2^x))
    } else if(args[3] == "J48") {
      perf = runBaseLearner(datafile = datafile, algo = "J48", params = params, 
        folds = as.numeric(args[2]))
    } else {
      stop("Invalid base learner!")
    }
    return(perf)
  })

  result.matrix[i, "time.comp"] = System$currentTimeMillis() - start.time
  result.matrix[i, 1:30] = do.call("rbind", aux)
}

result.matrix[, "mean.acc"] = rowMeans(result.matrix[, 1:30])
print(result.matrix)

# save the result.matrix to the disk
file.name = paste(paste("expDrp",paste("-", args, sep="", collapse=""),sep=""), "rda", sep=".")
dput(result.matrix, file = paste(results.dir,file.name,sep="/"))

