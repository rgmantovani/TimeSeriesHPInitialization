# Parameters:
# args[1] = {"pso", "rs", "df"}  // PSO, RS, DF (w.r.t. SVM)
# args[2] = {3,5,10} // folds for cross-validation (will try only 5)
# args[3] = {"svm", "J48"}
# number of experiments = 3 * 1 * 2 = 6

# -----------------------------------------------------------------------------
# Main program
# -----------------------------------------------------------------------------

args = commandArgs(TRUE)
# args = c("df", 2, "svm") #J48
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

    if(ALGO == "svm") {
      trafo = function(x) return(2^x)
    } else {
      trafo = NULL
    }
    perf = runBaseLearner(datafile = datafile, algo = ALGO, 
      params = params, folds = as.numeric(args[2]), trafo = trafo)

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

