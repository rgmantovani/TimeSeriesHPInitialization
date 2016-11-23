#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
# Parameters:
# args[1] = {"pso", "rs", "df", "smbo"}  // PSO, RS, DF (w.r.t. SVM)
# args[2] = {3,5,10} // folds for cross-validation (will try only 5)
# args[3] = {"svm", "J48"}
# number of experiments = 3 * 1 * 2 = 6

#--------------------------------------------------------------------------------------------------
# Main program
#--------------------------------------------------------------------------------------------------

args = commandArgs(TRUE)
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
  
  datafile = datafile.names[i]
  cat(i,"/", length(datafile.names), "-", datafile,"\n")
  
  settings = getHPSolutions(datasets = dataset.names[i], 
    hp.technique = args[1], algo = args[3])[[1]]

  colnames(settings) = paste(colnames(settings), "response", sep=".")  
  aggregated.hp = checkParams(settings = settings, algo = args[3])

  # compute results of SVM using cross-validation
  cat("/")
  aux = lapply(1:30, function(j) {

    cat("=")
    set.seed(j) # seed = rep id
    response = aggregated.hp[j, ]
    names(response) = gsub(x = names(response), 
      pattern = paste0(toupper(args[1]), "\\.|\\.response"), replacement = "")
    params = as.list(response)
    
    inner.time = System$currentTimeMillis()

    if(ALGO == "svm") {
      trafo = function(x) return(2^x)
    } else {
      trafo = NULL
    }
    perf = runBaseLearner(datafile = datafile, algo = ALGO, 
      params = params, folds = as.numeric(args[2]), trafo = trafo)

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
dput(result.matrix, file = paste(results.dir,file.name,sep="/"))

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
