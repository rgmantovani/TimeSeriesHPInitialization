#--------------------------------------------------------------------------------------------------
# Command line parameters:
#--------------------------------------------------------------------------------------------------

# Parameters:
# args[1] = {1, 2, ..., 256} // meta-feature group combination corresponding to
#           {"sl", "st", "it", "lm", "mb", "ti", "dc", "cn"}
# args[2] = {"rv", "nv"}  // real or normalized meta-feature vectors
# args[3] = {"pso", "rs", "df", "smbo"}  // PSO, RS, DF SMBO
# args[4] = {3,5,10} // folds for cross-validation (will try only 5)
# args[5] = {"svm", "J48"} // algorithm which hyper-parameters are predicted

#--------------------------------------------------------------------------------------------------
# Fucntion definitions
#--------------------------------------------------------------------------------------------------

args = c(1, "rv", "smbo", 3, "J48")
# args = commandArgs(TRUE)

GROUPS    = as.numeric(args[1])
RV.NV     = args[2]
HP.TUNING = args[3]
FOLDS     = as.numeric(args[4])
ALGO      = args[5]

cat(" ========================================== \n")
cat(" * Running expRFmfg with following parameters: \n")
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

meta.feature.groups = convert.mf.group.combination.to.vector(number = GROUPS)
ret = getComputationTimes(meta.feature.groups = meta.feature.groups, obj = obj)

# normalize the feature matrix if required
if (RV.NV == "nv") {
  cat(" - Scaling features.\n")
  feature.matrix = scale(ret$feature.matrix)
} else {
  cat(" - Using normal features.\n")
  feature.matrix = ret$feature.matrix
}
result.matrix[, "time.FE"] = ret$computation.times

cat(" @ Retrieving HP solutions \n")
result.matrix = fillParamsMfgRF(result.matrix = result.matrix, args = args)
hp.solutions = getHPSolutions(datasets = dataset.names, hp.technique = HP.TUNING, 
  algo = ALGO, dirs = dirs)

outer.aux = lapply(1:REPETITIONS, function(rep.id) {
  
  set.seed(seed = rep.id)
  cat(" @ Repetition: ", rep.id, " ... \n")
 
  targets = do.call("rbind", lapply(hp.solutions, function(pos) return(pos[rep.id, ])))
  hp.predicted = multitargetRF(feature.matrix = feature.matrix, targets = targets)
 
  inner.aux = lapply(1:length(datafile.names), function(i) {

    datafile = datafile.names[i]
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

result.matrix[, "time.comp"] = rowMeans(times)

colnames(accs) = paste0("acc.", 1:ncol(accs))
result.matrix[ , 1:ncol(accs)] = accs
result.matrix[, "mean.acc"] = rowMeans(accs)

cat(" - Saving results \n")
file.name = paste(paste("expRFMfg",paste("-", args, sep="", collapse=""),sep=""), "rda", sep=".")
dput(result.matrix, file = paste(dirs$results.dir,file.name,sep="/"))

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------