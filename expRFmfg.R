#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

# Parameters:
# args[1] = {1, 2, ..., 256} // meta-feature group combination corresponding to
#           {"sl", "st", "it", "lm", "mb", "ti", "dc", "cn"}
# args[2] = {"rv", "nv"}  // real or normalized meta-feature vectors
# args[3] = {"ed", "ip", "cs", "pc"} // euclidean dist., inner prod., cosine sim., pearson corr.
# args[4] = {1,2,3} // k for nearest neighbors
# args[5] = {"pso", "rs", "dfs"}  // PSO, RS, DF (w.r.t. SVM)
# args[6] = {3,5,10} // folds for cross-validation (will try only 5)
# args[7] = {"svm", "J48"} // algorithm which hyper-parameters are predicted

# args <- commandArgs(TRUE)

args = c("255", "rv", "ed", "1", "rs", "3", "J48")
# args = c("255", "rv", "ed", "1", "rs", "3", "svm")
ALGO = args[7]
cat(" @Algorithm: ", ALGO, "\n")

cat("Sourcering files ... \n")
my.files = list.files(path = "R", full.names = TRUE)
for(file in my.files) {
  source(file)
  cat(" - file: ", file, "\n")
}

meta.feature.groups = convert.mf.group.combination.to.vector(as.numeric(args[1]))
computation.times = matrix(0, nrow = length(COMMON.DATA), ncol = 1)
dimnames(computation.times) = list(COMMON.DATA, NULL)

ret = getComputationTimes(meta.feature.groups = meta.feature.groups, obj = obj)
sel.ids = which(rownames(ret$feature.matrix) %in% COMMON.DATA)

# normalize the feature matrix if required
if (args[2] == "nv") {
  cat(" - Scaling features.\n")
  feature.matrix = scale(ret$feature.matrix[sel.ids, ])
} else {
  cat(" - Using normal features.\n")
  feature.matrix = ret$feature.matrix[sel.ids, ]
}
result.matrix[, "time.FE"] = ret$computation.times[sel.ids]
result.matrix[, "MTL.algo"] = 2 # 1 = k-NN, 2 = RF
result.matrix = fillParamsMfg(result.matrix = result.matrix, args = args)

# Reading HP-solutions previously found
hp.solutions = getHPSolutions(dataset.names = dataset.names, hp.technique = args[5], algo = ALGO)

outer.aux = lapply(1:30, function(rep.id) {

  cat(" @ Repetition: ", rep.id, " ... \n")
  targets = do.call("rbind", lapply(hp.solutions, function(pos) return(pos[rep.id, ])))
  
  start.time = System$currentTimeMillis()
  hp.predicted = multitargetRF(feature.matrix = feature.matrix, targets = targets)
  model.time = System$currentTimeMillis() - start.time

  inner.aux = lapply(1:length(datafile.names), function(i) {

    datafile = datafile.names[i]
   
    # Getting predicted parameters and formating them to be a list
    hp.setting = hp.predicted[i, ]
    response = hp.setting[grepl(pattern = "response", x = colnames(hp.setting))]
    colnames(response) = gsub(x = colnames(response), 
      pattern = paste0(toupper(args[5]), "\\.|\\.response"), replacement = "")
    params = as.list(response)
   
    inner.time = System$currentTimeMillis()

    if(ALGO == "svm") {
      perf = runBaseLearner(datafile = datafile, algo = "svm", params = params, 
        folds = as.numeric(args[6]), trafo = function(x) return(2^x))
    } else if(ALGO == "J48") {
      perf = runBaseLearner(datafile = datafile, algo = "J48", params = params, 
        folds = as.numeric(args[6]))
    } else {
      stop("Invalid base learner!")
    }
 
    # QUESTION: Should I include the model time ? 
    pred.time = System$currentTimeMillis() - inner.time + model.time
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

# save the result.matrix to the disk
file.name = paste(paste("expRFMfg",paste("-", args, sep="", collapse=""),sep=""), "rda", sep=".")
dput(result.matrix, file = paste(results.dir,file.name,sep="/"))

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------