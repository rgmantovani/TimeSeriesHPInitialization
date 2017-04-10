#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

library("mlr")
library("foreign")
library("kernlab")
library("dtw")
library("e1071")
library("R.utils")
library("checkmate")
library("randomForest")

REPETITIONS = 30
# REPETITIONS = 3 (debug)

configureMlr(show.info = FALSE)

if(!dir.exists("output")) {
  dir.create(path = "output")
  cat(" - Creating output folder\n")
}

results.dir = paste(getwd(), "output", sep = "/")
data.dir = paste(getwd(), "input/datasets", sep = "/")
hp.dir = paste0(getwd(), "/input/", ALGO, "_hp/")
hp.dir.smbo = paste0(getwd(), "/input/svm_smbo_900/")

# loading meta-features
load(file = paste(getwd(), "input/metaFeatures.RData", sep = "/"), verbose = TRUE)
obj$landmarking = obj$landmarking[,-10]

res.ds = gsub(x = list.files(path = data.dir), pattern = ".arff", replacement = "")
res.hp = list.files(path = hp.dir)
res.hp.smbo = list.files(path = hp.dir.smbo)
res.mf = rownames(obj$simple)

# Intersection - hp + ds + mf + smbo_results
COMMON.DATA = intersect(
  x = intersect(
    x = intersect(
      x = res.ds, 
      y = res.mf), 
    y = res.hp), 
  y = res.hp.smbo)

if(length(COMMON.DATA) <= 0) {
  stop("No datasets remained for experiments!")
} else {
  cat("it's ok.\n")
}

dataset.names = COMMON.DATA
datafile.names = paste0(dataset.names, ".arff")

result.matrix.column.names = c(paste(c("acc"), 1:30, sep = "."), 
  "mean.acc", "PCA.in.ex", "PCA.gamma", "PCA.qu.hi", "PCA.bins", "MF.group.comb", "MF.dist", 
  "DTW.MF.rv.nv", "BL.alg", "MTL.alg", "algo", "EXP.folds", "NN.k", "time.FE", "time.dist", 
  "time.comp", "time.RF")
result.matrix <<- matrix(, nrow = length(dataset.names), 
  ncol = length(result.matrix.column.names))
dimnames(result.matrix) = list(dataset.names, result.matrix.column.names)

# subset meta-features here (in setup)
sel.ids = which(rownames(obj$simple) %in% COMMON.DATA)
obj = lapply(obj, function(elem) {
  return(elem[sel.ids,])
})

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
