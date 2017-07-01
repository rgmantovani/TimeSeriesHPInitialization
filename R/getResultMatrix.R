#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getResultMatrix = function(dirs, algo, tuning) {

  if(!dir.exists("output")) {
    dir.create(path = "output")
    cat(" - Creating output folder\n")
  }

  # loading meta-features
  load(file ="data/metaFeatures.RData", verbose = FALSE)
  obj$landmarking = obj$landmarking[,-10]

  res.ds = gsub(x = list.files(path = dirs$data.dir), pattern = ".arff", replacement = "")
  res.hp = gsub(x = list.files(path = dirs$hp.dir), pattern = ".RData", replacement = "")
  res.mf = rownames(obj$simple)

  # Intersection of data
  COMMON.DATA = Reduce(intersect, list(res.ds, res.mf, res.hp))

  if(length(COMMON.DATA) <= 0) {
    stop("No datasets remained for experiments!")
  } else {
    cat(" - INFO: A total of", length(COMMON.DATA) , "datasets will be used in experiments.\n")
  }

  dataset.names = COMMON.DATA
  datafile.names = paste0(dataset.names, ".arff")

  result.matrix.column.names = c(paste(c("acc"), 1:30, sep = "."), 
    "mean.acc", "PCA.in.ex", "PCA.gamma", "PCA.qu.hi", "PCA.bins", "MF.group.comb", "MF.dist", 
    "DTW.MF.rv.nv", "BL.alg", "MTL.alg", "algo", "EXP.folds", "NN.k", "time.FE", "time.dist", 
    "time.comp", "time.RF")

  result.matrix = matrix(, nrow = length(dataset.names), 
    ncol = length(result.matrix.column.names))
  dimnames(result.matrix) = list(dataset.names, result.matrix.column.names)

  # subset meta-features here (in setup)
  sel.ids = which(rownames(obj$simple) %in% COMMON.DATA)
  obj = lapply(obj, function(elem) {
    return(elem[sel.ids,])
  })
  
  ret = list(mat = result.matrix, obj = obj, dataset.names = dataset.names, 
    datafile.names = datafile.names)

  return(ret)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
