
# -----------------------------------------------------------------------------
# Function definitions
# -----------------------------------------------------------------------------

# fill the parameter settings to the result.matrix
fillParamsDrp = function(result.matrix, args) {

  switch(args[1],
    pso  = { result.matrix[, "BL.alg"] = 1}, 
    rs   = { result.matrix[, "BL.alg"] = 2}, 
    dfs  = { result.matrix[, "BL.alg"] = 3}, 
    smbo = { result.matrix[, "BL.alg"] = 4}, 
           { result.matrix[, "BL.alg"] = 5}) 

  result.matrix[, "EXP.folds"] = as.numeric(args[2])
  result.matrix[ ,"algo"] = if(args[3] == "svm") 1 else 2

  return(result.matrix)
}

# -----------------------------------------------------------------------------
# Function definitions
# -----------------------------------------------------------------------------

# fill the parameter settings to the result.matrix
fillParamsDtw = function(result.matrix, args) {
  
  result.matrix[ , "PCA.in.ex"] = if(args[1] == "in") 1 else 2
  result.matrix[ , "PCA.gamma"] = as.numeric(args[2])
  result.matrix[ , "DTW.MF.rv.nv"] = if(args[3] == "rv") 1 else 2
  result.matrix[ , "NN.k"] = as.numeric(args[4])
    
  switch(args[5],
    pso  = { result.matrix[, "BL.alg"] = 1}, 
    rs   = { result.matrix[, "BL.alg"] = 2}, 
    dfs  = { result.matrix[, "BL.alg"] = 3}, 
    smbo = { result.matrix[, "BL.alg"] = 4}, 
           { result.matrix[, "BL.alg"] = 5}) 

  result.matrix[ , "EXP.folds"] = as.numeric(args[6])
  result.matrix[ , "algo"] = if(args[7] == "svm") 1 else 2
  result.matrix[ , "MTL.alg"] = 1 # 1 = k-NN, 2 = RF

  return(result.matrix)
}

# -----------------------------------------------------------------------------
# Function definitions
# -----------------------------------------------------------------------------

# fill the parameter settings to the result.matrix
fillParamsPqhRF = function(result.matrix, args) {

  result.matrix[ , "PCA.in.ex"] = if(args[1] == "in") 1 else 2
  result.matrix[ , "PCA.gamma"] = as.numeric(args[2])
  result.matrix[ , "PCA.qu.hi"] = if(args[3] == "qu") 1 else 2
  result.matrix[ , "PCA.bins"] = as.numeric(args[4])

  switch(args[5], 
    pso  = { result.matrix[, "BL.alg"] = 1 },
    rs   = { result.matrix[, "BL.alg"] = 2 },
    dfs  = { result.matrix[, "BL.alg"] = 3 },
    smbo = { result.matrix[, "BL.alg"] = 4 },
           { result.matrix[, "BL.alg"] = 5 })

  result.matrix[ , "EXP.folds"] = as.numeric(args[6])
  result.matrix[ , "DTW.MF.rv.nv"] = if(args[7] == "rv") 1 else 2
  result.matrix[ , "algo"] = if(args[8] == "svm") 1 else 2
  result.matrix[ , "MTL.alg"] = 2 # 1 = k-NN, 2 = RF

  return(result.matrix)
}

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# for KNN
fillParamsPqhKNN = function(result.matrix, args) {

  result.matrix[ , "PCA.in.ex"] = if(args[1] == "in") 1 else 2
  result.matrix[ , "PCA.gamma"] = as.numeric(args[2])
  result.matrix[ , "PCA.qu.hi"] = if(args[3] == "qu") 1 else 2
  result.matrix[ , "PCA.bins"] = as.numeric(args[4])

  switch(args[5], 
    ed  = { result.matrix[ , "MF.dist"] = 1 },
    ip  = { result.matrix[ , "MF.dist"] = 2 },
    cs  = { result.matrix[ , "MF.dist"] = 3 },
          { result.matrix[ , "MF.dist"] = 4 })

  result.matrix[ , "NN.k"] = as.numeric(args[6])

  switch(args[7], 
    pso  = { result.matrix[, "BL.alg"] = 1 },
    rs   = { result.matrix[, "BL.alg"] = 2 },
    dfs  = { result.matrix[, "BL.alg"] = 3 },
    smbo = { result.matrix[, "BL.alg"] = 4 },
           { result.matrix[, "BL.alg"] = 5 })

  result.matrix[ , "EXP.folds"] = as.numeric(args[8])
  result.matrix[ , "DTW.MF.rv.nv"] = if(args[9] == "rv") 1 else 2
  result.matrix[ , "algo"] = if(args[10] == "svm") 1 else 2
  result.matrix[ , "MTL.alg"] = 1 # 1 = k-NN, 2 = RF

  return(result.matrix)
}

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# fill the parameter settings to the result.matrix to the experiments with 
# meta-features group
fillParamsMfgRF = function(result.matrix, args) {

  result.matrix[ ,"MF.group.comb"] = as.numeric(args[1])
  result.matrix[ ,"DTW.MF.rv.nv"] = if(args[2] == "rv") 1 else 2
  
  switch(args[3], 
    pso  = { result.matrix[, "BL.alg"] = 1 },
    rs   = { result.matrix[, "BL.alg"] = 2 },
    dfs  = { result.matrix[, "BL.alg"] = 3 },
    smbo = { result.matrix[, "BL.alg"] = 4 },
           { result.matrix[, "BL.alg"] = 5 })

  result.matrix[ ,"EXP.folds"] = as.numeric(args[4])
  result.matrix[ ,"algo"] = if(args[5] == "svm") 1 else 2
  result.matrix[, "MTL.alg"] = 2 # 1 = k-NN, 2 = RF

  return(result.matrix)
}

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# for KNN
fillParamsMfgKNN = function(result.matrix, args) {

  result.matrix[ ,"MF.group.comb"] = as.numeric(args[1])
  result.matrix[ ,"DTW.MF.rv.nv"] = if(args[2] == "rv") 1 else 2
  
  switch(args[3], 
    ed  = { result.matrix[, "MF.dist"] = 1 },
    ip  = { result.matrix[, "MF.dist"] = 2 },
    cs  = { result.matrix[, "MF.dist"] = 3 },
          { result.matrix[, "MF.dist"] = 4 })

  result.matrix[ ,"NN.k"] = as.numeric(args[4])

  switch(args[5], 
    pso  = { result.matrix[, "BL.alg"] = 1 },
    rs   = { result.matrix[, "BL.alg"] = 2 },
    dfs  = { result.matrix[, "BL.alg"] = 3 },
    smbo = { result.matrix[, "BL.alg"] = 4 },
           { result.matrix[, "BL.alg"] = 5 })

  result.matrix[ ,"EXP.folds"] = as.numeric(args[6])
  result.matrix[ ,"algo"] = if(args[7] == "svm") 1 else 2
  result.matrix[, "MTL.alg"] = 1 # 1 = k-NN, 2 = RF
  
  return(result.matrix)
}

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------