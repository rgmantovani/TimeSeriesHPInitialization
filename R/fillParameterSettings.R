
# -----------------------------------------------------------------------------
# Function definitions
# -----------------------------------------------------------------------------

# fill the parameter settings to the result.matrix
# arg[1] = the index of the row the settings are filled to

fillParamsDrp = function(result.matrix, args) {

  switch(args[1],
    pso  = { result.matrix[, "BL.alg"] = 1}, 
    rs   = { result.matrix[, "BL.alg"] = 2}, 
    dfs  = { result.matrix[, "BL.alg"] = 3}, 
    smbo = { result.matrix[, "BL.alg"] = 4}, 
           { result.matrix[, "BL.alg"] = 5}) 

  result.matrix[, "EXP.folds"] = as.numeric(args[2])

  if(args[3] == "svm") {
    result.matrix[, "algo"] = 1 #svm
  } else {
    result.matrix[, "algo"] = 2 #J48
  }

  return(result.matrix)
}

# -----------------------------------------------------------------------------
# Function definitions
# -----------------------------------------------------------------------------

# fill the parameter settings to the result.matrix
# arg[1] = the index of the row the settings are filled to
fillParamsDtw = function(result.matrix, args) {
  
  if (args[1] == "in") {
    result.matrix[ ,"PCA.in.ex"] = 1
  } else {
    result.matrix[ ,"PCA.in.ex"] = 2
  }
  
  result.matrix[ ,"PCA.gamma"] = as.numeric(args[2])
  
  if (args[3] == "rv") {
    result.matrix[ ,"DTW.MF.rv.nv"] = 1
  } else {
    result.matrix[ ,"DTW.MF.rv.nv"] = 2 
  }

  result.matrix[ ,"NN.k"] = as.numeric(args[4])
    
  switch(args[5],
    pso  = { result.matrix[, "BL.alg"] = 1}, 
    rs   = { result.matrix[, "BL.alg"] = 2}, 
    dfs  = { result.matrix[, "BL.alg"] = 3}, 
    smbo = { result.matrix[, "BL.alg"] = 4}, 
           { result.matrix[, "BL.alg"] = 5}) 

  result.matrix[ ,"EXP.folds"] = as.numeric(args[6])
  return(result.matrix)
}

# -----------------------------------------------------------------------------
# Function definitions
# -----------------------------------------------------------------------------

# fill the parameter settings to the result.matrix
# arg[1] = the index of the row the settings are filled to
fillParamsPqh = function(result.matrix, args) {

  if (args[1] == "in") {
    result.matrix[ ,"PCA.in.ex"] = 1
  } else {
    result.matrix[ ,"PCA.in.ex"] = 2
  }

  result.matrix[ , "PCA.gamma"] = as.numeric(args[2])
  
  if (args[3] == "qu") {
    result.matrix[ ,"PCA.qu.hi"] = 1
  } else {
    result.matrix[ ,"PCA.qu.hi"] = 2  
  }

  result.matrix[ , "PCA.bins"] = as.numeric(args[4])

  switch(args[5],
    ed = { result.matrix[, "MF.dist"] = 1 },
    ip = { result.matrix[, "MF.dist"] = 2 },
    cs = { result.matrix[, "MF.dist"] = 3 },
         { result.matrix[, "MF.dist"] = 4 })

  switch(args[6], 
    pso  = { result.matrix[, "BL.alg"] = 1 },
    rs   = { result.matrix[, "BL.alg"] = 2 },
    dfs  = { result.matrix[, "BL.alg"] = 3 },
    smbo = { result.matrix[, "BL.alg"] = 4 },
           { result.matrix[, "BL.alg"] = 5 })


  result.matrix[ , "EXP.folds"] = as.numeric(args[7])

  if (args[8] == "rv") {
    result.matrix[ ,"DTW.MF.rv.nv"] = 1
  } else {
    result.matrix[ ,"DTW.MF.rv.nv"] = 2
  }

  if(args[9] == "svm") {
    result.matrix[, "algo"] = 1 #svm
  } else {
    result.matrix[, "algo"] = 2 #J48
  }

  return(result.matrix)
}

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# fill the parameter settings to the result.matrix to the experiments with 
# meta-features group

fillParamsMfg = function(result.matrix, args) {

  result.matrix[ ,"MF.group.comb"] = as.numeric(args[1])
  
  if (args[2] == "rv") {
    result.matrix[ ,"DTW.MF.rv.nv"] = 1
  } else {
    result.matrix[ ,"DTW.MF.rv.nv"] = 2
  }
    
  switch(args[3],
    ed = { result.matrix[, "MF.dist"] = 1 },
    ip = { result.matrix[, "MF.dist"] = 2 },
    cs = { result.matrix[, "MF.dist"] = 3 },
         { result.matrix[, "MF.dist"] = 4 })
    
  result.matrix[ ,"NN.k"] = as.numeric(args[4])

  switch(args[5], 
    pso  = { result.matrix[, "BL.alg"] = 1 },
    rs   = { result.matrix[, "BL.alg"] = 2 },
    dfs  = { result.matrix[, "BL.alg"] = 3 },
    smbo = { result.matrix[, "BL.alg"] = 4 },
           { result.matrix[, "BL.alg"] = 5 })

  result.matrix[ ,"EXP.folds"] = as.numeric(args[6])
  return(result.matrix)
}

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------