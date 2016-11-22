#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getComputationTimes = function(meta.feature.groups, obj) {
	
  computation.times = matrix(0, nrow = length(rownames(obj$simple)[,1]), ncol = 1)
  dimnames(computation.times) = list(rownames(obj$simple), NULL)
  feature.matrix = NULL

  if (meta.feature.groups[1]) {
    feature.matrix = cbind(feature.matrix, obj$simple)
    computation.times[,1] = computation.times[,1] + obj$subGroupTimes[,"simple_time"]
  }
  if (meta.feature.groups[2]) {
    feature.matrix = cbind(feature.matrix,obj$statistical)
    computation.times[,1] = computation.times[,1] + obj$subGroupTimes[,"statistical_time"]
  }
  if (meta.feature.groups[3]) {
    feature.matrix = cbind(feature.matrix,obj$inftheo)
    computation.times[,1] = computation.times[,1] + obj$subGroupTimes[,"inftheo_time"]
  }
  if (meta.feature.groups[4]) {
    feature.matrix = cbind(feature.matrix,obj$landmarking)
    computation.times[,1] = computation.times[,1] + obj$timeFeat[,"naive_bayes_time"] +
      obj$timeFeat[,"lda_time"] + obj$timeFeat[,"stump_time"] +
      obj$timeFeat[,"nn_time"]
  }
  if (meta.feature.groups[5]) {
    feature.matrix = cbind(feature.matrix,obj$modelbased)
    computation.times[,1] = computation.times[,1] + obj$timeFeat[,"tree_time"]
  }
  if (meta.feature.groups[6]) {
    feature.matrix = cbind(feature.matrix,obj$timeFeat)
    
    time.to.add = matrix(0, nrow = length(rownames(obj$simple)[,1]), ncol = 1) 
    if (meta.feature.groups[4] == FALSE) {
      time.to.add[,1] = time.to.add[,1] + obj$timeFeat[,"naive_bayes_time"] +
        obj$timeFeat[,"lda_time"] + obj$timeFeat[,"stump_time"] +
        obj$timeFeat[,"nn_time"]
    }
    if (meta.feature.groups[5] == FALSE) {
      time.to.add[,1] = time.to.add[,1] + obj$timeFeat[,"tree_time"]
    }
    
    computation.times[,1] = computation.times[,1] + time.to.add[,1]
  }
  if (meta.feature.groups[7]) {
    feature.matrix = cbind(feature.matrix,as.matrix(obj$dataComplex))
    computation.times[,1] = computation.times[,1] + obj$subGroupTimes[,"dcol_time"]
  }
  if (meta.feature.groups[8]) {
    feature.matrix = cbind(feature.matrix,obj$complexNet)
    computation.times[,1] = computation.times[,1]   + obj$subGroupTimes[,"cnet_time"]
  }

  ret = list(computation.times = computation.times, feature.matrix = feature.matrix)
  return(ret)

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
