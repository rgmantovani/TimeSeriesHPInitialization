#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getAggValueDF = function(files, label, measure = "mean") {

  # no file, no data frame
  if(length(files) == 0) {
    return(data.frame())
  }

  if(measure == "mean") {
    aux = lapply(files, getAvgAcc)
  } else if(measure == "max") {
    aux = lapply(files, getMaxAcc)
  } else if(measure == "feat.time") {
    aux = lapply(files, getAvgFeatTime)
  } else {
    stop("Invalid measure!")
  }

  tmp = lapply(aux, function(elem) {
    return(data.frame(t(elem)))
  })

  tmp2 = data.frame(t(plyr::rbind.fill(tmp)))
  df = data.frame(rowMeans(tmp2))
  colnames(df) = label
  return(df)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getAvgAcc = function(file) {
  
  obj = dget(file)
  ret = data.frame(obj[, "mean.acc"]) 
  return(ret)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getMaxAcc = function(file) {
  
  obj = dget(file)
  ret = data.frame(apply(obj[,1:30], 1, max)) 
  return(ret)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getAvgFeatTime = function(file) {
  obj = dget(file)
  ret = data.frame(obj[, "time.FE"])
  return(ret)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------