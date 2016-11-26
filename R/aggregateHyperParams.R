#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

aggregateHyperParams = function(params, algo) {

  if(algo == "svm")
    return(.aggregateSVMParams(params = params))
  else
    return(.aggregateDTParams(params = params))
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

.aggregateSVMParams = function(params) {

  aux = lapply(1:nrow(params[[1]]), function(i) {
    inner.aux = lapply(params, function(elem) {
      return(elem[i, ])
    })
    hp = do.call("rbind", inner.aux)
    return(colMeans(hp))
  })

  hps = do.call("rbind", aux)
  colnames(hps) = paste0(colnames(params[[1]]),".response")
  return(hps)

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

.aggregateDTParams = function(params) {

  aux = lapply(1:nrow(params[[1]]), function(i) {
    
    inner.aux = lapply(params, function(elem) {
      return(elem[i, ])
    })

    hp = do.call("rbind", inner.aux)

    inner.aux.2 = lapply(1:ncol(hp), function(j) {
      aux = table(hp[,j], exclude = NULL)
      switch(class(hp[,j]), 
        numeric = { 
          if(is.na(names(aux)[which.max(aux)])) {
            value = NA
          } else {
            value = mean(na.omit(hp[,j]))
          }
        },
        integer = { 
          if(is.na(names(aux)[which.max(aux)])) {
            value = NA
          } else if(aux[which.max(aux)] == 1) {
            value = round(mean(na.omit(hp[,j])))
          } else {
            value = aux[which.max(aux)]
          }
        },
        logical = { value = names(aux)[which.max(aux)]}
      )
      return(value)
    })
    return(unlist(inner.aux.2))    
  })

  hps = data.frame(do.call("rbind", aux))
  hps[,1] = as.numeric(as.character(hps[,1]))
  hps[,2] = as.integer(as.character(hps[,2]))
  hps[,3] = as.integer(as.character(hps[,3]))
  for(k in 4:9){
    hps[,k] = as.logical(hps[,k])
  }
  
  colnames(hps) = paste0(colnames(params[[1]]),".response") 
  new.hps = checkParams(settings = hps, algo = "J48")
  
  return(new.hps)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
