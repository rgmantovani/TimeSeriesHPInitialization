#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

# Check if a HP setting is predicted correctly, if not modify the values to be correct according 
# to the params type

checkParams = function(settings, algo) {

  row.na = which(unlist(lapply(1:nrow(settings), function(i) return(all(is.na(settings[i,]))))))

  if(length(row.na) > 0) {
    warning(" - There are missing solutions\n.")
    settings[row.na, ] = settings[1, ]
  }

  if(algo == "J48"){
    return(checkDTParams(settings = settings))
  } else {
    return(settings)
  }
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

# TODO: define par.set (using ParamHelpers)?
checkDTParams = function(settings) {
 
  R.id = which(grepl(pattern = ".using.reduced.error.pruning.response", x = colnames(settings)))
  C.id = which(grepl(pattern = ".pruning.confidence.response", x = colnames(settings)))
  N.id = which(grepl(pattern = ".n.folds.error.response", x = colnames(settings)))
  M.id = which(grepl(pattern = ".min.n.instances.response", x = colnames(settings)))

  # R (use reduce error pruning)
  for(i in 1:nrow(settings)) {
   
    if(settings[i, R.id] == FALSE) {
 
      # if R = FALSE, C (pruning confidence) [0.001, 0.49], N not used
      settings[i, N.id] = NA 
      if(settings[i, C.id] < 0.001) {
        settings[i, C.id] = 0.001
      } else if(settings[i, C.id] > 0.49) {
        settings[i, C.id] = 0.49
      }
 
    } else {
      # if R = TRUE, N (number of folds reduced) [2, 10], C not used, N default = 3
       settings[i, C.id] = NA 
      if(settings[i, N.id] == "New.level") {
        settings[i, N.id] = 3
      }
    }
  }

  settings = .renameParams(settings = settings)
  settings[, M.id] = as.numeric(settings[, M.id])
  settings[, N.id] = as.numeric(settings[, N.id])

  invalid.ids = which(settings[, N.id] < 2 | settings[,N.id] > 10)
  if(length(invalid.ids) > 0) {
    warning(" - Predicted \'N\' values out of the range\n")
    settings[invalid.ids, N.id] = 3
  } 

  ids =  which(grepl(pattern = ".response", x = colnames(settings)))
  bol.ids = !(ids %in% c(N.id, M.id, C.id))
  for(id in ids[bol.ids]) {
    settings[, id] = as.logical(settings[, id])
  }

  settings$set = NULL
  settings$iter = NULL
  return(settings)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

.renameParams = function(settings) {

  aux.str = colnames(settings)
  aux.str = gsub(x = aux.str, pattern = "min.n.instances", replacement = "M")
  aux.str = gsub(x = aux.str, pattern = "pruning.confidence", replacement = "C")
  aux.str = gsub(x = aux.str, pattern = "n.folds.error", replacement = "N")
  aux.str = gsub(x = aux.str, pattern = "not.collapse.tree", replacement = "O")
  aux.str = gsub(x = aux.str, pattern = "using.reduced.error.pruning", replacement = "R")
  aux.str = gsub(x = aux.str, pattern = "using.only.binary.split", replacement = "B")
  aux.str = gsub(x = aux.str, pattern = "not.doing.subtree.raising", replacement = "S")
  aux.str = gsub(x = aux.str, pattern = "using.laplace.smoth", replacement = "A")
  aux.str = gsub(x = aux.str, pattern = "not.using.MDL.correction", replacement = "J")
  colnames(settings) = aux.str
  return(settings)

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
