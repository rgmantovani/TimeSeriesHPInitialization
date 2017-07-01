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

checkDTParams = function(settings) {
 
  R.id = which(grepl(pattern = "R.response", x = colnames(settings)))
  C.id = which(grepl(pattern = "C.response", x = colnames(settings)))
  N.id = which(grepl(pattern = "N.response", x = colnames(settings)))
  M.id = which(grepl(pattern = "M.response", x = colnames(settings)))

  settings[, C.id] = as.numeric(as.character(settings[, C.id]))

  ids =  which(grepl(pattern = ".response", x = colnames(settings)))
  bol.ids = !(ids %in% c(N.id, M.id, C.id))
  for(id in ids[bol.ids]) {
    settings[, id] = as.logical(settings[, id])
  }

  # R (use reduce error pruning)
  for(i in 1:nrow(settings)) {
   
    if(settings[i, R.id] == FALSE) {
 
      # if R = FALSE, C (pruning confidence) [0.001, 0.49], N not used
      settings[i, N.id] = NA 
      if(is.na(settings[i, C.id])) {
        settings[i, C.id] = 0.25
      } else if(settings[i, C.id] < 0.001) {
        settings[i, C.id] = 0.001
      } else if(settings[i, C.id] > 0.49) {
        settings[i, C.id] = 0.49
      }
 
    } else {
      # if R = TRUE, N (number of folds reduced) [2, 10], C not used, N default = 3
        settings[i, C.id] = NA 
      if(is.na(settings[i, N.id]) | settings[i, N.id] == "New.level") {
        settings[i, N.id] = 3
      } 
    }
  }

  settings[, N.id] = as.integer(as.character(settings[, N.id]))
  settings[, M.id] = as.integer(as.character(settings[, M.id]))

  invalid.ids = which(settings[, N.id] < 2 | settings[,N.id] > 10)
  if(length(invalid.ids) > 0) {
    warning(" - Predicted \'N\' values out of the range\n")
    settings[invalid.ids, N.id] = 3
  } 

  settings$set = NULL
  settings$iter = NULL

  return(settings)
}
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
