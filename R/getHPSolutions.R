#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

# Returns the best hp solutions for each dataset in all repetitions
# args          
# dataset.names = datasets to be considered
# hp.technique  = the tuning method according to which the best HP values are delivered (rs, pso, df)
# algo          = algorithm to be analyzed (svm or j48)

getHPSolutions = function(datasets, hp.technique, algo) {

  assertChoice(x = hp.technique , choices = c("rs", "df", "pso", "smbo"))
  assertChoice(x = algo, choices = c("svm", "J48"), .var.name = "algo")
  
  aux = lapply(datasets, function(datafile) {
   
    directory = paste0(hp.dir, datafile)
    inner.files = list.files(path = directory)

    inner.aux = lapply(inner.files, function(file) {

      hp.file = paste0(directory, "/" ,file)
      ret = dget(file = hp.file, keep.source = TRUE)
  
      sel.ids = grepl(pattern = toupper(hp.technique), x = colnames(ret))
      if(!any(sel.ids)) {
        stop(paste0("There are no results for ", hp.technique, " technique!"))  
      }    
      return(ret[nrow(ret), sel.ids])
    })
   
    inner.aux = do.call("rbind", inner.aux)

    # Returning just the hyper-parameter
    if(algo == "svm") {
      return(inner.aux[, 1:2])
    } else {
      inner.aux[, 2] = as.integer(inner.aux[,2])
      inner.aux[, 3] = as.integer(inner.aux[,3])
      return(inner.aux[, 1:9])
    }
  })

  return(aux)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
