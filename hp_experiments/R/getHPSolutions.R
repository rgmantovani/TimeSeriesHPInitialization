#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

# Returns the best hp solutions for each dataset in all repetitions
# args          
# dataset.names = datasets to be considered
# hp.technique  = the tuning method according to which the best HP values are delivered (rs, pso, df)
# algo          = algorithm to be analyzed (svm or j48)

getHPSolutions = function(datasets, hp.technique, algo, dirs) {

  assertChoice(x = hp.technique , choices = c("rs", "df", "pso", "smbo"))
  assertChoice(x = algo, choices = c("svm", "J48"), .var.name = "algo")

  aux = lapply(datasets, function(datafile) {
    hp.file = paste0(dirs$hp.dir, "/", datafile, ".RData")
    load(hp.file, verbose = FALSE)
    return(ret[, 1:(ncol(ret)-1)])
  })

  return(aux)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

