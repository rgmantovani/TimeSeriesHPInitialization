#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

# perform cross-validation and compute average balanced accuracy on folds

# arg[1] datafile  = the name of the data file
# arg[2] algo      = the name of algo to be evaluated (svm or J48)
# arg[3] datafile  = the name of the data file
# arg[4] folds     = the number of folds for cross-validation
# arg[5] datafile  = the name of the data file


runBaseLearner = function(datafile, algo, params, folds = 5, trafo = NULL) {

  assertChoice(x = algo, choices = c("svm", "J48"), .var.name = "algo")
 
  data = read.arff(paste(data.dir, datafile, sep="/"))
  task = makeClassifTask(id = datafile, data = data, target = "Class")
  
  rdesc = makeResampleDesc(method = "CV", iter = folds)

  # TODO: handle imputation ? meta-features are complete ?
  lrn = makeRemoveConstantFeaturesWrapper(
    learner = makeLearner(paste0("classif.", algo))
  )

  if(!is.null(trafo)) {
    params = lapply(params, trafo)
  }

  new.lrn = setHyperPars(learner = lrn, par.vals = params[!is.na(params)])
  ret = mlr::resample(learner = new.lrn, task = task, resampling = rdesc, 
    measures = list(ber, timeboth), show.info = FALSE)
  
  # ber = balanced error rate
  # averaged accuracy = 1 - balanced error rate
  value = 1 - (ret$aggr["ber.test.mean"])
  return(value)
}


#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
