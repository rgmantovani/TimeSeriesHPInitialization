#--------------------------------------------------------------------------------------------------
# perform cross-validation and compute average balanced accuracy on folds
#--------------------------------------------------------------------------------------------------

runBaseLearner = function(datafile, algo, params, dirs, folds = 5, trafo = NULL) {

  assertChoice(x = algo, choices = c("svm", "J48"), .var.name = "algo")

  data = read.arff(paste(dirs$data.dir, datafile, sep="/"))
  task = makeClassifTask(id = datafile, data = data, target = "Class")
  rdesc = makeResampleDesc(method = "CV", iter = folds)

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
