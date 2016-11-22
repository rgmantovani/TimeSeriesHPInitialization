#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

# perform cross-validation (using SVM) ad compute average balanced accuracy on folds

# arg[1] datafile  = the name of the data file
# arg[2] svm.cost  = the C parameter for SVM
# arg[3] svm.gamma = the gamma parameter for SVM
# arg[4] folds     = the number of folds for cross-validation

runSVM = function(datafile, svm.cost, svm.gamma, folds) {

  data = read.arff(paste(data.dir, datafile, sep="/"))
  task = makeClassifTask(id = datafile, data = data, target = "Class")
  
  rdesc = makeResampleDesc(method = "CV", iter = folds)

  lrn = makeLearner("classif.svm")
  lrn = makeRemoveConstantFeaturesWrapper(learner = lrn)
  params = list(kernel = "radial", gamma = 2^svm.gamma, cost = 2^svm.cost)
  new.lrn = setHyperPars(learner = lrn, par.vals = params)

  ret = mlr::resample(learner = new.lrn, task = task, resampling = rdesc, 
    measures = list(ber, timeboth), show.info = FALSE)
  
  # ber = balanced error rate
  # averaged accuracy = 1 - balanced error rate
  value = 1 - (ret$aggr["ber.test.mean"])
  return(value)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
