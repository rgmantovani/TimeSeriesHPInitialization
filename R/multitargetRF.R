#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

# Approach  (Individual target view)

# HP-type  Task                   Missing values
#  real    regression             0 (0 is not used by the parameter C)
#  logical binary classification  No
#  integer classification         New class
 
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

# convert logical features to (binary) factor ones
.handleDataFrame = function(df) {

  ids = which(sapply(df, class) == "logical")
  for(id in ids) {
    na.id = which(is.na(df[, id]))
    df[na.id, id] = FALSE
    df[, id] = as.factor(df[,id])
  }
  return(df)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

# Add a new class replacing NA values at the target feature
.completeClasses = function(df, target) {

  ids = which(is.na(df[,target]))
  if(length(ids) > 0) {
    df[ids, target] = "New.level"
    df[, target] = as.factor(df[,target])
  }
  return(df)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

# return a regression model
.getRegrLearner = function() {

  lrn = makeImputeWrapper(
    learner = makeLearner("regr.randomForest"),
    # classes = list(numeric = imputeMedian(), factor = imputeMode(), integer = imputeConstant(0)),
    classes = list(numeric = imputeConstant(0), factor = imputeMode(), integer = imputeConstant(0)),
    dummy.classes = c("numeric", "factor", "integer")
  )
  return(lrn)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

#return a classification model
.getClassifLearner = function() {

  lrn = makeImputeWrapper(
    learner = makeLearner("classif.randomForest"),
    # classes = list(numeric = imputeMedian(), factor = imputeMode(), integer = imputeConstant(0)),
    classes = list(numeric = imputeConstant(0), factor = imputeMode(), integer = imputeConstant(0)),
    dummy.classes = c("numeric", "factor", "integer")
  )
  return(lrn)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

# * args
# feature.matrix: meta-feature matrix
# targets: matrix with the multi-targets

multitargetRF = function(feature.matrix, targets) {
  
  rdesc = makeResampleDesc(method = "LOO")
  npars = ncol(targets)
  aux = lapply(colnames(targets), function(target) {
    
    cat("    - Predicting target: ", target, "\n")
    df = as.data.frame(cbind(feature.matrix, targets))
    df = .handleDataFrame(df = df)
  
    if(class(targets[, target]) == "numeric") {
   
      # regression problems
      lrn = .getRegrLearner()
      meas = list(mse, rmse, timeboth)
      df[which(is.na(df[, target])), target] = 0
      new.task = makeRegrTask(id = paste0(target, "_regr"), data = df, target = target)
   
    } else {
   
      # classification problems
      lrn = .getClassifLearner()
      meas = list(acc, ber, timeboth)
      new.df = .completeClasses(df = df, target = target)
      new.task = makeClassifTask(id = paste0(target, "_classif"), data = new.df, target = target)
    
    }

    run = mlr::resample(learner = lrn, task = new.task, resampling = rdesc, 
      measures = meas, show.info = FALSE)

    ret = run$pred$data
    rownames(ret) = rownames(df)

    new.name = paste(target, colnames(ret[, 2:3]), sep=".")
    colnames(ret)[2:3] = new.name
    return(ret)
  })

  # Reducing values
  res = Reduce(merge, aux)
  res$id = NULL
  res = res[order(res$iter),]
  rownames(res) = rownames(feature.matrix)

  cat("    - Checking predictions ... \n")
  new.settings = checkParams(settings = res, algo = ALGO)
  return(new.settings)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
