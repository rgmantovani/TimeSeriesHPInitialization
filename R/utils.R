#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
# reading and pre-processing an already (standardized) dataset for PCA
# arg[1] = the name of the file the data are stored in
# arg[2] = exclude ("ex") or include (e.g. "in") the class attribute 
read.pre.process.data.pca <- function(data.file, inex) {

  data = read.arff(data.file)
  feature.names = colnames(data)

  to.exclude = vector("character")
  to.delete = vector("integer")
  
  for (j in 1:ncol(data)) {

    if (length(unique(data[,j])) == 1) {
      to.delete = append(to.delete,j,length(to.delete))
    } else {
   		if (class(data[,j]) != "factor") {
	     	to.exclude = append(to.exclude,feature.names[j],length(to.exclude))
	 		}
		}
	}
			
  if (inex == "ex") {
    to.delete = append(to.delete, ncol(data), length(to.delete))
  }
  
  if (length(to.delete) > 0) {
    data = data[, -to.delete]
  }

  # OLD:   
  # binarized.data = createDummyFeatures(data, method = "1-of-n", exclude = to.exclude)
  if(inex == "ex") {
    target = character(0L) 
  } else { #include class, but not binarized
    target = "Class"
    data[, target] = as.numeric(data[, target])
  }
  binarized.data = createDummyFeatures(obj = data, target = target)
  return(binarized.data)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
# computing the distance matrix using DTW
# arg[1] = list of eigenvalues computed from pca
# arg[2] = classical distance ("rv") or normalized (e.g. "nv")
compute.distance.matrix.dtw <- function(input.data, rv.nv) {
  result <- matrix(, nrow = length(input.data), ncol = length(input.data))
  dimnames(result) <- list(dataset.names,dataset.names)

  for (i in 1:length(input.data)) {
  	start.time <- System$currentTimeMillis()

		vect.i <- input.data[[i]]/sum(input.data[[i]])
  	  	
    for (j in 1:length(input.data)) {
      if (j != i) {
   			vect.j <- input.data[[j]]/sum(input.data[[i]])

		    dtw.dist <- dtw(vect.i,vect.j,distance.only=TRUE);
      
		    if (rv.nv == "rv") {
					result[i,j] <- dtw.dist$distance
		    } else {
		      	result[i,j] <- dtw.dist$normalizedDistance
		      }
      } else {
      		result[i,j] <- 0
      	}
    }
        
    result.matrix[i,"time.dist"] <<- System$currentTimeMillis() - start.time
  }

  return(result)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
# orders other datasets according to their distance (nearest first) to the given dataset
# arg[1] = the number of the dataset in the list of dataset names
# arg[2] = the distance matrix
# arg[3] = the number of nearest neighbors
compute.k.nn <- function(dataset.number, distances, k) {
	result <- vector(mode="character", length=k)
	
	sorted.list <- sort(distances, method="shell", index.return=TRUE)

  i <- j <- 1
	while (i <= k) {
		if (dataset.names[dataset.number] != dataset.names[sorted.list$ix[j]]) {
			result[i] <- dataset.names[sorted.list$ix[j]]
			i <- i + 1
		}
		j <- j + 1
	}  
	
	return(result)
}


#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
# get the best hyper-parameters for the given dataset
# arg[1] = datasets from the nearest neighbor list
# arg[2] = the tuning method according to which the best hyper-parameters are delivered
#					 i.e. "pso", "rs", "dfs"
# arg[3] = the number of nearest neighbors the results are aggregated for 
#					 (basically, the length of arg[1])

# TODO: adapt here to run with DT data

get.aggregated.best.hp <- function(datasets.to.consider, hp.tuning, k) {
	
  res = matrix(0, nrow=30, ncol=2)
	
	for (i in 1:length(datasets.to.consider)) {
		
    directory = paste(hp.dir,datasets.to.consider[i],sep="/")
		
		for (j in 1:30) {
			hp.file = paste(directory, paste(paste(datasets.to.consider[i],j,sep="-"), "RData", sep="."), sep="/")
			best.hp.j = dget(file = hp.file, keep.source = TRUE)
			
			if (hp.tuning == "pso") {
				res[j,] = res[j,] + c(best.hp.j[11,"PSO.cost"], best.hp.j[11,"PSO.gamma"])
			} else {
				if (hp.tuning == "rs") {
					res[j,] = res[j,] + c(best.hp.j[11,"RS.cost"], best.hp.j[11,"RS.gamma"])
				} else {
					if (hp.tuning == "df") {
						res[j,] = res[j,] + c(best.hp.j[11,"DF.cost"], best.hp.j[11,"DF.gamma"])
					}
				}
			}
		}
	}

	return(res/k)
}
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------


# compute the balanced classification accuracy (based on Rafael's code)
# arg[1] = predicted classes
# arg[2] = real classes
compute.balanced.accuracy <- function(pred, test){
	pred <- factor(pred, levels=levels(test))
	m <- caret::confusionMatrix(pred, test)

	if (length(levels(test)) > 2) {
		balanced.acc <- mean(m$byClass[,8])
	} else {
			balanced.acc <- m$byClass[8]
	}
	
	if (is.na(balanced.acc)) {
	  balanced.acc <- m$overall[1]
	}
	
	return(balanced.acc)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

# perform cross-validation (using SVM) ad compute average balanced accuracy on folds
# arg[1] = the name of the data file
# arg[2] = the C parameter for SVM
# arg[3] = the gamma parameter for SVM
# arg[4] = the number of folds for cross-validation
compute.svm.cv <- function(datafile, svm.cost, svm.gamma, folds) {
	data <- read.arff(paste(data.dir,datafile,sep="/"))
  result <- vector("numeric")
  
  summed.accuracy <- 0
  number.of.empty.models <- 0
  
  for (i in 1:folds) {
    from <- ceiling(nrow(data)*((i-1)/folds))+1
  	to <- ceiling(nrow(data)*(i/folds))
		range <- c(from:to)

		test.data = data[range,]; 
		train.data = data[-range,];   

		if (length(unique(train.data[,"Class"])) > 1) {
			model <- svm(Class ~ ., train.data, kernel="radial", cost = 2^svm.cost, gamma=2^svm.gamma);
			predicted <- predict(model, test.data)

			balanced.accuracy <- compute.balanced.accuracy(predicted,test.data[,"Class"])

		  summed.accuracy <- summed.accuracy + balanced.accuracy
    } else {
      	number.of.empty.models <- number.of.empty.models + 1
      }
  }
  value = summed.accuracy/(folds-number.of.empty.models)
  return(value)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
#
# compute histograms of proportion of variance
# arg[1] = input vector of eigenvalues
# arg[2] = number of bins
find.histogram <- function(input.data, bins, rv.nv) {
	prop.of.variance <- input.data/sum(input.data)	
  intervals <- seq(from = 0.0, to = 1.0, by = (1/bins))

	histogram <- hist(prop.of.variance, breaks=intervals, plot=FALSE)

	if (rv.nv == "nv") {
		return( histogram$counts/length(prop.of.variance) )
	}	else {
			return(histogram$counts)
		}
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

# compute the indices of pc explaining different quantiles of variance
# arg[1] = input vector of eigenvalues
# arg[2] = number of quantiles
# arg[3] = real or normalized values
find.quantiles <- function(input.data, quantiles, rv.nv) {
	cum.prop.variance <- input.data/sum(input.data)	
	for (i in 2:length(cum.prop.variance)) {
		cum.prop.variance[i] <- cum.prop.variance[i] + cum.prop.variance[i-1]
	}
		
  intervals <- seq(from = 0.0, to = 1.0, by = (1/quantiles))

  quantile.indices <- vector("numeric",quantiles)
	q <- 1
	while (q <= quantiles) {
		to <- intervals[q+1]

    for (i in (quantile.indices[q]+1):length(cum.prop.variance)) {
			if (cum.prop.variance[i] > to) {
				quantile.indices[q] <- i				
				break
      }
    }

		q <- q+1
  }
	quantile.indices[q-1] <- length(cum.prop.variance)
	
	if (rv.nv == "nv") {
		return( quantile.indices/length(cum.prop.variance) )
	}	else {
			return(quantile.indices)
		}
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

# computes the euclidean distance of the two vectors in the arguments
euclidean.distance <- function(vector.i, vector.j) {
	return( sqrt(sum((vector.i - vector.j)^2)) )
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

# computes the inner product of the two vectors in the arguments
inner.product <- function(vector.i, vector.j) {
  return( sum(vector.i * vector.j) )
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
# computes the norm of the vector in the argument
vector.norm <- function(vector) {
  return( sqrt(sum(vector^2)) )
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
# computes the cosine similarity of the two vectors in the arguments
cosine.similarity <- function(vector.i, vector.j) {
  return( inner.product(vector.i, vector.j) / (vector.norm(vector.i) * vector.norm(vector.j)) )
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
# computes the pearson correlation of the two vectors in the arguments
pearson.correlation <- function(vector.i, vector.j) {
  return( cosine.similarity(vector.i - mean(vector.i), vector.j - mean(vector.j)) )
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
# computes distance of the two vectors in the arguments according to the given distace measure
compute.distance <- function(vector.i, vector.j, distance.measure) {
	if (distance.measure == "ed") {
		return( euclidean.distance(vector.i,vector.j) )
	} else {
			if (distance.measure == "ip") {
				return( inner.product(vector.i,vector.j) )
			} else {
					if (distance.measure == "cs") {
					 	return( cosine.similarity(vector.i,vector.j) )
					} else {
							if (distance.measure == "pc") {
								return( pearson.correlation(vector.i,vector.j) )
							} else {
									return(NULL)
								}
						}
				}
		}
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
# computing the distance matrix from quantiles or histograms
# arg[1] = list of eigenvvalues computed from pca
# arg[2] = quantiles ("qu") or histograms (e.g. "hi") should be considered
# arg[3] = number of bins
# arg[4] = the distance measure used, i.e. "ed", "ip", "cs", "pc"
# arg[5] = real ("rv") or normalized ("nv") values w.r.t. the length of vectors
compute.distance.matrix.qu.hi <- function(input.data, qu.hi, num.bins, distance.measure, rv.nv) {
  result <- matrix(, nrow = length(input.data), ncol = length(input.data))
  dimnames(result) <- list(dataset.names,dataset.names)

  for (i in 1:length(input.data)) {
  	start.time <- System$currentTimeMillis()

   	if (qu.hi == "qu") {
  		vect.i <- find.quantiles(input.data[[i]], num.bins, rv.nv)
  	} else {
				vect.i <- find.histogram(input.data[[i]], num.bins, rv.nv)
  		}
  	  	
    for (j in 1:length(input.data)) {
      if (j != i) {
      	if (qu.hi == "qu") {
		  		vect.j <- find.quantiles(input.data[[j]], num.bins, rv.nv)
		  	} else {
						vect.j <- find.histogram(input.data[[j]], num.bins, rv.nv)		  	
		  		}
		  	
		  	result[i,j] <- compute.distance(vect.i, vect.j, distance.measure)
			} else {
					result[i,j] <- 0
				}
    }
    
    result.matrix[i,"time.dist"] <<- System$currentTimeMillis() - start.time
  }

  return(result)
}


#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
# converts an integer to a boolean vector corresponding to its binary representation
convert.mf.group.combination.to.vector <- function (number, num.bits = 8) {
#  return( rev(as.logical(intToBits(number))[1:num.bits]) )
	return( as.logical(intToBits(number))[1:num.bits] )
}



#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
# computing the distance matrix from quantiles or histograms
# arg[1] = matrix of meta-features (row represents datasets, column represents meta-features)
# arg[2] = the distance measure used, i.e. "ed", "ip", "cs", "pc"
compute.distance.matrix.mf.vectors <- function(input.data, distance.measure) {
  result <- matrix(, nrow = nrow(input.data), ncol = nrow(input.data))
  dimnames(result) <- list(dataset.names,dataset.names)

  for (i in 1:nrow(input.data)) {
  	start.time <- System$currentTimeMillis()
  	  	
    for (j in 1:nrow(input.data)) {
      if (j != i) {
		  	result[i,j] <- compute.distance(input.data[i,], input.data[j,], distance.measure)
			} else {
					result[i,j] <- 0
				}
    }
    
    result.matrix[i,"time.dist"] <<- System$currentTimeMillis() - start.time
  }

  return(result)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
