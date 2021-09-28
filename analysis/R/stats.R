#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

wilcoxonTest = function(alg1, alg2, conf = 0.95) {

  obj = wilcox.test(alg1, alg2, paired = TRUE)
  p.value = obj$p.value
  alpha = 1 - conf
  return(p.value < alpha)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

statTest = function(mat) {

  out = matrix(data = NA, ncol = ncol(mat), nrow = ncol(mat)) 
  colnames(out) = colnames(mat)
  rownames(out) = colnames(mat)  

  diffs = out

  for(i in 1:(ncol(mat)-1)) {
    for(j in (i+1):ncol(mat)) {
      if(i != j ){
        out[i,j] = wilcoxonTest(alg1 = mat[,i], alg2 = mat[,j])
        diffs[i,j] = mean(mat[,i] - mat[,j])
      }
    }
  }
  diffs = diffs * 10^3
  ret = list(stats = out, diffs = diffs)
  return(ret)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
