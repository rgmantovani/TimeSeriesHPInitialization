#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getBaselinesResults = function(algo.files, measure) {

  rs.files  = algo.files[grepl(x = algo.files, pattern = "expDrp.*rs")]
  rs.result = getAggValueDF(files = rs.files, label = "RS", measure = measure)

  df.files  = algo.files[grepl(x = algo.files, pattern = "expDrp.*df")]
  df.result = getAggValueDF(files = df.files, label = "DF", measure = measure)

  pso.files  = algo.files[grepl(x = algo.files, pattern = "expDrp.*pso")]
  pso.result = getAggValueDF(files = pso.files, label = "PSO", measure = measure)

  smbo.files  = algo.files[grepl(x = algo.files, pattern = "expDrp.*smbo")]
  smbo.result = getAggValueDF(files = smbo.files, label = "SMBO", measure = measure)

  df.baselines = cbind(df.result, rs.result, pso.result, smbo.result)
  
  df.baselines$dataset = rownames(df.baselines)
  rownames(df.baselines) = NULL

  return(df.baselines)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getDWTResults = function(sub.files, measure) {

  dtw.files = sub.files[grepl(x = sub.files, pattern = "expKNNdtw.*rv")]
  dtw.results = getAggValueDF(files = dtw.files, label = "DTW", measure = measure)
  dtw.results$dataset = rownames(dtw.results)
  rownames(dtw.results) = NULL
  return(dtw.results)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getNnMtfResults = function(sub.files, measure) {

  nn.mtf.files = sub.files[grepl(x = sub.files, pattern = "expkNNmfg.*rv.*ed")]
  nn.mtf.result = getAggValueDF(files = nn.mtf.files, label = "NN.CMF", measure = measure)
  nn.mtf.result$dataset = rownames(nn.mtf.result)
  rownames(nn.mtf.result) = NULL
  return(nn.mtf.result)
}


#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getRfMtfResults = function(sub.files, measure) {

  rf.mtf.files = sub.files[grepl(x = sub.files, pattern = "expRFMfg.*rv")]
  rf.mtf.result = getAggValueDF(files = rf.mtf.files, label = "RF.CMF", measure = measure)
  rf.mtf.result$dataset = rownames(rf.mtf.result)
  rownames(rf.mtf.result) = NULL
  return(rf.mtf.result)
}


#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getPqhResults = function(sub.files, measure) {

  nn.his.files = sub.files[grepl(x = sub.files, pattern="expKNNpqh.*hi.*ed.*rv")]
  nn.his.result = getAggValueDF(files = nn.his.files,label = "NN.HIST", measure = measure)
  nn.his.result$dataset = rownames(nn.his.result)

  nn.cup.files = sub.files[grepl(x = sub.files, pattern="expKNNpqh.*qu.*ed.*rv")]
  nn.cup.result = getAggValueDF(files = nn.cup.files, label = "NN.CUP", measure = measure)  
  nn.cup.result$dataset = rownames(nn.cup.result)

  rownames(nn.his.result) = rownames(nn.cup.result) = NULL

  rf.his.files = sub.files[grepl(x = sub.files, pattern = "expRFpqh.*hi.*rv")]
  rf.his.result = getAggValueDF(files = rf.his.files, label = "RF.HIS", measure = measure)
  rf.his.result$dataset = rownames(rf.his.result)

  rf.cup.files = sub.files[grepl(x = sub.files, pattern = "expRFpqh.*qu.*rv")]
  rf.cup.result = getAggValueDF(files = rf.cup.files, label = "RF.CUP", measure = measure)
  rf.cup.result$dataset = rownames(rf.cup.result)

  rownames(rf.his.result) = rownames(rf.cup.result) = NULL

  tmp = list(rf.his.result, rf.cup.result, nn.his.result, nn.cup.result)
  tmp2 = lapply(tmp, function(elem) {
    rownames(elem) = elem$dataset
    elem$dataset = NULL
    ret = data.frame(t(elem))
    return(ret)
  })

  df = data.frame(t(plyr::rbind.fill(tmp2)))
  df$dataset = rownames(df)
  colnames(df) = c("RF.HIS", "RF.CUP", "NN.HIST", "NN.CUP", "dataset")
  rownames(df) = NULL
  return(df)
}


#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
