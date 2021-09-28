#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getRunTimeData = function(sub.files) {

  tmp1 = .getkNNRuntime(sub.files = sub.files)
  tmp2 = .getRFRuntime(sub.files = sub.files)

  df.nn.time = na.omit(tmp1)
  df.rf.time = na.omit(tmp2)

  aux = Reduce("+", list(df.nn.time[2:ncol(df.nn.time)], df.rf.time[2:ncol(df.rf.time)]))/2
  aux = cbind(1:nrow(aux), df.nn.time$dataset, aux)
  colnames(aux)[1:2] = c("data.id", "dataset") 

  return(aux)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

.getkNNRuntime = function(sub.files) {

  dtw.files = sub.files[grepl(x = sub.files, pattern = "expKNNdtw.*rv-3")]
  dtw.time = getAggValueDF(files = dtw.files, label = "DTW", measure = "feat.time")
  dtw.time$dataset = rownames(dtw.time)
  rownames(dtw.time) = NULL

  nn.his.files = sub.files[grepl(x = sub.files, pattern="expKNNpqh.*hi.*10-ed-3.*rv")]
  nn.his.result = getAggValueDF(files = nn.his.files,label = "HIST", measure = "feat.time")
  nn.his.result$dataset = rownames(nn.his.result)
  rownames(nn.his.result) = NULL

  nn.cup.files = sub.files[grepl(x = sub.files, pattern="expKNNpqh.*qu.*10-ed-3.*rv")]
  nn.cup.result = getAggValueDF(files = nn.cup.files, label = "CUP", measure = "feat.time") 
  nn.cup.result$dataset = rownames(nn.cup.result)
  rownames(nn.cup.result) = NULL

  mtf.1.nn = sub.files[grepl(x = sub.files, pattern = "expkNNmfg-1-.*rv-ed-3")]
  nn.sl = getAggValueDF(files = mtf.1.nn, label = "CMF.SL", measure = "feat.time")
  nn.sl$dataset = rownames(nn.sl)
  rownames(nn.sl) = NULL

  mtf.2.nn   = sub.files[grepl(x = sub.files, pattern = "expkNNmfg-2-.*rv-ed-3")]
  nn.st = getAggValueDF(files = mtf.2.nn, label = "CMF.ST", measure = "feat.time")
  nn.st$dataset = rownames(nn.st)
  rownames(nn.st) = NULL
 
  mtf.4.nn   = sub.files[grepl(x = sub.files, pattern = "expkNNmfg-4-.*rv-ed-3")]
  nn.it = getAggValueDF(files = mtf.4.nn, label = "CMF.IT", measure = "feat.time")
  nn.it$dataset = rownames(nn.it)
  rownames(nn.it) = NULL
  
  mtf.8.nn   = sub.files[grepl(x = sub.files, pattern = "expkNNmfg-8-.*rv-ed-3")]
  nn.lm = getAggValueDF(files = mtf.8.nn, label = "CMF.LM", measure = "feat.time")
  nn.lm$dataset = rownames(nn.lm)
  rownames(nn.lm) = NULL
  
  mtf.16.nn  = sub.files[grepl(x = sub.files, pattern = "expkNNmfg-16-.*rv-ed-3")]
  nn.mb = getAggValueDF(files = mtf.4.nn, label = "CMF.MB", measure = "feat.time")
  nn.mb$dataset = rownames(nn.mb)
  rownames(nn.mb) = NULL
  
  # mtf.32.nn  = sub.files[grepl(x = sub.files, pattern = "expkNNmfg-32-.*rv-ed-3")]
  # nn.ti = getAggValueDF(files = mtf.32.nn, label = "CMF.TI", measure = "feat.time")
  # nn.ti$dataset = rownames(nn.ti)
  # rownames(nn.ti) = NULL
  
  mtf.64.nn  = sub.files[grepl(x = sub.files, pattern = "expkNNmfg-64-.*rv-ed-3")]
  nn.co = getAggValueDF(files = mtf.64.nn, label = "CMF.CO", measure = "feat.time")
  nn.co$dataset = rownames(nn.co)
  rownames(nn.co) = NULL
  
  mtf.128.nn = sub.files[grepl(x = sub.files, pattern = "expkNNmfg-128-.*rv-ed-3")]
  nn.cn = getAggValueDF(files = mtf.128.nn, label = "CMF.CN", measure = "feat.time")
  nn.cn$dataset = rownames(nn.cn)
  rownames(nn.cn) = NULL
 
  obj = list(nn.sl, nn.st, nn.it, nn.lm, nn.mb, #nn.ti, 
    nn.co, nn.cn, dtw.time, 
    nn.cup.result, nn.his.result)

  for (i in length(obj):1) {
    if(nrow(obj[[i]]) == 0) {
      obj[[i]] = NULL
    }
  }

  df.nn.time = Reduce(function(...) merge(..., by = "dataset", all = TRUE), obj)
  return(df.nn.time)

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

.getRFRuntime = function(sub.files) {

  dtw.files = sub.files[grepl(x = sub.files, pattern = "expKNNdtw.*rv-3")]
  dtw.time = getAggValueDF(files = dtw.files, label = "DTW", measure = "feat.time")
  dtw.time$dataset = rownames(dtw.time)
  rownames(dtw.time) = NULL

  rf.his.files = sub.files[grepl(x = sub.files, pattern = "expRFpqh.*hi-10-.*rv")]
  rf.his.result = getAggValueDF(files = rf.his.files, label = "HIS", measure = "feat.time")
  rf.his.result$dataset = rownames(rf.his.result)
  rownames(rf.his.result) = NULL

  rf.cup.files = sub.files[grepl(x = sub.files, pattern = "expRFpqh.*qu-10-.*rv")]
  rf.cup.result = getAggValueDF(files = rf.cup.files, label = "CUP", measure = "feat.time")
  rf.cup.result$dataset = rownames(rf.cup.result)
  rownames(rf.cup.result) = NULL

  mtf.1.rf = sub.files[grepl(x = sub.files, pattern = "expRFMfg-1-.*rv")]
  rf.sl = getAggValueDF(files = mtf.1.rf, label = "CMF.SL", measure = "feat.time")
  rf.sl$dataset = rownames(rf.sl)
  rownames(rf.sl) = NULL
 
  mtf.2.rf = sub.files[grepl(x = sub.files, pattern = "expRFMfg-2-.*rv")]
  rf.st = getAggValueDF(files = mtf.2.rf, label = "CMF.ST", measure = "feat.time")
  rf.st$dataset = rownames(rf.st)
  rownames(rf.st) = NULL

  mtf.4.rf = sub.files[grepl(x = sub.files, pattern = "expRFMfg-4-.*rv")]
  rf.it = getAggValueDF(files = mtf.4.rf, label = "CMF.IT", measure = "feat.time")
  rf.it$dataset = rownames(rf.it)
  rownames(rf.it) = NULL

  mtf.8.rf = sub.files[grepl(x = sub.files, pattern = "expRFMfg-8-.*rv")]
  rf.lm = getAggValueDF(files = mtf.8.rf, label = "CMF.LM", measure = "feat.time")
  rf.lm$dataset = rownames(rf.lm)
  rownames(rf.lm) = NULL

  mtf.16.rf = sub.files[grepl(x = sub.files, pattern = "expRFMfg-16-.*rv")]
  rf.mb = getAggValueDF(files = mtf.4.rf, label = "CMF.MB", measure = "feat.time")
  rf.mb$dataset = rownames(rf.mb)
  rownames(rf.mb) = NULL

  # mtf.32.rf = sub.files[grepl(x = sub.files, pattern = "expRFMfg-32-.*rv")]
  # rf.ti = getAggValueDF(files = mtf.32.rf, label = "CMF.TI", measure = "feat.time")
  # rf.ti$dataset = rownames(rf.ti)
  # rownames(rf.ti) = NULL

  mtf.64.rf = sub.files[grepl(x = sub.files, pattern = "expRFMfg-64-.*rv")]
  rf.co = getAggValueDF(files = mtf.64.rf, label = "CMF.CO", measure = "feat.time")
  rf.co$dataset = rownames(rf.co)
  rownames(rf.co) = NULL

  mtf.128.rf = sub.files[grepl(x = sub.files, pattern = "expRFMfg-128-.*rv")]
  rf.cn = getAggValueDF(files = mtf.128.rf, label = "CMF.CN", measure = "feat.time")
  rf.cn$dataset = rownames(rf.cn)
  rownames(rf.cn) = NULL

  obj = list(rf.sl, rf.st, rf.it, rf.lm, rf.mb, #rf.ti, 
    rf.co, rf.cn, dtw.time, 
    rf.cup.result, rf.his.result)

  for (i in length(obj):1) {
    if(nrow(obj[[i]]) == 0) {
      obj[[i]] = NULL
    }
  }

  df.rf.time = Reduce(function(...) merge(..., by = "dataset", all = TRUE), obj)
  return(df.rf.time)

}
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
