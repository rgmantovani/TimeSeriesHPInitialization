#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getMtfKnnData = function(sub.files, measure) {

  mtf.1.nn = sub.files[grepl(x = sub.files, pattern = "expkNNmfg-1-.*rv-ed-3")]
  nn.sl = getAggValueDF(files = mtf.1.nn, label = "NN.CMF.SL", measure = measure)
  nn.sl$dataset = rownames(nn.sl)
  rownames(nn.sl) = NULL
 
  mtf.2.nn = sub.files[grepl(x = sub.files, pattern = "expkNNmfg-2-.*rv-ed-3")]
  nn.st = getAggValueDF(files = mtf.2.nn, label = "NN.CMF.ST", measure = measure)
  nn.st$dataset = rownames(nn.st)
  rownames(nn.st) = NULL
 
  mtf.4.nn = sub.files[grepl(x = sub.files, pattern = "expkNNmfg-4-.*rv-ed-3")]
  nn.it = getAggValueDF(files = mtf.4.nn, label = "NN.CMF.IT", measure = measure)
  nn.it$dataset = rownames(nn.it)
  rownames(nn.it) = NULL

  mtf.8.nn = sub.files[grepl(x = sub.files, pattern = "expkNNmfg-8-.*rv-ed-3")]
  nn.lm = getAggValueDF(files = mtf.8.nn, label = "NN.CMF.LM", measure = measure)
  nn.lm$dataset = rownames(nn.lm)
  rownames(nn.lm) = NULL
  
  mtf.16.nn  = sub.files[grepl(x = sub.files, pattern = "expkNNmfg-16-.*rv-ed-3")]
  nn.mb = getAggValueDF(files = mtf.4.nn, label = "NN.CMF.IT", measure = measure)
  nn.mb$dataset = rownames(nn.mb)
  rownames(nn.mb) = NULL

  mtf.32.nn  = sub.files[grepl(x = sub.files, pattern = "expkNNmfg-32-.*rv-ed-3")]
  nn.ti = getAggValueDF(files = mtf.32.nn, label = "NN.CMF.TI", measure = measure)
  nn.ti$dataset = rownames(nn.ti)
  rownames(nn.ti) = NULL
  
  mtf.64.nn  = sub.files[grepl(x = sub.files, pattern = "expkNNmfg-64-.*rv-ed-3")]
  nn.co = getAggValueDF(files = mtf.64.nn, label = "NN.CMF.CO", measure = measure)
  nn.co$dataset = rownames(nn.co)
  rownames(nn.co) = NULL

  mtf.128.nn = sub.files[grepl(x = sub.files, pattern = "expkNNmfg-128-.*rv-ed-3")]
  nn.cn = getAggValueDF(files = mtf.128.nn, label = "NN.CMF.CN", measure = measure)
  nn.cn$dataset = rownames(nn.cn)
  rownames(nn.cn) = NULL

  mtf.7.nn = sub.files[grepl(x = sub.files, pattern = "expkNNmfg-7-.*rv-ed-3")]
  nn.stl = getAggValueDF(files = mtf.7.nn, label = "NN.CMF.STL", measure = measure)
  nn.stl$dataset = rownames(nn.stl)
  rownames(nn.stl) = NULL

  mtf.56.nn = sub.files[grepl(x = sub.files, pattern = "expkNNmfg-56-.*rv-ed-3")]
  nn.lmt = getAggValueDF(files = mtf.56.nn, label = "NN.CMF.LMT", measure = measure)
  nn.lmt$dataset = rownames(nn.lmt)
  rownames(nn.lmt) = NULL

  mtf.255.nn = sub.files[grepl(x = sub.files, pattern = "expkNNmfg-255-.*rv-ed-3")]
  nn.all = getAggValueDF(files = mtf.255.nn, label = "NN.CMF.ALL", measure = measure)
  nn.all$dataset = rownames(nn.all)
  rownames(nn.all) = NULL

  obj = list(nn.sl, nn.st, nn.it, nn.lm, nn.mb, nn.ti, nn.co, nn.cn, nn.stl, 
    nn.lmt, nn.all)

  for (i in length(obj):1) {
    if(nrow(obj[[i]]) == 0) {
      obj[[i]] = NULL
    }
  }

  df.mfg = Reduce(function(...) merge(..., by = "dataset", all = TRUE), obj)
  return(df.mfg)

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getMtfRfData = function(sub.files, measure) {

  mtf.1.rf = sub.files[grepl(x = sub.files, pattern = "expRFMfg-1-.*rv")]
  rf.sl = getAggValueDF(files = mtf.1.rf, label = "RF.CMF.SL", measure = measure)
  rf.sl$dataset = rownames(rf.sl)
  rownames(rf.sl) = NULL
 
  mtf.2.rf = sub.files[grepl(x = sub.files, pattern = "expRFMfg-2-.*rv")]
  rf.st = getAggValueDF(files = mtf.2.rf, label = "RF.CMF.ST", measure = measure)
  rf.st$dataset = rownames(rf.st)
  rownames(rf.st) = NULL
 
  mtf.4.rf = sub.files[grepl(x = sub.files, pattern = "expRFMfg-4-.*rv")]
  rf.it = getAggValueDF(files = mtf.4.rf, label = "RF.CMF.IT", measure = measure)
  rf.it$dataset = rownames(rf.it)
  rownames(rf.it) = NULL
  
  mtf.8.rf = sub.files[grepl(x = sub.files, pattern = "expRFMfg-8-.*rv")]
  rf.lm = getAggValueDF(files = mtf.8.rf, label = "RF.CMF.LM", measure = measure)
  rf.lm$dataset = rownames(rf.lm)
  rownames(rf.lm) = NULL
  
  mtf.16.rf = sub.files[grepl(x = sub.files, pattern = "expRFMfg-16-.*rv")]
  rf.mb = getAggValueDF(files = mtf.4.rf, label = "RF.CMF.IT", measure = measure)
  rf.mb$dataset = rownames(rf.mb)
  rownames(rf.mb) = NULL
  
  mtf.32.rf = sub.files[grepl(x = sub.files, pattern = "expRFMfg-32-.*rv")]
  rf.ti = getAggValueDF(files = mtf.32.rf, label = "RF.CMF.TI", measure = measure)
  rf.ti$dataset = rownames(rf.ti)
  rownames(rf.ti) = NULL
  
  mtf.64.rf = sub.files[grepl(x = sub.files, pattern = "expRFMfg-64-.*rv")]
  rf.co = getAggValueDF(files = mtf.64.rf, label = "RF.CMF.CO", measure = measure)
  rf.co$dataset = rownames(rf.co)
  rownames(rf.co) = NULL
  
  mtf.128.rf = sub.files[grepl(x = sub.files, pattern = "expRFMfg-128-.*rv")]
  rf.cn = getAggValueDF(files = mtf.128.rf, label = "RF.CMF.CN", measure = measure)
  rf.cn$dataset = rownames(rf.cn)
  rownames(rf.cn) = NULL

  mtf.7.rf = sub.files[grepl(x = sub.files, pattern = "expRFMfg-7-.*rv")]
  rf.stl = getAggValueDF(files = mtf.7.rf, label = "RF.CMF.STL", measure = measure)
  rf.stl$dataset = rownames(rf.stl)
  rownames(rf.stl) = NULL

  mtf.56.rf = sub.files[grepl(x = sub.files, pattern = "expRFMfg-56-.*rv")]
  rf.lmt = getAggValueDF(files = mtf.56.rf, label = "RF.CMF.LMT", measure = measure)
  rf.lmt$dataset = rownames(rf.lmt)
  rownames(rf.lmt) = NULL

  mtf.255.rf = sub.files[grepl(x = sub.files, pattern = "expRFMfg-255-.*rv")]
  rf.all = getAggValueDF(files = mtf.255.rf, label = "RF.CMF.ALL", measure = measure)
  rf.all$dataset = rownames(rf.all)
  rownames(rf.all) = NULL

  obj = list(rf.sl, rf.st, rf.it, rf.lm, rf.mb, rf.ti, rf.co, rf.cn, rf.stl, 
    rf.lmt, rf.all)

  for (i in length(obj):1) {
    if(nrow(obj[[i]]) == 0) {
      obj[[i]] = NULL
    }
  }

  df.mfg = Reduce(function(...) merge(..., by = "dataset", all = TRUE), obj)
  return(df.mfg)
}


#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------


