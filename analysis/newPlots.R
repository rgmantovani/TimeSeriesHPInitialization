#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

library("reshape2")
library("ggplot2")

source("R/config.R")
source("R/utils.R")
source("R/stats.R")

source("R/getData.R")
source("R/getMtfData.R")
source("R/getRuntimeData.R")

source("R/getLinePlot.R")
source("R/getRuntimePlot.R")

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

averagePerformance = function(algo, tuning, measure = "mean", extension = ".pdf") {

  checkmate::assertChoice(x = algo, choices = c("svm", "J48"))
  checkmate::assertChoice(x = tuning, choices = c("rs", "df", "pso", "mbo"))
  checkmate::assertChoice(x = measure, choices = c("mean", "max"))
  checkmate::assertChoice(x = extension, choices = c(".pdf", ".eps"))

  cat( "============================================= \n")
  cat(" @Plots for algo:", algo, "and measure:", measure, "\n")
  cat( "============================================= \n")

  all.files = list.files(path = "data", full.name = TRUE)
  algo.files = all.files[grepl(x = all.files, pattern = algo)]

  # -----------------------------------
  # Baselines - Results for 50 datasets
  # -----------------------------------

  cat(" - reading baselines' results \n")
  df.baselines = getBaselinesResults(algo.files = algo.files, measure = measure)
  sub.files = algo.files[grepl(x = algo.files, pattern = tuning)]

  # -----------------------------------
  # Meta-feature files
  # -----------------------------------

  cat(" - reading meta-features' results \n")
  nn.mtf.result = getNnMtfResults(sub.files = sub.files, measure = measure)
  rf.mtf.result = getRfMtfResults(sub.files = sub.files, measure = measure)

  # -----------------------------------
  # DTW strategy - Results for 50 datasets
  # -----------------------------------

  cat(" - reading dtw results \n")
  dtw.results = getDWTResults(sub.files = sub.files, measure = measure)

  # -----------------------------------
  # CUP and HIST files (with and without k-value)
  # -----------------------------------

  cat(" - reading pqh results \n")
  df.cup.hist = getPqhResults(sub.files = sub.files, measure = measure)

  # -----------------------------------
  # Complete dataset and best technique per dataset
  # -----------------------------------
  cat(" - ploting simple measures \n")

  # - Data frame with kNN results
  df.list = list(df.baselines, nn.mtf.result, dtw.results,
    df.cup.hist[,c("NN.CUP", "NN.HIST", "dataset")])
  df.nn = Reduce(function(...) merge(..., by = "dataset"), df.list)

  Best = colnames(df.nn)[apply(df.nn[2:ncol(df.nn)], 1, which.max) + 1]
  df.nn = cbind(1:nrow(df.nn), df.nn, Best)

  colnames(df.nn)[1] = "data.id"
  df.nn = df.nn[order(df.nn[,3], decreasing = TRUE),]
  df.nn[,1] = factor(df.nn[,1], levels = df.nn[,1])

  g.nn = getLinePlot(df = df.nn, measure = measure, put.annotations = TRUE)
  ggsave(g.nn, file = paste0("output/", algo, "_", tuning, "_nn_", measure, extension),
    dpi = 480, width = 11.9, height = 3.4, units = "in")

  # - Data frame with RF results
  rf.list = list(df.baselines, rf.mtf.result,
    df.cup.hist[,c("RF.CUP", "RF.HIS", "dataset")])
  df.rf = Reduce(function(...) merge(..., by = "dataset"), rf.list)

  Best = colnames(df.rf)[apply(df.rf[2:ncol(df.rf)], 1, which.max) + 1]
  df.rf = cbind(1:nrow(df.rf), df.rf, Best)

  colnames(df.rf)[1] = "data.id"
  df.rf = df.rf[order(df.rf[,3], decreasing = TRUE),]
  df.rf[,1] = factor(df.rf[,1], levels = df.rf[,1])

  g.rf = getLinePlot(df = df.rf, measure = measure, put.annotations = TRUE)

  ggsave(g.rf, file = paste0("output/", algo, "_", tuning, "_rf_", measure, extension),
    dpi = 480, width = 11.9, height = 3.4, units = "in")

  # -----------------------------------
  # do the plots (max and avg)
  # -----------------------------------

  # rv, k = 3, b = 10
  sub.dtw.files = sub.files[grepl(x = sub.files, pattern = "expKNNdtw.*rv-3")]
  df.dtw.sub = getAggValueDF(files = sub.dtw.files, label = "DTW", measure = measure)
  df.dtw.sub$dataset = rownames(df.dtw.sub)
  rownames(df.dtw.sub) = NULL

  aux.files  = sub.files[grepl(x = sub.files, pattern="expKNNpqh.*hi.*10-ed-3.*rv")]
  sub.nn.his = getAggValueDF(files = aux.files, label = "NN.HIS", measure = measure)
  sub.nn.his$dataset = rownames(sub.nn.his)
  rownames(sub.nn.his) = NULL

  aux.files =  sub.files[grepl(x = sub.files, pattern="expKNNpqh.*qu.*10-ed-3.*rv")]
  sub.nn.cup = getAggValueDF(files = aux.files, label = "NN.CUP", measure = measure)
  sub.nn.cup$dataset = rownames(sub.nn.cup)
  rownames(sub.nn.cup) = NULL

  # -----------------------------------
  # NN - mtf
  # -----------------------------------

  cat(" - ploting mtf comparisons \n ")
  df.nn.mfg = getMtfKnnData(sub.files = sub.files, measure = measure)

  nn.list = list(df.baselines, df.nn.mfg, sub.nn.cup, sub.nn.his, df.dtw.sub)
  mtf.nn = Reduce(function(...) merge(..., by = "dataset", all = TRUE), nn.list)
  Best = colnames(mtf.nn)[apply(mtf.nn[2:ncol(mtf.nn)], 1, which.max) + 1]

  mtf.nn = cbind(1:nrow(mtf.nn), mtf.nn, Best)
  colnames(mtf.nn)[1] = "data.id"
  mtf.nn = mtf.nn[order(mtf.nn[,3], decreasing = TRUE),]
  mtf.nn[,1] = factor(mtf.nn[,1], levels = mtf.nn[,1])

  g2.nn = getLinePlot(df = mtf.nn, measure = measure, put.annotations = TRUE)

  ggsave(g2.nn, file = paste0("output/", algo, "_", tuning, "_nn_mfg_", measure, extension),
    dpi = 480, width = 11.9, height = 3.9, units = "in")


  # -----------------------------------
  # RF- cup, hist and mtf
  # -----------------------------------

  rf.his.files = sub.files[grepl(x = sub.files, pattern = "expRFpqh.*hi.*rv")]
  aux.files = rf.his.files[grepl(x = rf.his.files, pattern = "-10-.*rv")]
  sub.rf.his = getAggValueDF(files = aux.files, label = "RF.HIS", measure = measure)
  sub.rf.his$dataset = rownames(sub.rf.his)
  rownames(sub.rf.his) = NULL

  rf.cup.files = sub.files[grepl(x = sub.files, pattern = "expRFpqh.*qu.*rv")]
  aux.files = rf.cup.files[grepl(x = rf.cup.files, pattern = "-10-.*rv")]
  sub.rf.cup = getAggValueDF(files = aux.files, label = "RF.CUP", measure = measure)
  sub.rf.cup$dataset = rownames(sub.rf.cup)
  rownames(sub.rf.cup) = NULL

  # -----------------------------------
  # -----------------------------------

  df.rf.mfg = getMtfRfData(sub.files = sub.files, measure = measure)
  rf.list = list(df.baselines, df.rf.mfg, sub.rf.cup, sub.rf.his)
  mtf.rf = Reduce(function(...) merge(..., by = "dataset", all = TRUE), rf.list)

  Best = colnames(mtf.rf)[apply(mtf.rf[2:ncol(mtf.rf)], 1, which.max) + 1]

  mtf.rf = cbind(1:nrow(mtf.rf), mtf.rf, Best)
  colnames(mtf.rf)[1] = "data.id"
  mtf.rf = mtf.rf[order(mtf.rf[,3], decreasing = TRUE),]
  mtf.rf[,1] = factor(mtf.rf[,1], levels = mtf.rf[,1])

  g2.rf = getLinePlot(df = mtf.rf, measure = measure, put.annotations = TRUE)
  ggsave(g2.rf, file = paste0("output/", algo, "_", tuning, "_rf_mfg_", measure, extension),
    dpi = 480, width = 11.9, height = 3.9, units = "in")

  cat("=============================================\n")
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

runTime = function(algo, tuning, extension = ".pdf") {

  checkmate::assertChoice(x = algo, choices = c("svm", "J48"))
  checkmate::assertChoice(x = tuning, choices = c("rs", "df", "pso", "mbo"))

  cat("============================================= \n")

  all.files  = list.files(path = "data", full.name = TRUE)
  algo.files = all.files[grepl(x = all.files, pattern = algo)]
  sub.files  = algo.files[grepl(x = algo.files, pattern = tuning)]

  df.time = getRunTimeData(sub.files = sub.files)

  cat(" @Plots runtime for algo:", algo, "and measure: feat.time \n")
  g = getRuntimePlot(df = df.time)
  ggsave(g, file = paste0("output/", algo, "_", tuning, "_featTime", extension),
    dpi = 300, width = 11.9, height = 3.4, units = "in")

  cat("=============================================\n")
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

for(algo in c("svm", "J48")) {
  for(tuning in c("rs", "pso", "smbo")) {
    for(measure in c("mean", "max")) {
      averagePerformance(algo = algo, tuning = tuning, measure = measure, extension = ".pdf")
    }
    runTime(algo = algo, tuning = tuning, extension = ".pdf")
  }
}

# averagePerformance(algo = "svm", tuning = "rs", measure = "mean", extension = ".pdf")
# averagePerformance(algo = "svm", tuning = "rs", measure = "max",  extension = ".pdf")
# averagePerformance(algo = "J48", tuning = "rs", measure = "mean", extension = ".pdf")
# averagePerformance(algo = "J48", tuning = "rs", measure = "max",  extension = ".pdf")
# runTime(algo = "svm", tuning = "rs", extension = ".pdf")
# runTime(algo = "J48", tuning = "rs", extension = ".pdf")

cat("\n Finished.\n")

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
