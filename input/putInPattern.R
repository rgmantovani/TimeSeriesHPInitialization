
# files = list.files()

files = list.files(path = "dt_hp_clean")

# data.names = gsub(x = files, pattern = ".RData", replacement = "")

#create 
for(file in files) {
  print(file)
  obj = dget(paste0("dt_hp_clean/", file))

  df = cbind(obj$psol, obj$rs.sol, obj$df.sol)

  data.name = gsub(x = file, pattern = ".RData", replacement = "") 
  output.dir = paste0("dt_hp_clean/", data.name)
  dir.create(path = output.dir)

  df$PSO.not.collapse.tree = as.logical(df$PSO.not.collapse.tree)
  df$PSO.using.reduced.error.pruning = as.logical(df$PSO.using.reduced.error.pruning)
  df$PSO.using.only.binary.split = as.logical(df$PSO.using.only.binary.split)
  df$PSO.not.doing.subtree.raising = as.logical(df$PSO.not.doing.subtree.raising)
  df$PSO.using.laplace.smoth = as.logical(df$PSO.using.laplace.smoth)
  df$PSO.not.using.MDL.correction = as.logical(df$PSO.not.using.MDL.correction)
  df$PSO.data.balancing = NULL
  df$PSO.feature.selection = NULL

  df$RS.not.collapse.tree = as.logical(df$RS.not.collapse.tree)
  df$RS.using.reduced.error.pruning = as.logical(df$RS.using.reduced.error.pruning)
  df$RS.using.only.binary.split = as.logical(df$RS.using.only.binary.split)
  df$RS.not.doing.subtree.raising = as.logical(df$RS.not.doing.subtree.raising)
  df$RS.using.laplace.smoth = as.logical(df$RS.using.laplace.smoth)
  df$RS.not.using.MDL.correction = as.logical(df$RS.not.using.MDL.correction)
  df$RS.data.balancing = NULL
  df$RS.feature.selection = NULL

  df$DF.not.collapse.tree = as.logical(df$DF.not.collapse.tree)
  df$DF.using.reduced.error.pruning = as.logical(df$DF.using.reduced.error.pruning)
  df$DF.using.only.binary.split = as.logical(df$DF.using.only.binary.split)
  df$DF.not.doing.subtree.raising = as.logical(df$DF.not.doing.subtree.raising)
  df$DF.using.laplace.smoth = as.logical(df$DF.using.laplace.smoth)
  df$DF.not.using.MDL.correction = as.logical(df$DF.not.using.MDL.correction)
  df$DF.data.balancing = NULL
  df$DF.feature.selection = NULL


  for(i in 1:30) {
    x = df[i, ]
    dump("x", file = paste0(output.dir, "/", data.name, "-", i, ".RData"))
  }
}