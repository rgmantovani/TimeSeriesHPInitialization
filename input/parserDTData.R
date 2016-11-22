#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------

library("dplyr")


if(!dir.exists("input/dt_hp_clean/")) {
  dir.create("input/dt_hp_clean/", recursive = TRUE)
}

data.files = list.files(path = "input/dt_hp/")

aux = lapply(data.files, function(file) {

  print(file)
  obj = dget(paste0("input/dt_hp/", file))

  pso.sol = dplyr::select(data.frame(obj), dplyr::contains("PSO"))
  rs.sol  = dplyr::select(data.frame(obj), dplyr::contains("RS"))
  df.sol  = dplyr::select(data.frame(obj), dplyr::contains("DF"))

  # filtering data (C used if R = FALSE & N used if R = TRUE)
  ids = which(pso.sol$PSO.using.reduced.error.pruning == 0)
  pso.sol[-ids,  "PSO.pruning.confidence"] = NA 
  pso.sol[ids, "PSO.n.folds.error"] = NA 

  ids = which(rs.sol$RS.using.reduced.error.pruning == 0)
  rs.sol[-ids,  "RS.pruning.confidence"] = NA 
  rs.sol[ids, "RS.n.folds.error"] = NA 

  ids = which(df.sol$DF.using.reduced.error.pruning == 0)
  df.sol[-ids,  "DF.pruning.confidence"] = NA 
  df.sol[ids, "DF.n.folds.error"] = NA 

  obj = list(psol = pso.sol, rs.sol = rs.sol, df.sol = df.sol)
  dump("obj", file = paste0("input/dt_hp_clean/", file))

})


#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------

