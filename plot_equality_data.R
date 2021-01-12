namefun <- function(x){
    paste0("/res_stat/poor_pref_10dollarsSSP", x, "_rcp60_constant_estimates_climensemble_eta_1.RData") 
}
filelist = lapply(c(1:5), namefun)

results_table <- data.table(ssp=integer(), rcp=integer(), value=numeric())
origin = getwd()
for (file in filelist) {
  load(paste0(origin, file))
  sspnum = substr(strsplit(file, split = "SSP")[[1]][2], 1, 1)
  rcpnum = substr(strsplit(file, split = "rcp")[[1]][2], 1, 1)
  results_table <- rbind(results_table, list(sspnum, rcpnum, poor_prefer_10[[1,"mean"]]))
  
}

