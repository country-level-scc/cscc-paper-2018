library(data.table)
library(ggplot2)

type_str = "poor_pref_10dollars"
version_string = "v1"
namefun <- function(x, y, z){
    paste0("/res_stat/", type_str, "SSP", x, "_rcp", y, 
           "_constant_estimates_climensemble_eta_", z, ".RData") 
}
filelist=c()
for (y in c(45, 60)){
  for (z in c(1, 1.5)){
    filelist2 = lapply(c(1:5), namefun, y=y, z=z)
    filelist=c(filelist, filelist2)
  }
}


results_table <- data.table(ssp=integer(), rcp=numeric(), eta=numeric(), value=numeric())
origin = getwd()
for (file in filelist) {
  load(paste0(origin, file))
  sspnum = as.numeric(substr(strsplit(file, split = "SSP")[[1]][2], 1, 1))
  rcpnum = substr(strsplit(file, split = "rcp")[[1]][2], 1, 2)
  rcpnum = sub("(.{1})(.*)", "\\1.\\2", rcpnum)
  etanum = strsplit(strsplit(file, split = "eta_")[[1]][2], split=".RData")[[1]][1]
  results_table <- rbind(results_table, list(sspnum, rcpnum, etanum, poor_prefer_10[[1,"mean"]]))
}

ggplot(results_table, aes(x=rcp, y=value, fill=eta))+geom_boxplot()+geom_point(
  position=position_jitterdodge(),alpha=0.3
) + ylab("Yearly income (USD)") + labs(
  title="Income at which direct donation is preferable to $10/ton CO2",
  fill="Inequality aversion"
)
savefig = paste0(type_str, version_string, ".png")
ggsave(path="plots", filename=savefig)
