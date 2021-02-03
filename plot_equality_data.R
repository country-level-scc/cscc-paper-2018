library(data.table)
library(ggplot2)
# Should be either "eri_eq_statscc_2020d" or "poor_pref_10dollars"
type_str = "eri_eq_statscc_2020d" 

version_string = "v2"
namefun <- function(x, y, z){
    paste0("/res_stat/", type_str, "SSP", x, "_rcp", y, 
           "_constant_estimates_climensemble_eta_", z, ".RData") 
}
namefun_altdamage <- function(x, y, z){
  paste0("/res_stat_djo_richpoor/", type_str, "SSP", x, "_rcp", y,
         "_constant_estimates_climensemble_djo_eta_", z, ".RData")
}

filelist=c()
for (y in c(45, 60, 85)){
  for (z in c(1, 1.5)){
    filelist2 = lapply(c(1:5), namefun, y=y, z=z)
    filelist=c(filelist, filelist2)
  }
}
for (y in c(45, 60, 85)){
  for (z in c(1, 1.5)){
    filelist2 = lapply(c(1:5), namefun_altdamage, y=y, z=z)
    filelist=c(filelist, filelist2)
  }
}



results_table <- data.table(ssp=integer(), rcp=numeric(), eta=numeric(), damages=character(), indicator=character(), value=numeric())
columns_to_save = c("mean")
origin = getwd()
for (file in filelist) {
  load(paste0(origin, file))
  sspnum = as.numeric(substr(strsplit(file, split = "SSP")[[1]][2], 1, 1))
  rcpnum = substr(strsplit(file, split = "rcp")[[1]][2], 1, 2)
  rcpnum = sub("(.{1})(.*)", "\\1.\\2", rcpnum)
  etanum = strsplit(strsplit(file, split = "eta_")[[1]][2], split=".RData")[[1]][1]
  damages = if(grepl("djo", file)) "DJO" else "Burke"
  for (column in columns_to_save){
    if(type_str == "eri_eq_statscc_2020d"){
      results_table <- rbind(results_table, list(
        sspnum, rcpnum, etanum, damages, column, eri_eq_stat_wscc[[1, column]])
      )
    } else {
      results_table <- rbind(results_table, list(
        sspnum, rcpnum, etanum, damages, column, poor_prefer_10[[1, column]])
      )
    }
  }
}

if(type_str == "eri_eq_statscc_2020d"){
  plot_labs = labs(
    title="World SCC relative to the income of an Eritrean",
    fill="Inequality aversion"
  ) 
  plot_ylab = ylab("SCC (2020 USD)") 
} else{
    plot_labs = labs(
      title="Income at which direct donation is preferable to $10/ton CO2",
      fill="Inequality aversion"
    )
    plot_ylab = ylab("Yearly income (2020 USD)") 
  }

set.seed(10)
explain_eta = function(et){paste0("RRA: ", et)}
results_table$SSP <- factor(results_table$ssp)
plot = ggplot(results_table, aes(x=rcp, y=value, color=SSP))+geom_point(
  alpha=0.9, size=2
) + plot_labs + plot_ylab + facet_wrap(~damages+eta, labeller = labeller(
  damages=c("Burke"="Damage fn: Burke", "DJO"="Damage fn: Dell"), eta=explain_eta
)
                                       )
if (type_str != "eri_eq_statscc_2020d"){
  plot = plot + geom_hline(yintercept=1.9*365)
}

plot
savefig = paste0(type_str, version_string, ".png")
ggsave(path="plots", filename=savefig)
