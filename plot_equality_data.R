library(data.table)
library(ggplot2)
library(stringr)
# Should be either "eri_eq_statscc_2020d", "poor_pref_10dollars" 
type_str = "poor_pref_10dollars" 
# Name the file with the version number
version_string = "v4"

# Conversion factor
dollar_val_2020 = 1.35

namefun <- function(x, y, z, timeframe){
    paste0("/results/res_statbhm_30C/", type_str, "SSP", x, "_rcp", y, 
           "_", timeframe, "_estimates_climensemble_", "eta_", z, ".RData") 
}
namefun_altdamage <- function(x, y, z){
  paste0("/results/res_statdjo_richpoor/", type_str, "SSP", x, "_rcp", y,
  "_constant_estimates_climensemble_djo_", "eta_", z, ".RData")
}
namefun_nocut <- function(x, y, z, timeframe){
  paste0("/results/res_statbhm/", type_str, "SSP", x, "_rcp", y, 
         "_", timeframe, "_estimates_climensemble_", "eta_", z, ".RData") 
}

filelist=c()
 
for (y in c(45, 60, 85)){
  for (z in c(1, 2)){
    filelist2 = lapply(c(1:5), namefun_altdamage, y=y, z=z)
    filelist=c(filelist, filelist2)
  }
}
for (y in c(45, 60, 85)){
  for (z in c(1, 2)){
    for (timeframe in c("horizon2100", "constant")){
      filelist2 = lapply(c(1:5), namefun, y=y, z=z, timeframe=timeframe) 
      filelist=c(filelist, filelist2)
    }
  }
}
for (y in c(45, 60, 85)){
  for (z in c(1, 2)){
    for (timeframe in c("horizon2100", "constant")){
      filelist2 = lapply(c(1:5), namefun_nocut, y=y, z=z, timeframe=timeframe) 
      filelist=c(filelist, filelist2)
    }
  }
}

results_table <- data.table(ssp=integer(), rcp=numeric(), eta=numeric(), PRTP=numeric(), damages=character(), indicator=character(), value=numeric())
columns_to_save = c("mean")
origin = getwd()
compare_results = results_table
for (file in filelist) {
  load(paste0(origin, file))
  sspnum = as.numeric(substr(strsplit(file, split = "SSP")[[1]][2], 1, 1))
  rcpnum = substr(strsplit(file, split = "rcp")[[1]][2], 1, 2)
  rcpnum = sub("(.{1})(.*)", "\\1.\\2", rcpnum)
  damages = if(grepl("djo", file)) "Dell" else (if (grepl("horizon2100", file)) "Burke 2100" else "Burke 2200")
  if (grepl("_30C", file)){
    damages = str_c(damages, " 30C")
  }
  for (column in columns_to_save){
    if(type_str == "eri_eq_statscc_2020d"){
      use_table = eri_eq_stat_wscc
      # We can also compare the impact of inequality aversion
      compare_file = str_replace(file, "eri_eq_statscc_2020d", "statscc_")
      load(paste0(origin, compare_file))
      compare_table = stat_scc
    } else {
      use_table = poor_prefer_10
    }
    rows_to_save = lapply(use_table$ID, strsplit, split="_")
    rowind = 0
    for (row in rows_to_save){
      rowind = rowind + 1
      if (row[[1]][1] %in% c("NA", "3") ) next
      PRTPnum = row[[1]][1]
      etanum = row[[1]][2]
      results_table <- rbind(results_table, list(
        sspnum, rcpnum, etanum, PRTPnum, damages, column, use_table[[rowind, column]])
      )
      if(type_str == "eri_eq_statscc_2020d"){
        compare_results <- rbind(
          compare_results, list(sspnum, rcpnum, etanum, PRTPnum, damages, column, compare_table[[
            which(compare_table$ID == paste(PRTPnum, etanum, "NA", "WLD", sep="_")), column
          ]])
        )
      }
    }
  }
}
compare_results$value = compare_results$value * dollar_val_2020

if(type_str == "eri_eq_statscc_2020d"){
  plot_labs = labs(
    title="World SCC relative to the income of an Eritrean",
    fill="Inequality aversion"
  ) 
  plot_ylab = ylab("SCC (2020 USD)") 
} else if(type_str == "poor_pref_10dollars"){
    plot_labs = labs(
      title="Income at which direct donation is preferable to $10/ton CO2",
      fill="Inequality aversion"
    )
    plot_ylab = ylab("Yearly income (2020 USD)") 
}

set.seed(10)
explain_eta = function(et){paste0("RRA: ", et)}
explain_prtp = function(pr){paste0("PRTP: ", pr)}
label_axes = labeller(
  eta=explain_eta, PRTP=explain_prtp
)
results_table$SSP <- factor(results_table$ssp)
plot = ggplot(results_table, aes(x=rcp, y=value, color=SSP))+geom_point(
  alpha=0.9, size=2
) + plot_labs + plot_ylab + xlab("RCP pathway") + facet_grid(PRTP+eta~damages , labeller = label_axes
)
if (type_str != "eri_eq_statscc_2020d"){
  plot = plot + geom_hline(yintercept=1.9*365)
  plot = plot + geom_hline(yintercept=500, color="red")
  plot = plot + scale_y_continuous(trans = "log2")
} else {
  plot = plot + geom_hline(yintercept=10)
  plot = plot + scale_y_continuous(trans = "log2")
}

plot
savefig = paste0(type_str, version_string, ".png")
ggsave(path="plots", filename=savefig)

if (type_str == "eri_eq_statscc_2020d"){
  combined_data = results_table
  combined_data$ratio = compare_results$value / results_table$value
  combined_data$no_ineq = compare_results$value
  plotdif = ggplot(combined_data, aes(x=value, y=no_ineq, color=SSP, shape=rcp)) + geom_point(
    alpha=0.9, size=3
  ) + facet_grid(PRTP+eta~damages, labeller = label_axes) + 
    scale_y_continuous(trans = "log2") + scale_x_continuous(trans = "log2") + ylab(
    "SCC inequality-indifferent value") + xlab("SCC inequality-averse value for Eritrea")
  plotdif
  savediffig = paste0(type_str, "InequalityAlteration", version_string, ".png")
  ggsave(path="plots", filename=savediffig)
}
