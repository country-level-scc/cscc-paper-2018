library(ggplot2)
library(stringr)
library(docopt)
library(data.table)

# This file plots results from the generate_cscc.R file
# The outputs can be either "eri_eq_statscc_2020d" (GSCC relative to the income of an Eritrean),
# or "poor_pref_10dollars" (income at which donation is preferable to SCC) 

# Input values in options should correspond with files in the results folder for -e, -r and -f.

' Input values to be plotted. Use values also used to generate files in the generate_cscc.r results folder

usage: plot_equality_data.R [-e <eta> -v <ver> -s <string> -r <rcp> -f <dmg>]

options:
 -e Eta value used in the model (1, 2 or 1:2 (default))
 -v Version number to name the plotted figure
 -s Type of string and plot (either poor_pref_10dollars (default) or eri_eq_statscc_2020d)
 -r Plot rcp scenario (4.5, 6.0, 8.5 or all (default))
 -f Damage function (default=bhm (Burke et al.), djo (Dell et al.). separate by columns)' -> my_doc

#my_opts <- docopt(my_doc, "-e 1 -v v4 -s poor_pref_10dollars -r 6.0,4.5,8.5 -f bhm") # Default case
#my_opts <- docopt(my_doc, "-e 1,2 -v v2 -s eri_eq_statscc_2020d -r 8.5 -f bhm,djo") 
my_opts <- docopt(my_doc, "-e 1, -s eri_eq_statscc_2020d -r 8.5 -f djo") 
#my_opts <- docopt(my_doc)

# unpack variables from the options
if (is.null(my_opts[["e"]])){ # default RRA is 1 and 2
  variable_risk = c(1,2)
} else if (my_opts[["e"]] == 1) {
  variable_risk = c(1)
} else if (my_opts[["e"]]==2){
  variable_risk = c(2)
} else {variable_risk = c(1,2)}

if (is.null(my_opts[["v"]])) {
  version_string = "" # no version number if there is no input for -v
} else{ version_string = as.character(my_opts[["v"]])
}

if (is.null(my_opts[["s"]])){
  type_str = "poor_pref_10dollars" #default
} else{type_str = as.character(my_opts[["s"]])}  

variable_rcp = c()
if (length(grep(4.5, my_opts[["r"]]) != 0)){
  variable_rcp <- append(variable_rcp, 45)
} 
if (length(grep(6.0, my_opts[["r"]]) != 0)){
  variable_rcp <- append(variable_rcp, 60)
} 
if (length(grep(8.5, my_opts[["r"]])) != 0){
  variable_rcp <- append(variable_rcp, 85)
} 
if (length(variable_rcp) == 0){variable_rcp = c(45,60,85)}

if (is.null(my_opts[["f"]])) {
  dmg_f = "bhm"     # bhm is default damage function
} else {
  dmg_f = as.character(my_opts["f"])
}

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

if (length(grep( "djo", dmg_f))!=0){
  for (y in variable_rcp){
    for (z in variable_risk){
      filelist2 = lapply(c(1:5), namefun_altdamage, y=y, z=z)
      filelist=c(filelist, filelist2)
    }
  }
}  

if (length(grep("bhm", dmg_f))!=0){ 
  for (y in variable_rcp){
    for (z in variable_risk){
      for (timeframe in c("horizon2100", "constant")){
        filelist2 = lapply(c(1:5), namefun, y=y, z=z, timeframe=timeframe) 
        filelist=c(filelist, filelist2)
      }
    }
  }
  for (y in variable_rcp){
    for (z in variable_risk){
      for (timeframe in c("horizon2100", "constant")){
        filelist2 = lapply(c(1:5), namefun_nocut, y=y, z=z, timeframe=timeframe) 
        filelist=c(filelist, filelist2)
      }
    }
  }
}

results_table <- data.table(ssp=integer(), rcp=numeric(), eta=numeric(), PRTP=numeric(), damages=character(), indicator=character(), value=numeric())
columns_to_save = c("mean")
origin = getwd()
compare_results = results_table
for (file in filelist) {
  #load(paste0(origin, file))
  load("results/res_statdjo_richpoor/Test_raw_scc_SSP3_rcp85_constant_estimates_climensemble_djo.RData")
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

subDir = "plots"
if (!dir.exists(file.path(subDir))){
  dir.create(file.path(subDir))
}

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
