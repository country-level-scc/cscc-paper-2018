library(foreach)
library(stringr)
library(data.table)
library(dplyr)

files <- Sys.glob("data/pulse/RegionalSCC_pulseuncertainty/*.csv")

subDir = "data/pulse/RegionalSCC_pulseuncertainty_Test"
if (!dir.exists(file.path(subDir))){
  dir.create(file.path(subDir))
}

for (f in files) {
  # Load sample temp from one model [temperatures have to be adjusted to baseline]
  pulse_data <- fread(f)
  start_model = as.data.frame(str_locate_all(pattern="fromfit_", f)) # start specific name at the variable for model where fromfit_ ends
  name_model =  substr(f, start = start_model[[2]][1]+1, stop = nchar(f)-4)
  for (i in 1:nrow(pulse_data)){
    for (k in 4:ncol(pulse_data)){
      pulse_data[[k]][i] <- 0
    }
    #if (pulse_data[[2]][i] == "Afghanistan"){
     # temp_change <- 1
      #pulse_data[[4]][i] <- temp_change
    #}
  }
  write.table(pulse_data, file = file.path("data","pulse","RegionalSCC_pulseuncertainty_Test",
                                              paste0("popweightcountry", "_fromfit", "_", name_model,".csv")),row.names=FALSE, sep =",")
}  

