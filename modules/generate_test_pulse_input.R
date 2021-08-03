library(foreach)
library(stringr)
library(data.table)
library(dplyr)

t0_pulse <- function(files) {
  for (f in files) {
    # Load sample temp from one model
    pulse_data <- fread(f)
    # start specific name at the variable for model where fromfit_ ends
    start_model = as.data.frame(str_locate_all(pattern="fromfit_", f)) 
    name_model =  substr(f, start = start_model[[2]][1]+1, stop = nchar(f)-4)
    # set all temp pulses to 0
    for (i in 1:nrow(pulse_data)){
      for (k in 4:ncol(pulse_data)){
        pulse_data[[k]][i] <- 0
      }
    }
    write.table(pulse_data, file = file.path(subDir, paste0("popweightcountry", "_fromfit", "_", name_model,".csv")),
      row.names=FALSE, sep =",")
  }
}

t1_pulse <- function(files) {
  for (f in files) {
    # Load sample temp from one model
    pulse_data <- fread(f)
    # start specific name at the variable for model where fromfit_ ends
    start_model = as.data.frame(str_locate_all(pattern="fromfit_", f)) 
    name_model =  substr(f, start = start_model[[2]][1]+1, stop = nchar(f)-4)
    # set all temperature pulses to 0
    for (i in 1:nrow(pulse_data)){
      for (k in 4:ncol(pulse_data)){
        pulse_data[[k]][i] <- 0
      }
      # set temperature pulse of Angola in the year 2021 to 1
      if (pulse_data[[2]][i] == "Angola"){
        temp_change <- 1
        pulse_data[[5]][i] <- temp_change # change pulse for year 2021
      }
    }
    write.table(pulse_data, file = file.path(subDir, paste0("popweightcountry", "_fromfit", "_", name_model,".csv")),
                row.names=FALSE, sep =",")
  }
}

files <- Sys.glob("data/pulse/RegionalSCC_pulseuncertainty/*.csv")

if (test_opt == "t1"){
  subDir = "data/pulse/RegionalSCC_pulseuncertainty_t1"
  if (!dir.exists(file.path(subDir))){
    dir.create(file.path(subDir))
  }
  t1_pulse(files)
} else if (test_opt == "t0") {
  subDir = "data/pulse/RegionalSCC_pulseuncertainty_t0"
  if (!dir.exists(file.path(subDir))){
    dir.create(file.path(subDir))
  } 
  t0_pulse(files)
}
