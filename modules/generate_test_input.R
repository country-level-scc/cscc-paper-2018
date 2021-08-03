library(countrycode)
library(foreach)
library(stringr)
library(data.table)
library(dplyr)

# Load popweighted country temperatureincrease
files = Sys.glob(file.path("data","cmip5","RegionalSCC_rcpfits","pop*.csv"))

# load basetemp to set all temperatures in the files equal to to test for no T increase
basetemp <- fread(file.path("data","cmip5","popweightcountry_1980_2010_obsbaseline.csv"), header = F, skip = 0)
names(basetemp)[2] <- "Country"
names(basetemp)[3] <- "temp"
basetemp[Country=="Sudan",temp:=basetemp[Country=="Chad",temp]]
basetemp[Country=="Serbia",temp:=basetemp[Country=="Bosnia and Herzegovina",temp]]
basetemp[Country=="Western Sahara",temp:=basetemp[Country=="Mauritania",temp]]
basetemp[Country=="Mongolia",temp:=ctemp[ISO3=="MNG" & year==2006,.(quantile(temp,prob=0.125))]$V1] # Missing Mongolia in basetemp dataset
basetemp = basetemp[!is.na(temp)]
basetemp.list <- as.list(as.data.frame(t(basetemp)))

# check if directory to store files in does exist already, otherwise make a new one
subDir = "data/cmip5/RegionalSCC_rcpfits_Test"
if (!dir.exists(file.path(subDir))){
  dir.create(file.path(subDir))
}

for (f in files) {
  # Load sample temp from one model [temperatures have to be adjusted to baseline]
  temperature_data <- fread(f)
  # save RCP for more sensible name in Test file
  start_model = as.data.frame(str_locate_all(pattern="fromfit_", f)) # start specific name at the variable for model where fromfit_ ends
  # use end of general name +1 to mark the start of the model and end before .csv
  name_rcp =  substr(f, start = start_model[[2]][1]+1, stop = nchar(f)-4) 
  for (i in 1:nrow(temperature_data)){ # k is the row in the data.frame
    country <- temperature_data[[2]][i]
    country_in_list = which(sapply(basetemp.list, function(y) country %in% y))
    if (length(country_in_list) != 0){
      base_T <- basetemp[Country==country, temp][1]
      for (k in 3:ncol(temperature_data)){
        temperature_data[[k]][i] <- base_T
      }
    }
  }
  temperature_data
  write.table(temperature_data, file = file.path("data","cmip5","RegionalSCC_rcpfits_Test",
      paste0("popweightcountry", "_fromfit", "_", name_rcp,".csv")), col.names = FALSE,row.names=FALSE, sep =",")
}
