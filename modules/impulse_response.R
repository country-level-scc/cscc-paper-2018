# Impulse response [mK/GtC]
library(data.table)
library(countrycode)
library(foreach)
library(stringr)

# if doing a test:
if (test == TRUE) {
  if ((test_opt == "t1") & (!dir.exists("data/cmip5/RegionalSCC_rcpfits_t1"))){
    source("modules/generate_test_pulse_input.R")
  } else if ((test_opt == "t0") & (!dir.exists("data/cmip5/RegionalSCC_rcpfits_t0"))){
    source("modules/generate_test_pulse_input.R")
  }
}

if (test == TRUE){
  if (test_opt == "t0"){
    files <- Sys.glob("data/pulse/RegionalSCC_pulseuncertainty_t0/*.csv")
  } else if (test_opt == "t1"){
    files <- Sys.glob("data/pulse/RegionalSCC_pulseuncertainty_t1/*.csv")
  }
} else {files <- Sys.glob("data/pulse/RegionalSCC_pulseuncertainty/*.csv")}  
  
all_pulse = foreach(f=files) %do% {
  # Load sample temp from one model [temperatures have to be adjusted to baseline]
  pulse = fread(input = f, header = T)
  pulse = melt(pulse, id.vars = c("FAO", "Country", "C Model"), 
               variable.name = "mid_year", variable.factor = F, value.name = "temp_pulse")
  pulse[, mid_year := as.numeric(mid_year)]
  setnames(pulse,"C Model", "ccmodel")
  pulse[, model := str_sub(str_split(basename(f),"_")[[1]][3],1,-12) ]
  
  # Check Sudan
  if(nrow(pulse[Country=="Sudan"])==0){
    .temp = pulse[Country=="Chad"]
    .temp$Country="Sudan"
    pulse = rbind(pulse,.temp)
  }
  
  # Check Serbia
  if(nrow(pulse[Country=="Serbia"])==0){
    .temp = pulse[Country=="Bosnia and Herzegovina"]
    .temp$Country="Serbia"
    pulse = rbind(pulse,.temp)
  }
  
  # Check Western Sahara
  if(nrow(pulse[Country=="Western Sahara"])==0){
    .temp = pulse[Country=="Mauritania"]
    .temp$Country="Western Sahara"
    pulse = rbind(pulse,.temp)
  }
  
  # Check Mongolia
  if(nrow(pulse[Country=="Mongolia"])==0){
    .temp = pulse[Country=="China"]
    .temp$Country="Mongolia"
    pulse = rbind(pulse,.temp)
  }
  pulse
  
}
all_pulse <- rbindlist(all_pulse)
rm(pulse)

all_countries <- unique(all_pulse$Country)
all_iso3 <- countrycode(all_countries, "country.name", "iso3c")

cpulse <- merge(all_pulse,data.table(Country=all_countries, ISO3=all_iso3),by=c("Country"))
rm(all_pulse)

cpulse = cpulse[,list(model,ccmodel,ISO3,mid_year,temp_pulse)]

setkey(cpulse,ISO3,mid_year)

epulse = cpulse[,list(temp_pulse=mean(temp_pulse)), by=c("mid_year","ISO3")]
