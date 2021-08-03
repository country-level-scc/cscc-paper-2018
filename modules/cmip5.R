#
library(data.table)
library(countrycode)
library(foreach)
library(stringr)

# if doing a test:
if ((test == TRUE) & !dir.exists("data/cmip5/RegionalSCC_rcpfits_Test")){
  source("modules/generate_test_input.R")
}

if (test == TRUE) {
  # Load popweighted country with baseline temperature for the test files
  files = Sys.glob(file.path("data","cmip5","RegionalSCC_rcpfits_Test","pop*.csv"))
} else {
  #Load popweighted country temperature increase
  files = Sys.glob(file.path("data","cmip5","RegionalSCC_rcpfits","pop*.csv"))
}

all_ctemp = list()

for (f in files) {
  
  # Load sample temp from one model [temperatures have to be adjusted to baseline]
  temp = fread(input = f, header = F, skip = 0)
  # remove bad lines in RCP45
  temp = temp[V3!=1]
  colnames(temp) <- c("FAO","Country",paste(2006:2105))
  ctemp = melt(temp,id.vars=c("Country"),measure.vars=paste(2006:2105),
               variable.name="year",value.name="temperature",
               variable.factor = F)
  ctemp[, year := as.numeric(year)]
  ctemp[, rcp := str_sub(f,-9,-5)]
  ctemp[, model := str_split(basename(f),"_")[[1]][3]]
  
  # Check Afganistan [only for ACCESS-1]
  if(nrow(ctemp[Country=="Afghanistan"])==0){
    .temp = ctemp[Country=="Pakistan"]
    .temp$Country="Afghanistan"
    ctemp = rbind(ctemp,.temp)
  }
  
  # Check Sudan
  if(nrow(ctemp[Country=="Sudan"])==0){
    .temp = ctemp[Country=="Chad"]
    .temp$Country="Sudan"
    ctemp = rbind(ctemp,.temp)
  }
  
  # Check Serbia
  if(nrow(ctemp[Country=="Serbia"])==0){
    .temp = ctemp[Country=="Bosnia and Herzegovina"]
    .temp$Country="Serbia"
    ctemp = rbind(ctemp,.temp)
  }
  
  # Check Western Sahara
  if(nrow(ctemp[Country=="Western Sahara"])==0){
    .temp = ctemp[Country=="Mauritania"]
    .temp$Country="Western Sahara"
    ctemp = rbind(ctemp,.temp)
  }
  
  all_ctemp = c(all_ctemp,list(ctemp))
  ctemp
}
all_ctemp = rbindlist(all_ctemp)
rm(temp,ctemp)

all_countries <- unique(all_ctemp$Country)
all_iso3 <- countrycode(all_countries, "country.name", "iso3c")

ctemp <- merge(all_ctemp,data.table(Country=all_countries, ISO3=all_iso3),by=c("Country"))
rm(all_ctemp)

ctemp = ctemp[,list(model,rcp,ISO3,year,temp=temperature)]

setkey(ctemp,rcp,ISO3,year)

# Expected temperature
etemp = ctemp[,.(temp=mean(temp)),by=c("rcp","ISO3","year")]

# BASELINE TEMPERATURE (annual average popweighted temperature observed 1980-2010)

basetemp <- fread(file.path("data","cmip5","popweightcountry_1980_2010_obsbaseline.csv"), header = F, skip = 0)
colnames(basetemp) <- c("FAO","Country","temp")
basetemp <- merge(basetemp, data.table(Country=all_countries, ISO3=all_iso3),by=c("Country"),all=T)
basetemp[Country=="Sudan",temp:=basetemp[Country=="Chad",temp]]
basetemp[Country=="Serbia",temp:=basetemp[Country=="Bosnia and Herzegovina",temp]]
basetemp[Country=="Western Sahara",temp:=basetemp[Country=="Mauritania",temp]]
basetemp[Country=="Mongolia",temp:=ctemp[ISO3=="MNG" & year==2006,.(quantile(temp,prob=0.125))]$V1] # Missing Mongolia in basetemp dataset
basetemp = basetemp[!is.na(temp)]

basetemp[, ISO3 := countrycode(Country, "country.name", "iso3c")]
basetemp = basetemp[,list(ISO3,basetemp=temp)]
