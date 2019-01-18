# Burke et al. damage function
require(data.table)

if (rich_poor) {
  
  if (lag5) {
    # use Rich/Poor 5-lag specification 
    pb = fread("data/BurkeHsiangMiguel2015_Replication/data/output/bootstrap/bootstrap_richpoor_5lag.csv")
    mb = as.matrix(pb[,.(tlin,tlinpoor,tsq,tsqpoor)])
  } else {
    # use Rich/Poor no lag specification 
    pb = fread("data/BurkeHsiangMiguel2015_Replication/data/output/bootstrap/bootstrap_richpoor.csv")
    mb = as.matrix(pb[,.(temp,temppoor,temp2,temp2poor)])
  }

  # temp: projected annual average temperature in the country in any year after 2010
  # temp_baseline: average temperature in the country between 1980-2010 (base period)
  # gdpcap_tm1: GDP/capita in the previous year
  g_rich <- function(temp, rid) { return(mb[rid + 1,1] * temp + mb[rid + 1,3] * temp^2) }
  g_poor <- function(temp, rid) { return(mb[rid + 1,2] * temp + mb[rid + 1,4] * temp^2) }
  warming_effect <- function(temp, temp_baseline, gdpcap_tm1, runid, out_of_sample=T){
    if (!out_of_sample) {
      if (temp > 30) {temp <- 30} # Does not allow for out-of-sample prediction
    }
    if (gdpcap_tm1 > Y_STAR) {
      return(g_rich(temp,runid) - g_rich(temp_baseline,runid))
    } else {
      return(g_poor(temp,runid) - g_poor(temp_baseline,runid))
    }
  }
  
} else {
  
  if (lag5) {
    # pooled bootstrap_5Lag 
    pb = fread("data/BurkeHsiangMiguel2015_Replication/data/output/bootstrap/bootstrap_5Lag.csv")
    mb = as.matrix(pb[,.(tlin,tsq)])
  } else {
    # pooled bootstrap_noLag 
    pb = fread("data/BurkeHsiangMiguel2015_Replication/data/output/bootstrap/bootstrap_noLag.csv")
    mb = as.matrix(pb[,.(temp,temp2)])
  }
  
  g_pool <- function(temp, rid) { return(mb[rid + 1,1] * temp + mb[rid + 1,2] * temp^2) }
  warming_effect <- function(temp, temp_baseline, gdpcap_tm1, rid, out_of_sample=T){
    if (!out_of_sample) {
      if (temp > 30) {temp <- 30} # Does not allow for out-of-sample prediction
    }
    return(g_pool(temp,rid) - g_pool(temp_baseline,rid))
  }

}

# get GDP per capita separating between poor and rich
if(!file.exists(file.path("data","historical_gdp_burke.RData"))){

# Y_STAR from BHM replication code
library(haven)
GrowthClimateDataset <- read_dta("data/BurkeHsiangMiguel2015_Replication/data/input/GrowthClimateDataset.dta")
gdpCap = GrowthClimateDataset$TotGDP/GrowthClimateDataset$Pop
dta <- data.frame(GrowthClimateDataset,gdpCap)
dta <- data.table(dta)

mt <- dta[year>=1980 & is.na(UDel_temp_popweight)==F & is.na(growthWDI)==F,
          .(gdpCap = mean(gdpCap,na.rm=T)),
          by=c("iso")]

Y_STAR_BHM <- median(mt$gdpCap) # 2268.528 [USD2005]

save(Y_STAR_BHM, file = file.path("data","historical_gdp_burke.RData"))
  
} else {
  load(file.path("data","historical_gdp_burke.RData"))
}

Y_STAR <- Y_STAR_BHM