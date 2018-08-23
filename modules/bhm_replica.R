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

# get GDP per capita* separating between poor and rich
if (!file.exists(file.path("data","historical_gdp.RData"))) {
  # get YSTAR from WDI
  library(WDI)
  library(data.table)
  hyear <- 1980
  #Load historical GDP per capita
  #"NY.GDP.MKTP.KN"
  #"GDP, PPP (constant 2005 international $)"
  res <- data.table(WDI(indicator = "NY.GDP.MKTP.KN", start = hyear, end = hyear, extra = T))
  hgdp <- res[!region %in% c("Aggregates") & !is.na(iso3c),
              .(country,iso3 = iso3c,gdp = NY.GDP.MKTP.KN)]
  #"SP.POP.TOTL"
  #"Population, total"
  res <- data.table(WDI(indicator = "SP.POP.TOTL", start = hyear, end = hyear, extra = T))
  hpop <- res[!region %in% c("Aggregates") & !is.na(iso3c),
              .(country,iso3 = iso3c,pop = SP.POP.TOTL)]
  
  h_gdpcap <- merge(hgdp,hpop,by = c("country","iso3"))
  h_gdpcap[, gdpcap := gdp/pop]
  h_gdpcap <- h_gdpcap[!is.na(gdpcap)]
  
  Y_STAR <- median(h_gdpcap$gdpcap,na.rm = T) # 20715.44
  
  save(h_gdpcap, Y_STAR, file = file.path("data","historical_gdp.RData"))
} else {
  load(file.path("data","historical_gdp.RData"))
}
