# DJO damage function
require(MASS)
#Estimates
#  wtem=0.2609434 (0.3121942) # not significant
#  wtem_initxtilegdp1=-1.655145 (0.4853579) # ***
#Variance:
#  wtem=0.09746524
#  wtem_initp1=0.23557231
#CoVariance:
#  wtem,wtem_initxtilegdp1=-0.08330767

# Model
#  annual_growth_rate = wtemp*Temp + wtem_initxtilegdp1*dummy_poor_country

mu = c(0,-1.655145)
mb = matrix(mu,nrow=1)

g_rich <- function(temp, rid) { return(0)  }
g_poor <- function(temp, rid) { return((0.2609434-1.655145) * temp) }

if(F){
  xx = seq(-0.5,1.5,by=0.1)
  plot(xx,g_poor(xx,0),type="l",col="red")
  for(i in 1:500){
    lines(xx,g_poor(xx,i),col="blue")
  }
  lines(xx,g_poor(xx,0),type="l",col="red")
  
  xx = seq(-0.5,1.5,by=0.1)
  plot(xx,g_rich(xx,0),type="l",col="red")
  for(i in 1:500){
    lines(xx,g_rich(xx,i),col="blue")
  }
  lines(xx,g_rich(xx,0),type="l",col="red")
}

warming_effect <- function(temp, temp_tm1, gdpcap_tm1, runid, out_of_sample=T){
  if(gdpcap_tm1>Y_STAR){
    return(g_rich(temp-temp_tm1,runid))
  } else {
    return(g_poor(temp-temp_tm1,runid))
  }
}


# get GDP per capita* separating between poor and rich
if(!file.exists(file.path("data","historical_gdp.RData"))){
  # get YSTAR from WDI
  library(WDI)
  library(data.table)
  hyear=1980
  #Load historical GDP per capita
  #"NY.GDP.MKTP.KN"
  #"GDP, PPP (constant 2005 international $)"
  res = data.table(WDI(indicator="NY.GDP.MKTP.KN", start=hyear, end=hyear, extra=T))
  hgdp = res[!region %in% c("Aggregates") & !is.na(iso3c),list(country,iso3=iso3c,gdp=NY.GDP.MKTP.KN)]
  #"SP.POP.TOTL"
  #"Population, total"
  res = data.table(WDI(indicator="SP.POP.TOTL", start=hyear, end=hyear, extra=T))
  hpop = res[!region %in% c("Aggregates") & !is.na(iso3c),list(country,iso3=iso3c,pop=SP.POP.TOTL)]
  
  h_gdpcap = merge(hgdp,hpop,by=c("country","iso3"))
  h_gdpcap[, gdpcap:=gdp/pop]
  h_gdpcap = h_gdpcap[!is.na(gdpcap)]
  
  Y_STAR <- median(h_gdpcap$gdpcap,na.rm = T) # 20715.44
  
  save(h_gdpcap, Y_STAR, file = file.path("data","historical_gdp.RData"))
} else {
  load(file.path("data","historical_gdp.RData"))
}
