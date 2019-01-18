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

g_rich <- function(temp, rid) { return(-0.191 / 100 * temp) }
g_poor <- function(temp, rid) { return(-1.041 / 100 * temp) }

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


# get GDP per capita separating between poor and rich
if(!file.exists(file.path("data","historical_gdp_djo.RData"))){
  
  # Y_STAR from DJO replication code
  # Poor is defined as a dummy for a country having below median PPP GDP  per capita in its first year in the data.
  library(haven)
  climate_panel <- read_dta("data/AEJ Mac2010-0092-reproduction/climate_panel.dta")
  climate_panel <- data.table(climate_panel)
  
  Y_STAR_DJO <- median(climate_panel[year == 1950,exp(lnrgdpl_t0)], na.rm = T) # 2449.36
  
  save(Y_STAR_DJO, file = file.path("data","historical_gdp_djo.RData"))
  
} else {
  load(file.path("data","historical_gdp_djo.RData"))
}

Y_STAR <- Y_STAR_DJO