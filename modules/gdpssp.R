# data ssp
require(data.table)
library(stringr)
library(countrycode)
library(pracma)

# Load SSP database
ssp_csv = file.path('data','SspDb_country_data_2013-06-12.csv')
sspdb = fread(ssp_csv, header = T)
sspdb = sspdb[VARIABLE %in% c("Population","GDP|PPP") & MODEL %in% c("OECD Env-Growth")]
sspdb = melt(sspdb,
           id.vars = 1:5,
           measure.vars = paste(seq(2000,2100,by = 5)),
           variable.name = "year",
           na.rm = T,
           variable.factor = F)
setnames(sspdb,"REGION","ISO3")
sspdb[, year := as.numeric(year)]
sspdb[, Country := countrycode(ISO3, "iso3c", "country.name")]
sspdb[, SSP := str_extract(SCENARIO,"SSP\\d")]

# Load gdp and population

gdp = sspdb[VARIABLE %in% c("GDP|PPP"), .(SSP,ISO3,year,gdp=value)]
pop = sspdb[VARIABLE %in% c("Population"), .(SSP,ISO3,year,pop = value)]

# Compute gdp per capita

gdpcap = merge(gdp,pop,by = c("ISO3","year","SSP"))
gdpcap[, gdpcap := gdp/pop*1e3]
sspgdpcap = gdpcap[,list(SSP,ISO3,year,gdpcap)]

# Compute annual growth rate
annual_gdpg <- function(sd){
  g_years <- seq(sd$year[1], sd$year[length(sd$year)], by = 1)
  annual_gdpcap <- approx(sd$year, sd$gdpcap, g_years)$y
  annual_gdpcap <- interp1(sd$year, sd$gdpcap, g_years, method = "spline")
  gdpg <- annual_gdpcap[2:(length(g_years))]/annual_gdpcap[1:(length(g_years) - 1)] - 1
  return(list(year = g_years[2:(length(g_years))],gdpr = gdpg))
}

annual_gdp <- function(sd){
  g_years <- seq(sd$year[1], sd$year[length(sd$year)], by = 1)
  annual_gdp <- approx(sd$year, sd$gdp, g_years)$y
  annual_gdp <- interp1(sd$year, sd$gdp, g_years, method = "spline")
  gdp <- annual_gdp[2:(length(g_years))]
  return(list(year = g_years[2:(length(g_years))],gdp = gdp))
}

growthrate = gdpcap[, annual_gdpg(.SD), by = c("SSP","ISO3")]
gdp_yearly = gdp[, annual_gdp(.SD), by = c("SSP","ISO3")]

# World level
wgdp = gdp[,.(gdp=sum(gdp)),by=c("year","SSP")]
wpop = pop[,.(pop=sum(pop)),by=c("year","SSP")]
wgdpcap = merge(wgdp,wpop,by=c("year","SSP"))
wgdpcap[, gdpcap:=gdp/pop*1e3]
wgdpcap.2020 = wgdpcap[year==2020,.(gdpcap.2020=gdpcap),by="SSP"]
wgdpcap = merge(wgdpcap,wgdpcap.2020, by="SSP")

wgdpcap[, gdprate_avg := ifelse(year<=2020,NA,(gdpcap/gdpcap.2020)^(1/(year-2020))-1) ]
