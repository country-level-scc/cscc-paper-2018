# Compute national contributions to social cost of carbon [READ USAGE BELOW]
#
# Any questions, cooments: laurent.drouet@feem.it
#

library(data.table)

library(docopt)
'usage: generate_cscc.R [-s <ssp> -c <rcp> -r <runid> -p <type> -l <clim>] [-o] [-d] [-w]

options:
 -s <ssp>   SSP baseline (random(default), SSP1, SSP2,..., SSP5)
 -c <rcp>   RCP (random(default), rcp45, rcp60, rcp85)
 -r <runid> Bootstart run for the damage function parameter, 0 is estimates (0<=id<=1000)
 -p <type>  projection type (constant (default),horizon2100)
 -l <clim>  climate models (ensemble (default), mean[-ensemble])
 -o         does not allow for out-of-sample damage prediction (default, allows)
 -d         rich/poor damage function specification (default, pooled)
 -f <name>  damage function (default=bhm (Burke et al.), djo(Dell et al.))
 -w         save raw data' -> doc
#opts <- docopt(doc, "-s SSP2 -c rcp60 -l mean -r 1 -d")
opts <- docopt(doc)

t0 <- Sys.time()

# GLOBAL VARIABLES
if(is.null(opts[["s"]])){
  ssp = sample(paste0("SSP",1:5),1) # SSP{1,2,3,4,5}
} else {
  ssp = as.character(opts["s"])
}
if(is.null(opts[["c"]])){
  .rcp = sample(c("rcp45","rcp60","rcp85"),1)
} else {
  .rcp = as.character(opts["c"]) 
}
if(is.null(opts[["r"]])){
  dmg_func = "estimates" # dmg function
  runid = 0
} else {
  print(paste("r:",opts['r']))
  runid = as.integer(max(0,min(1000,as.numeric(opts['r']))))
  if(runid==0){
    dmg_func = "estimates" # dmg function
  }else{
    dmg_func = paste0("bootstrap") # dmg function
  }
}
if(is.null(opts[["p"]])){
  project_val = "constant" # growth rate constant
} else {
  #project_val = "horizon2100" # stop the computation after 2100
  #project_val = "constant" # constant time-dependent compound after 2100
  project_val = as.character(opts["p"])
}
if(is.null(opts[["l"]])){
  clim = "ensemble"
} else {
  clim = "mean"
  if(runid!=0){
    dmg_func = "bootstrap"
    runid = 1:1000
  }
}
if(is.null(opts[["f"]])){
  dmg_ref = "bhm"
} else {
  dmg_ref = as.character(opts["f"])
}
out_of_sample = !opts[['o']]
rich_poor = opts[['d']]
save_raw_data = opts[['w']]
very_last_year = 2200
impulse_year = 2020

if(dmg_func=="djo"){
  rich_poor = T
  out_of_sample = T
  dmg_func = "estimates"
}

# Print values
print(paste("SSP: ",ssp))
print(paste("RCP: ",.rcp))
print(paste("dmg_func: ",dmg_func))
print(paste("last year: ",very_last_year))
print(paste("climate ensemble: ",clim))
print(paste("impulse year: ",impulse_year))
print(paste("projection post2100: ",project_val))
print(paste("out_of_sample: ",out_of_sample))
print(paste("richpoor: ",rich_poor))

print(paste("damage function:",dmg_ref))

if(dmg_ref=="bhm"){
  dmg_ref=""
}else{
  dmg_ref=paste0("_",dmg_ref)
}

resdir = paste0("res_stat",dmg_ref)
if(!out_of_sample){resdir = paste0(resdir,"_30C")}
if(rich_poor){resdir = paste0(resdir,"_richpoor")}

resboot = "resboot"
if(!out_of_sample){resboot = paste0(resboot,"_30C")}
if(rich_poor){resboot = paste0(resboot,"_richpoor")}

if(dmg_func=="bootstrap" & clim=="ensemble"){
  ddd = file.path(resboot,paste0(ssp,"-",.rcp))
  filename = file.path(ddd,paste0("bbscc_",project_val,"_",runid,".RData"))
  if(file.exists(filename)){
    stop("already computed")
  }
}

# Load data 
source("data_gdpssp.R")
if(dmg_ref==""){
  source("data_burke_replica.R")
} else {
  source("data_djo_replica.R")
}
source("data_cmip5_kate.R")
source("data_impulse_response_with_models.R")
print(Sys.time() - t0)
#tables()

# Available combination of models available (CC x GCM for each RCP)
ssp_cmip5_models_temp <- ctemp[rcp==.rcp,unique(model)]
model_comb <- cpulse[ISO3=="USA" & mid_year==0.5 & model %in% ssp_cmip5_models_temp, list(model,ccmodel)]

# Future years
fyears <- impulse_year:2100

# Impulse year
cpulse[,year:=mid_year-0.5+fyears[1]]
epulse[,year:=mid_year-0.5+fyears[1]]

project_gdpcap_nocc <- function(SD){
  .gdpcap <- SD$gdpcap
  .gdpr <- SD$gdpr
  .gdpcap_tm1 <- .gdpcap[1]/(1+SD$gdpr[1]) # gdpcap in 2019
  for(i in seq_along(c(fyears))){
    .gdpcap[i] <- .gdpcap_tm1 * (1+SD$gdpr[i])
    .gdpcap_tm1 <- .gdpcap[i]
  }
  return(list(year = fyears, 
              gdpcap = .gdpcap,
              gdprate = SD$gdpr,
              delta = NA))
}

project_gdpcap_cc <- function(SD){
    .gdpcap <- SD$gdpcap
    .gdprate <- SD$gdpr
    .delta <- rep(NA,length(SD$gdpr))
    .gdpcap_tm1 <- .gdpcap[1]/(1+SD$gdpr[1]) # gdpcap nocc in 2019
    .ref_temp <- SD$temp[1] # reftemp is baseline temp for BHM and temp_tm1 for DJO
    for(i in seq_along(c(fyears))){
      if(dmg_ref==""){.ref_temp <- SD$basetemp[i]}
      .delta[i] <- warming_effect(SD$temp[i], .ref_temp, .gdpcap_tm1, nid)
      .gdprate[i] <- (SD$gdpr[i]+.delta[i]) 
      .gdpcap[i] <- .gdpcap_tm1 * (1+.gdprate[i])
      .gdpcap_tm1 <- .gdpcap[i]
      .ref_temp <- SD$temp[i]
    }
    return(list(year = fyears, 
                gdpcap = .gdpcap,
                gdprate = .gdprate,
                delta = .delta))
}

lcscc = NULL
lwscc = NULL

for(nid in runid){
  
print(paste("bootstrap run: ",nid))
  
# Create dataset for SSP
# ISO3 x model x ccmodel x years
ssp_gr <- growthrate[SSP==ssp & year %in% fyears]
if(clim=="ensemble"){
  ssp_temp <- ctemp[rcp==.rcp & year %in% fyears]
} else {
  ssp_temp <- etemp[rcp==.rcp & year %in% fyears]
}
ssp_temp = merge(ssp_temp, basetemp,by=c("ISO3")) # add basetemp
ssp_gdpr <- merge(ssp_gr, ssp_temp, by=c("ISO3","year")) # merge growth rate and temp
ssp_gdpr = merge(ssp_gdpr, sspgdpcap[SSP==ssp & year==fyears[1]],by=c("SSP","ISO3","year"),all.x=T) # add gdpcap0
if(clim=="ensemble"){
  ssp_gdpr = merge(cpulse[model %in% ssp_cmip5_models_temp & year %in% fyears],
                   ssp_gdpr, by=c("ISO3","year","model"), all.x=T) 
  ssp_gdpr[,model_id := paste(model,ccmodel)]
}else{
  ssp_gdpr = merge(epulse[year %in% fyears],
                   ssp_gdpr, by=c("ISO3","year"), all.x=T) 
}
miss_val_iso3 <- unique(ssp_gdpr[year==impulse_year & is.na(gdpcap),ISO3])
ssp_gdpr <- ssp_gdpr[!ISO3 %in% miss_val_iso3]

if(clim=="ensemble"){
# keep only model combination
model_comb[,model_id:=paste(model,ccmodel)]
ssp_gdpr <- ssp_gdpr[model_id %in% model_comb$model_id,
                     .(model_id,ISO3,year,temp,temp_pulse,basetemp,gdpr,gdpcap)]
} else {
  ssp_gdpr <- ssp_gdpr[,.(model_id=nid,ISO3,year,temp,temp_pulse,basetemp,gdpr,gdpcap)]
}
print(Sys.time() - t0)
#tables()

# 0. Compute GDP per capita without climate change
proj_gdpcapX <- ssp_gdpr[,project_gdpcap_nocc(.SD),by=c("model_id","ISO3")]
print(Sys.time() - t0)

#keep this gdpcap as a reference
gdcap0 = proj_gdpcapX[,.(model_id,ISO3,year,gdpcap0=gdpcap)]
ssp_gdpr = merge(gdcap0,ssp_gdpr,by=c("model_id","ISO3","year"))

# 1. Compute GDP per capita + damage effect
proj_gdpcap0 <- ssp_gdpr[,project_gdpcap_cc(.SD),by=c("model_id","ISO3")]
setnames(proj_gdpcap0,"gdpcap","gdpcap_cc")
setnames(proj_gdpcap0,"gdprate","gdprate_cc")
setnames(proj_gdpcap0,"delta","delta_cc")
print(Sys.time() - t0)

# 2. Compute GDP per capita + damage effect + impulse
ssp_gdpr[,temp:=temp+temp_pulse/1e3]
proj_gdpcap1 <- ssp_gdpr[,project_gdpcap_cc(.SD),by=c("model_id","ISO3")]
setnames(proj_gdpcap1,"gdpcap","gdpcap_imp")
setnames(proj_gdpcap1,"gdprate","gdprate_imp")
setnames(proj_gdpcap1,"delta","delta_imp")
print(Sys.time() - t0)

# yearly population 
popyear <- pop[SSP==ssp,approx(year,pop,fyears),by=c("SSP","ISO3")]
setnames(popyear,c("x","y"),c("year","pop"))
print(Sys.time() - t0)
#tables()

rm(list=c("ssp_gdpr","gdcap0"))

# Create main table for countries
res_scc <- merge(proj_gdpcap0, proj_gdpcap1, by=c(c("model_id","ISO3"), "year"))
res_scc <- merge(res_scc, proj_gdpcapX, by=c(c("model_id","ISO3"), "year"))
res_scc <- merge(res_scc,popyear,by=c("ISO3","year"))
print(Sys.time() - t0)

# create main table for world
res_wscc <- res_scc[,list(gdpcap_cc=weighted.mean(gdpcap_cc,pop)),by=c("year",c("model_id"),"SSP")]

# Compute average annual growth rate of per capita consumption between now and year t
# for the computation of discount factor
#countries
gdprate_cc_impulse_year = res_scc[year==impulse_year,
                                  .(gdpcap_cc_impulse_year=gdpcap_cc),
                                  by=c("model_id","ISO3")]
res_scc <- merge(res_scc,gdprate_cc_impulse_year,by=c("model_id","ISO3"))
res_scc[, gdprate_cc_avg := ifelse(year==impulse_year,gdprate_cc,(gdpcap_cc/gdpcap_cc_impulse_year)^(1/(year-impulse_year))-1) ]
#world
gdprate_cc_impulse_year = res_wscc[year==impulse_year,list(gdpcap_cc_impulse_year=gdpcap_cc),by=c("model_id")]
res_wscc <- merge(res_wscc,gdprate_cc_impulse_year,by=c("model_id"))
res_wscc[, gdprate_cc_avg := ifelse(year==impulse_year,NA,(gdpcap_cc/gdpcap_cc_impulse_year)^(1/(year-impulse_year))-1) ]
res_wscc = merge(res_wscc,res_wscc[year==(impulse_year+1),.(model_id,gdprate_cc_avg_impulse_year=gdprate_cc_avg)],by="model_id")
res_wscc[year==impulse_year,gdprate_cc_avg:=gdprate_cc_avg_impulse_year]
res_wscc[,gdprate_cc_avg_impulse_year:=NULL]

print(Sys.time() - t0)

# Compute SCC as in Anthoff and Tol (2010) equation A3 in Appendix
# \dfrac {\partial C_{t}} {\partial E_{0}}\times P_{t}
# approximate by change in GDP rather than consumption
res_scc[, scc := -(gdpcap_imp-gdpcap_cc)*pop*1e-3/44*12] # $2005/tCO2
sum_res_scc = res_scc[, .(scc=sum(scc)), by=c("year",c("model_id"))]
res_wscc = merge(res_wscc,sum_res_scc,by=c("year",c("model_id")))

# Extrapolation SCC (before discounting)
extrapolate_scc <- function(SD){
  if(project_val=="horizon2100"){
    .scc = 0
    .gdprate_cc_avg = 0
  } 
  if(project_val=="long-term"){
    .scc = SD[year==2100,scc]
    .gdprate_cc_avg = approx(c(2100,very_last_year),c(.gdpr,long_term_growth_rate),2101:very_last_year,rule=2)$y
  }
  if(project_val=="constant"){
    .scc = SD[year==2100,scc]
    .gdpr = (SD[year==2100,gdpcap_cc]/SD[year==2100,gdpcap_cc_impulse_year])^(1/(2100-impulse_year))-1
    if(.gdpr<0){
      .gdprate_cc_avg = (SD[year==2100,gdpcap_cc]/SD[year==2100,gdpcap_cc_impulse_year])^(1/((2101:very_last_year)-impulse_year))-1
    } else {
      .gdprate_cc_avg = .gdpr
    }
  }
  return(list(year=2101:very_last_year, scc=.scc, gdprate_cc_avg=.gdprate_cc_avg))
}

# combine if necessary
if(project_val != "horizon2100"){
  res_scc_future <- res_scc[,extrapolate_scc(.SD),by=c("ISO3",c("model_id"))]
  res_wscc_future <- res_wscc[,extrapolate_scc(.SD),by=c("model_id")]
  res_scc <- rbindlist(list(res_scc,res_scc_future),fill=T)
  res_wscc <- rbindlist(list(res_wscc,res_wscc_future),fill=T)
}
print(Sys.time() - t0)

# Discount SCC
# pure rate of time preference
prtps = c(1,2,3) # %
# elasticity of marginal utility of consumption
# based on Table 3.2 in IPCC AR5 WG2 Chapter 3
etas = c(0.7,1.5,2.5) 

cscc = NULL
for(.prtp in prtps){
  for(.eta in etas){
    
    dscc = res_scc[,list(ISO3,model_id,year,gdprate_cc_avg,scc)]
    dscc[,dfac:=(1/(1+.prtp/100+.eta*gdprate_cc_avg)^(year-impulse_year))]
    dscc[,dscc:= dfac * scc]
    cscc = rbind(cscc,dscc[,list(prtp=.prtp,eta=.eta,scc=sum(dscc)),by=c("ISO3","model_id")],fill=T)
    
  }
}
wscc = cscc[,list(scc=sum(scc)),by=c("prtp","eta","model_id")]

# Discount rates for comparison with EPA - WG SCC 
#[[https://www.epa.gov/climatechange/social-cost-carbon]]
drs = c(2.5,3,5) #%
cscc0 = NULL
for(.dr in drs){
  
  dscc = res_scc[,list(ISO3,model_id,year,scc)]
  dscc[,dfac:=(1/(1+.dr/100)^(year-impulse_year))]
  dscc[,dscc:= dfac * scc]
  cscc0 = rbind(cscc0,dscc[,list(dr=.dr,scc=sum(dscc)),by=c("ISO3","model_id")])
}
cscc = rbindlist(list(cscc0,cscc),fill=T)
wscc = rbindlist(list(wscc,cscc0[,list(scc=sum(scc)),by=c("dr","model_id")]),fill=T)

print(Sys.time() - t0)

# ID to be used
wscc[,ISO3:="WLD"]
cscc[,ID:=paste(prtp,eta,dr,ISO3,sep="_")]
wscc[,ID:=paste(prtp,eta,dr,ISO3,sep="_")]
cscc[,ID:=str_replace(ID,"\\.","p")]
wscc[,ID:=str_replace(ID,"\\.","p")]

lcscc = c(lcscc,list(cscc[,.(scc,ID)]))
lwscc = c(lwscc,list(wscc[,.(scc,ID)]))

}

cscc = rbindlist(lcscc)
wscc = rbindlist(lwscc)

store_scc = rbind(cscc,wscc)

print(Sys.time() - t0)
print("CSCC in list store_scc")
