# Compute country-level social cost of carbon
#
# Any questions: laurent.drouet@eiee.org
# Launch Rscript ./generate_cscc.R to see usage
# Check the Rmd file for an example of use
# Outputs are expressed in USD 2005

library(data.table)
library(docopt)

'usage: generate_cscc.R -s <ssp> -c <rcp> [ -r <runid> -p <type> -l <clim> -e <eta> -f <name> -t <opt>] [-a] [-o] [-d] [-w]
options:
 -s <ssp>   SSP baseline (random(default), SSP1, SSP2,..., SSP5)
 -c <rcp>   RCP (random(default), rcp45, rcp60, rcp85)
 -r <runid> Bootstart run for the damage function parameter, 0 is estimates (0<=id<=1000)
 -p <type>  projection type (constant (default),horizon2100)
 -l <clim>  climate models (ensemble (default), mean[-ensemble])
 -e <eta>   elasticity of marginal utility of consumption. eta is 1 (default) or 2
 -o         does not allow for out-of-sample damage prediction (default, allows)
 -d         rich/poor damage function specification (default, pooled)
 -a         5-lag damage function specification (default, 0-lag)
 -f <name>  damage function (default=bhm (Burke et al.), djo (Dell et al.)), dice (Nordhaus)
 -t <opt>   allows for generating tests with different temperature input. t0 (zero temp change) or t1 (1 degree for one year in one country)
 -w         save raw data' -> doc


#opts <- docopt(doc)

# set options
if (!exists("generate_test")){
  opts <- docopt(doc, "-s SSP3 -c rcp85 -t t1 -e 1 -f dice") # Default case
  #opts <- docopt(doc, "-s all -c all -f djo")
  #opts <- docopt(doc, "-s SSP2 -c rcp60 -r 1 -w -a -d")
  #opts <- docopt(doc, "-s SSP2 -c rcp60 -r 0 -l mean -w -a -d")
  #opts <- docopt(doc, "-s SSP2 -c rcp60 -r 0 -w -d -f djo")
} else if (generate_test == TRUE){
  opts = test_options
}

t0 <- Sys.time()

# GLOBAL VARIABLES
if (opts[["s"]] == "all") {
  ssps = c(paste0("SSP",1:5)) # SSP{1,2,3,4,5}
} else {
  ssps = c(as.character(opts["s"]))
}
if (opts[["c"]] == "all") {
  rcps = c("rcp45","rcp60","rcp85")
} else {
  rcps = as.character(opts["c"]) 
}
if (is.null(opts[["r"]])) {
  dmg_func = "estimates" # dmg function
  runid = 0
} else {
  print(paste("r:",opts['r']))
  runid = as.integer(max(0,min(1000,as.numeric(opts['r']))))
  if (runid == 0) {
    dmg_func = "estimates"
  }else{
    dmg_func = paste0("bootstrap")
  }
}

if (is.null(opts[["p"]])) {
  project_val = "constant" # growth rate constant
} else {
  project_val = as.character(opts["p"])
}
if (is.null(opts[["l"]])) {
  clim = "ensemble"
} else {
  clim = "mean"
  if (runid != 0) {
    dmg_func = "bootstrap"
    runid = 1:1000
  }
}

if (is.null(opts[["e"]])){ # default RRA is 1 
  etas = c(1)
} else if (opts["e"] == 1){
  etas = c(1)
} else {etas = c(2)}

if (is.null(opts[["f"]])) {
  dmg_ref = "bhm"
} else {
  dmg_ref = as.character(opts["f"])
}

if (is.null(opts[["-t"]])){
  test = FALSE
} else {
  test = TRUE
  test_opt = as.character(opts["-t"])
}

out_of_sample = !opts[['o']]
rich_poor = opts[['d']]
lag5 = opts[['a']]
save_raw_data = opts[['w']]
very_last_year = 2200
impulse_year = 2020
if (test == TRUE){
  preffdir = "results_test/res"
} else {preffdir = "results/res"}
pulse_scale = 1e6 # Gt=1e9 Mt=1e6 kt=1e3 t=1 
reftemplastyear = F

if (dmg_ref == "djo") {
  rich_poor = T
  out_of_sample = T
  lag5 = F
  dmg_func = "estimates"
  reftemplastyear = T
}

resdir = paste0(preffdir,"_stat",dmg_ref)
if (!out_of_sample) {resdir = paste0(resdir,"_30C")}
if (rich_poor) {resdir = paste0(resdir,"_richpoor")}
if (lag5) {resdir = paste0(resdir,"_lr")}

resboot = paste0(preffdir,"_boot")
if (!out_of_sample) {resboot = paste0(resboot,"_30C")}
if (rich_poor) {resboot = paste0(resboot,"_richpoor")}
if (lag5) {resboot = paste0(resboot,"_lr")}

if (dmg_ref == "bhm") {
  dmg_ref = ""
}else{
  dmg_ref = paste0("_",dmg_ref)
}

for (.rcp in rcps){
  for (ssp in ssps){
    # Print simulation parameters
    print(paste("SSP: ",ssp))
    print(paste("RCP: ",.rcp))
    print(paste("dmg_func: ",dmg_func))
    print(paste("last year: ",very_last_year))
    print(paste("prefix dir: ",preffdir))
    print(paste("climate ensemble: ",clim))
    print(paste("impulse year: ",impulse_year))
    print(paste("projection post2100: ",project_val))
    print(paste("out_of_sample: ",out_of_sample))
    print(paste("richpoor: ",rich_poor))
    print(paste("LR (lag5): ", lag5))
    print(paste("damage function:",dmg_ref))
    
    if (dmg_func == "bootstrap" & clim == "ensemble") {
      ddd = file.path(resboot,paste0(ssp,"-",.rcp))
      filename = file.path(ddd,paste0("store_scc_",project_val,"_",runid,dmg_ref,".RData"))
      if (file.exists(filename)) {
        stop("already computed")
      }
    }
    
    # Load data 
    source("modules/gdpssp.R")
    if (dmg_ref == "") {
      source("modules/bhm_replica.R")
    } else if (dmg_ref == "_dice"){
      source("modules/dice_replica.R")
    } else {
      source("modules/djo_replica.R")
    }
    
    source("modules/cmip5.R")
    source("modules/impulse_response.R")
    print(Sys.time() - t0)
    
    # All combination of models available (CC x GCM for each RCP)
    ssp_cmip5_models_temp <- ctemp[rcp == .rcp,unique(model)]
    model_comb <- cpulse[ISO3 == "USA" & mid_year == 0.5 & 
                           model %in% ssp_cmip5_models_temp, 
                         .(model,ccmodel)]
    
    # Future years
    fyears <- impulse_year:2100
    
    # Impulse year
    cpulse[,year := mid_year - 0.5 + fyears[1]]
    epulse[,year := mid_year - 0.5 + fyears[1]]
    
    project_gdpcap_nocc <- function(SD){
      .gdpcap <- SD$gdpcap
      .gdpr <- SD$gdpr
      .gdpcap_tm1 <- SD$gdpcap[1]/(1 + SD$gdpr[1]) # gdpcap in 2019
      for (i in seq_along(c(fyears))) {
        .gdpcap[i] <- .gdpcap_tm1 * (1 + SD$gdpr[i])
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
      .gdpcap_tm1 <- .gdpcap[1]/(1 + SD$gdpr[1]) # gdpcap nocc in 2019
      .ref_temp <- SD$temp[1] # reftemp is baseline temp for BHM and temp_tm1 for DJO
      for (i in seq_along(c(fyears))) {
        if (dmg_ref == "") {.ref_temp <- SD$basetemp[i]}
        .delta[i] <- warming_effect(SD$temp[i], .ref_temp, .gdpcap_tm1, nid)
        .gdprate[i] <- (SD$gdpr[i] + .delta[i])
        .gdpcap[i] <- .gdpcap_tm1 * (1 + .gdprate[i])
        .gdpcap_tm1 <- .gdpcap[i]
        .ref_temp <- SD$temp[i]
      }
      return(list(year = fyears, 
                  gdpcap = .gdpcap,
                  gdprate = .gdprate,
                  delta = .delta))
    }
    
    project_gdpcap_cc_pulse <- function(SD){
      .gdpcap <- SD$gdpcap
      .gdprate <- SD$gdpr
      .delta <- rep(NA,length(SD$gdpr))
      .gdpcap_tm1 <- .gdpcap[1]/(1 + SD$gdpr[1]) # gdpcap nocc in 2019
      .ref_temp <- SD$temp[1] # reftemp is baseline temp for BHM and temp_tm1 for DJO
      for (i in seq_along(c(fyears))) {
        if (dmg_ref == "") {.ref_temp <- SD$basetemp[i]}
        .delta[i] <- warming_effect(SD$temp_pulse[i], .ref_temp, .gdpcap_tm1, nid)
        .gdprate[i] <- (SD$gdpr[i] + .delta[i])
        .gdpcap[i] <- .gdpcap_tm1 * (1 + .gdprate[i])
        .gdpcap_tm1 <- .gdpcap[i]
        .ref_temp <- SD$temp[i]
      }
      return(list(year = fyears, 
                  gdpcap = .gdpcap,
                  gdprate = .gdprate,
                  delta = .delta))
    }
    
    # Project all scenarios
    project_gdpcap <- function(SD){
      .gdpcap <- SD$gdpcap
      .gdpcap_cc <- SD$gdpcap
      .gdpcap_imp <- SD$gdpcap
      
      .gdprate_cc <- SD$gdpr
      
      .gdpcap_tm1 <- .gdpcap[1]/(1 + SD$gdpr[1]) # gdpcap nocc in 2019
      .gdpcap_tm1_cc <- .gdpcap[1]/(1 + SD$gdpr[1]) # gdpcap nocc in 2019
      .gdpcap_tm1_imp <- .gdpcap[1]/(1 + SD$gdpr[1]) # gdpcap nocc in 2019
      
      .ref_temp <- SD$temp[1] # reftemp is baseline temp for BHM and temp_tm1 for DJO
      if (!reftemplastyear) {.ref_temp <- SD$basetemp[1]}
      
      for (i in seq_along(c(fyears))) {
        # No climate change
        .gdpcap[i] <- .gdpcap_tm1 * (1 + SD$gdpr[i])
        .gdpcap_tm1 <- .gdpcap[i]
        # With climate change
        .gdprate_cc[i] <- SD$gdpr[i] + warming_effect(SD$temp[i], .ref_temp, .gdpcap_tm1_cc, nid, out_of_sample)
        .gdpcap_cc[i] <- .gdpcap_tm1_cc * (1 + .gdprate_cc[i])
        .gdpcap_tm1_cc <- .gdpcap_cc[i]
        # With climate change and pulse
        .gdpcap_imp[i] <- .gdpcap_tm1_imp * (1 + (SD$gdpr[i] + warming_effect(
          SD$temp_pulse[i], .ref_temp, .gdpcap_tm1_imp, nid, out_of_sample)))
        .gdpcap_tm1_imp <- .gdpcap_imp[i]
        if (reftemplastyear) {.ref_temp <- SD$temp[i]}
      }
      return(list(year = fyears, 
                  gdpcap = .gdpcap,
                  gdpcap_cc = .gdpcap_cc,
                  gdpcap_imp = .gdpcap_imp,
                  gdprate_cc = .gdprate_cc
      ))
    }

    project_gdpcap_cc_dice <- function(SD){
      .gdp <- SD$gdp # gdp in billions of 2005 USD
      .gdprate_cc <- rep(NA, length(SD$gdp))
      .delta_cc <- rep(NA,length(SD$gdp))
      .delta_imp <- rep(NA,length(SD$gdp))
      .gdp_damages_cc <- rep(NA,length(SD$gdp))
      .gdp_netto_cc <- rep(NA,length(SD$gdp))
      .gdp_damages_imp <- rep(NA,length(SD$gdp))
      .gdp_netto_imp <- rep(NA,length(SD$gdp))
      .gdprate_cc[1] <- SD$gdpr[1]
      # basetemp is temp_history from 1900 until 2020 + temp in 2020 so reftemp is from 2020
      .ref_temp <- SD$temp[1]
      for (i in seq_along(c(fyears))){
        .gdp_base = SD$gdp[i]
        .delta_cc[i] <- warming_effect(SD$temp[i], .ref_temp, .gdp_base, nid, out_of_sample=T, temp_history)
        # damages for climate change
        # Damages = Ygross * damfrac in bllions of USD per year
        .gdp_damages_cc[i] <- (.gdp_base * .delta_cc[i])
        # Ynet = Ygross * (damage function) output net in billions of USD per year
        .gdp_netto_cc[i] <- (.gdp_base * (1 - .delta_cc[i]))
        # damages for impulse temp
        .delta_imp[i] <- warming_effect(SD$temp_pulse[i], .ref_temp, .gdp_base, nid, out_of_sample=T, temp_history)
        .gdp_damages_imp[i] <- (.gdp_base * .delta_imp[i])
        .gdp_netto_imp[i] <- (.gdp_base  * (1 - .delta_imp[i]))
        
        .gdp_base <- .gdp[i]
        # calculate gdprate_cc
        if (i > 1){
          .gdprate_cc[i] <- (.gdp_netto_cc[i] / .gdp_netto_cc[i-1]) -1
        }
      }
      return(list(year = fyears,
                  gdpcap = .gdp,
                  gdpcap_cc = .gdp_netto_cc,
                  gdpcap_imp = .gdp_netto_imp,
                  gdp_damages_cc = .gdp_damages_cc,
                  gdp_damages_imp = .gdp_damages_imp,
                  gdprate_cc = .gdprate_cc
      ))
    }
    
    lcscc = NULL
    lwscc = NULL
    leq_wscc = NULL
    leri_eq_wscc = NULL
    
    for (nid in runid) {
      
      # Create dataset for SSP
      # ISO3 x model x ccmodel x years
      #View(growthrate)
      #ssp_gr <- growthrate[year %in% fyears]
      ssp_gr <- growthrate[SSP == ssp & year %in% fyears]
      if (clim == "ensemble") {
        ssp_temp <- ctemp[rcp == .rcp & year %in% fyears]
      } else {
        ssp_temp <- etemp[rcp == .rcp & year %in% fyears]
      }
      ssp_temp = merge(ssp_temp,basetemp,by = c("ISO3")) # add basetemp
      ssp_gdpr <- merge(ssp_gr,ssp_temp,by = c("ISO3","year")) # merge growth rate and temp
      ssp_gdpr = merge(ssp_gdpr, sspgdpcap[SSP == ssp & year == fyears[1]],
                       by = c("SSP","ISO3","year"),all.x = T) # add gdpcap0
      if (clim == "ensemble") {
        ssp_gdpr = merge(cpulse[model %in% ssp_cmip5_models_temp & year %in% fyears],
                         ssp_gdpr, by = c("ISO3","year","model"), all.x = T) 
        ssp_gdpr[,model_id := paste(model,ccmodel)]
      }else{
        ssp_gdpr = merge(epulse[year %in% fyears],
                         ssp_gdpr, by = c("ISO3","year"), all.x = T) 
      }
      miss_val_iso3 <- unique(ssp_gdpr[year == impulse_year & is.na(gdpcap),ISO3])
      ssp_gdpr <- ssp_gdpr[!ISO3 %in% miss_val_iso3]

      if (clim == "ensemble") {
        # keep only model combination
        model_comb[,model_id := paste(model,ccmodel)]
        ssp_gdpr <- ssp_gdpr[model_id %in% model_comb$model_id,
                             .(model_id,ISO3,year,temp,temp_pulse,basetemp,gdpr,gdpcap)]
      } else {
        ssp_gdpr <- ssp_gdpr[,.(model_id = nid,ISO3,year,temp,temp_pulse,basetemp,gdpr,gdpcap)]
      }
      ssp_gdpr[,temp_pulse := temp + temp_pulse * 1e-3 / 44 * 12 * (pulse_scale * 1e-9)]
      setkey(ssp_gdpr,model_id,ISO3)
      print(Sys.time() - t0)

      if (dmg_ref == "_dice") {
        ssp_gdp <- gdp_yearly[SSP == ssp & year %in% fyears] # gdp in billions of USD
        ssp_gdp <- merge(ssp_gdpr, ssp_gdp, by = c("year","ISO3"))
        res_sccdice <- ssp_gdp[,project_gdpcap_cc_dice(.SD),by = c("model_id","ISO3")]

        # generating gdp cap by dividing through population
        popyear <- pop[SSP == ssp,approx(year,pop,fyears), by = c("SSP","ISO3")] # population in millions
        colnames(popyear) <- c("SSP", "ISO3", "year", "pop")
        
        # converting GDP and GDP damges to gdp per capitan and damages per capita
        gdpcap_yearly <- merge(popyear, res_sccdice, by = c("ISO3","year"))
        gdpcap_yearly[, gdpcap := gdpcap/pop*1e3] # as gdp is in billions but population in millions, do *1e3 to get dollars
        gdpcap_yearly[, gdp_damages_cc := gdp_damages_cc/pop*1e3]
        gdpcap_yearly[, gdpcap_cc := gdpcap_cc/pop*1e3]
        gdpcap_yearly[, gdp_damages_imp := gdp_damages_imp/pop*1e3]
        gdpcap_yearly[, gdpcap_imp := gdpcap_imp/pop*1e3]
        res_scc <- setcolorder(gdpcap_yearly, 
          c("ISO3", "year", "model_id","gdpcap", "gdpcap_cc", "gdpcap_imp","gdprate_cc", "gdp_damages_cc","gdp_damages_imp", "SSP", "pop"))
      } else {
        res_scc <- ssp_gdpr[,project_gdpcap(.SD),by = c("model_id","ISO3")]
        # yearly population
        popyear <- pop[SSP == ssp,approx(year,pop,fyears),by = c("SSP","ISO3")]
        setnames(popyear,c("x","y"),c("year","pop"))
        res_scc <- merge(res_scc,popyear,by = c("ISO3","year"))
        print(Sys.time() - t0)
      } 

      # create main table for world
      res_wscc <- res_scc[,.(gdpcap_cc = weighted.mean(gdpcap_cc,pop)),
                          by = c("year",c("model_id"),"SSP")]

      # Compute average annual growth rate of per capita consumption between now and year t
      # for the computation of discount factor
      #countries
      gdprate_cc_impulse_year = res_scc[year == impulse_year,
                                        .(gdpcap_cc_impulse_year = gdpcap_cc),
                                        by = c("model_id","ISO3")]
      res_scc <- merge(res_scc,gdprate_cc_impulse_year,by = c("model_id","ISO3"))
      res_scc[, gdprate_cc_avg := ifelse(year == impulse_year,
                                         gdprate_cc,
                                         (gdpcap_cc/gdpcap_cc_impulse_year)^(1/(year - impulse_year)) - 1)]
      
      #World res_scc
      gdprate_cc_impulse_year = res_wscc[year == impulse_year,
                                         .(gdpcap_cc_impulse_year = gdpcap_cc),
                                         by = c("model_id")]
      res_wscc <- merge(res_wscc,gdprate_cc_impulse_year,
                        by = c("model_id"))
      res_wscc[, gdprate_cc_avg := ifelse(year == impulse_year,
                                          NA,
                                          (gdpcap_cc/gdpcap_cc_impulse_year)^(1/(year - impulse_year)) - 1)]
      res_wscc = merge(res_wscc,res_wscc[year == (impulse_year + 1),
                                         .(model_id,gdprate_cc_avg_impulse_year = gdprate_cc_avg)],
                       by = "model_id")
      res_wscc[year == impulse_year,gdprate_cc_avg := gdprate_cc_avg_impulse_year]
      res_wscc[,gdprate_cc_avg_impulse_year := NULL]
      print(Sys.time() - t0)

      # Compute SCC according to Anthoff and Tol equation A3 in Appendix
      # \dfrac {\partial C_{t}} {\partial E_{0}}\times P_{t}
      # approximate by change in GDP rather than consumption
      res_scc[, scc := -(gdpcap_imp - gdpcap_cc) * pop * (1e6 / pulse_scale)] # $2005/tCO2
      sum_res_scc = res_scc[, .(scc = sum(scc)), 
                            by = c("year",c("model_id"))]
      res_wscc = merge(res_wscc,sum_res_scc,
                       by = c("year",c("model_id")))

      # Extrapolation SCC (before discounting)
      extrapolate_scc <- function(SD){
        if (project_val == "horizon2100") {
          .scc = 0
          .gdprate_cc_avg = 0
        } 
        if (project_val == "constant") {
          .scc = SD[year == 2100,scc]
          .gdpr = (SD[year == 2100,gdpcap_cc]/SD[year == 2100,gdpcap_cc_impulse_year])^(1/(2100 - impulse_year)) - 1
          if (.gdpr < 0) {
            .gdprate_cc_avg = (SD[year == 2100,gdpcap_cc]/
                                 SD[year == 2100,gdpcap_cc_impulse_year])^(1/((2101:very_last_year) - impulse_year)) - 1
          } else {
            .gdprate_cc_avg = .gdpr
          }
        }
        return(list(year = 2101:very_last_year, scc = .scc, gdprate_cc_avg = .gdprate_cc_avg))
      }

      # combine if necessary
      if (project_val != "horizon2100") {
        res_scc_future <- res_scc[,extrapolate_scc(.SD),by = c("ISO3",c("model_id"))]
        res_wscc_future <- res_wscc[,extrapolate_scc(.SD),by = c("model_id")]
        res_scc <- rbindlist(list(res_scc,res_scc_future),fill = T)
        res_wscc <- rbindlist(list(res_wscc,res_wscc_future),fill = T)
      }
      print(Sys.time() - t0)

      
      # Discount SCC according to Anthoff and Tol equation A3 in Appendix
      # elasticity of marginal utility of consumption = 1
      # based on Table 3.2 in IPCC AR5 WG2 Chapter 3
      # added 3% prtp to be compatible with EPA
      prtps = c(0, 1, 2, 3) # %
      if(any(size(etas) > 1)) {
        stop("Global equality weighting breaks down with multiple values of eta")
      }
      
      cscc = NULL
      for (.prtp in prtps) {
        for (.eta in etas) {
          dscc = res_scc[,list(ISO3,model_id,year,gdprate_cc_avg,scc)]
          dscc[,dfac := (1/(1 + .prtp/100 + .eta * gdprate_cc_avg)^(year - impulse_year))]
          dscc[,dscc := dfac * scc]
          cscc = rbind(cscc,dscc[,.(prtp = .prtp,eta = .eta,scc = sum(dscc)),
                                 by = c("ISO3","model_id")],fill = T)
        }
      }
      wscc = cscc[,list(scc = sum(scc)),by = c("prtp","eta","model_id")]
  
      # Also calculate the world social cost of carbon under equality conditions
      mean_gdp_per_cap_world = weighted.mean(
        gdpcap[year == impulse_year & SSP==ssp]$gdpcap, 
        gdpcap[year == impulse_year & SSP==ssp]$pop
      )
      weights = gdpcap[year == impulse_year & SSP==ssp, c("ISO3", "gdpcap")]
      weights$weight = (mean_gdp_per_cap_world / weights$gdpcap)^.eta  # Assumes only one value of eta
      eq_cscc = merge(x=cscc, y=weights, by="ISO3", all.x=TRUE)
      # The Somalian data is bad so we remove it entirely
      eq_cscc <- eq_cscc[ISO3 != "SOM",]
      eq_wscc = eq_cscc[,list(scc = sum(scc * weight)), by = c("prtp","eta","model_id")]
      
      # Comparison EPA (SC-CO2) [[http://www3.epa.gov/climatechange/EPAactivities/economics/scc.html]]
      drs = c(2.5,3,5) #%
      cscc0 = NULL
      
      for (.dr in drs) {
        dscc = res_scc[,list(ISO3,model_id,year,scc)]
        dscc[,dfac := (1/(1 + .dr/100)^(year - impulse_year))]
        dscc[,dscc := dfac * scc]
        cscc0 = rbind(cscc0,dscc[,.(dr = .dr,scc = sum(dscc)),
                                 by = c("ISO3","model_id")])
      }
      cscc = rbindlist(list(cscc0,cscc),fill = T)
      wscc = rbindlist(list(wscc,cscc0[,.(scc = sum(scc)),
                                       by = c("dr","model_id")]),
                       fill = T)
      weighted_cscc0 = merge(x = cscc0, y = weights[, c("ISO3", "weight")], by="ISO3")
      eq_wscc = rbindlist(list(eq_wscc, weighted_cscc0[,.(scc = sum(scc * weight)),
                                                       by = c("dr","model_id")]),
                          fill = T)
      
      print(Sys.time() - t0)

      # ID to be used
      wscc[, ISO3 := "WLD"]
      eq_wscc[, ISO3 := "WLD"]
      cscc[, ID := paste(prtp, eta, dr, ISO3, sep = "_")]
      wscc[, ID := paste(prtp, eta, dr, ISO3, sep = "_")]
      eq_wscc[, ID := paste(prtp, eta, dr, ISO3, sep = "_")]
      cscc[, ID := str_replace(ID, "\\.", "p")]
      wscc[, ID := str_replace(ID, "\\.", "p")]
      eq_wscc[, ID := str_replace(ID, "\\.", "p")]
      
      lcscc = c(lcscc, list(cscc[, .(scc, ID)]))
      lwscc = c(lwscc, list(wscc[, .(scc, ID)]))
      leq_wscc = c(leq_wscc, list(eq_wscc[, .(scc, ID)]))
    }
    
    cscc = rbindlist(lcscc)
    wscc = rbindlist(lwscc)
    eq_wscc = rbindlist(leq_wscc)
    
    dollar_val_2020 = 1.35
    eq_wscc$scc = dollar_val_2020 * eq_wscc$scc
    eri_eq_wscc = eq_wscc
    eri_eq_wscc$scc = eri_eq_wscc$scc/weights[ISO3 == "ERI"]$weight
    
    store_scc <- rbind(cscc, wscc)
    store_scc_flat <- split(store_scc$scc, store_scc$ID)
    store_eq_wscc_flat <- split(eq_wscc$scc, eq_wscc$ID)
    store_eri_eq_wscc_flat <- split(eri_eq_wscc$scc, eri_eq_wscc$ID)
    
    print(Sys.time() - t0)
    
    compute_stat <- function(.data) {
      res <- c(list(mean = mean(.data)),
               as.list(quantile(.data, probs = c(
                 0.1, 0.25, 0.5, 0.75, 0.9
               ))))
      return(as.data.table(res))
    }
    
    # Bayesian bootstrap to check quality of statistics
    #lapply(store_scc_flat, bayesboot, mean)
    
    if (save_raw_data) {
      dir.create(file.path(resdir), recursive = T, showWarnings = F)
      filename = file.path(resdir,paste0("raw_scc_",ssp,"_",.rcp,"_",project_val,"_",dmg_func,"_clim",clim,dmg_ref,".RData"))
      save(store_scc_flat, file = filename)
    }
    eq_stat_wscc <-  rbindlist(lapply(store_eq_wscc_flat, compute_stat))
    eq_stat_wscc$ID <- names(store_eq_wscc_flat)
    if (dmg_func == "estimates" | clim == "mean") {
      stat_scc <- rbindlist(lapply(store_scc_flat, compute_stat))
      stat_scc$ID <- names(store_scc_flat)
      dir.create(file.path(resdir), recursive = T, showWarnings = F)
      savestring = paste0(ssp,"_",.rcp,"_",project_val,"_",dmg_func,"_clim",clim,dmg_ref,"_eta_",.eta,".RData")
      filename = file.path(resdir,paste0("statscc_",savestring))
      save(stat_scc, file = filename)
      print(paste(filename,"saved"))
    } else {
      ddd = file.path(resboot,paste0(ssp,"-",.rcp))
      dir.create(ddd, recursive = T, showWarnings = F)
      savestring = paste0(ssp,"_",.rcp,"_",project_val,"_",dmg_func,"_clim",clim, dmg_ref,"_eta_",.eta,".RData")
      filename = file.path(ddd,paste0("store_scc_",savestring))
      save(store_scc_flat, file = filename)
      print(paste(filename,"saved"))
    }
    eq_stat_wscc <-  rbindlist(lapply(store_eq_wscc_flat, compute_stat))
    eq_stat_wscc$ID <- names(store_eq_wscc_flat)
    eri_eq_stat_wscc <-  rbindlist(lapply(store_eri_eq_wscc_flat, compute_stat))
    eri_eq_stat_wscc$ID <- names(store_eri_eq_wscc_flat)
    eq_filename = file.path(resdir,paste0("eq_statscc_2020d",savestring))
    eri_eq_filename = file.path(resdir,paste0("eri_eq_statscc_2020d",savestring))
    save(eq_stat_wscc, file = eq_filename)
    print(paste(eq_filename,"saved"))
    save(eri_eq_stat_wscc, file = eri_eq_filename)
    print(paste(eri_eq_filename,"saved"))
    
    # Calculate the income level at which it is preferable to give to the poor than to avert 
    # CO2 at $10/ton. 
    poorpref_fn <- function(.wscc) {
      mean_gdp_per_cap_world * (10/.wscc)^(1/.eta)
    }
    poor_prefer_10 <- as.data.frame(
      sapply(subset(eq_stat_wscc, select=names(eq_stat_wscc)[names(eq_stat_wscc) != "ID"]),FUN=poorpref_fn)
    )
    poor_prefer_10$ID <- eq_stat_wscc$ID
    poor_prefer_10_filename = file.path(resdir,paste0("poor_pref_10dollars",savestring))
    save(poor_prefer_10, file = poor_prefer_10_filename)
    print(paste(poor_prefer_10_filename,"saved"))
    
    print(Sys.time() - t0)
  }
}
if (exists("generate_test")){
  rm(generate_test)
}

print("end")

