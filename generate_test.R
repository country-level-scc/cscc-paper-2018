
# generate test_result and compare to analytical results
# all temp increases are set to zero
# temp pulses are set to zero except for Angola which gets a temperature pulse of 1 degree in 2021

# default option is RCP 4.5, SSP1, eta of 1 and bhm damage function
# change to option to choose different values? But how to do so within a source function

# change ssp, rcp, dam_func, eta
library(docopt)

'usage: generate_test.R -s <ssp> -c <rcp> [ -r <runid> -p <type> -l <clim> -e <eta> -f <name> -t <opt>] [-a] [-o] [-d] [-w]
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

# specify test options
test_options <- docopt(doc, "-s SSP1 -c rcp45 -w -e 1 -t t1 -f dice") # Default case

generate_test = TRUE
source("generate_cscc.R")

if (opts["-f"] == "bhm"){
  # derived from gdpcap_imp = gdpcap * (gdpr + gdpr_damage_imp) and gdpcap_cc = gdpcap * (gdpr - gdpr_damage_cc)
  # -(gdpcap_imp - gdpcap_cc) * pop * 1 (= 1e6/pulse_scale) and then discounting by 1/(1 + prtp/100 + eta * gdprate_cc_avg)^1
  value = 0.08361
} else if (opts["-f"] == "dice"){
  # derived from -(gdp * (damage_coeff_imp - damage_coeff_cc)) * 1e6/pulse_scale and then discounting
  test_temp_dif = 1e-3 / 44 * 12 * (pulse_scale * 1e-9)
  GDPval_ago = ssp_gdp[SSP=="SSP1"&ISO3=="AGO"&year==2021]$gdp[1]
  # GDP is in billions so add factor of 1e9. Discount by 1 year. 
  value = 0.00236 * ((test_temp_dif + temp_history) ** 2 - temp_history**2) * GDPval_ago * 1e9/pulse_scale / (1 + 0.025)
} else { 
  # derived from gdpcap_imp = gdpcap * (gdpr + gdpr_damage_imp) and gdpcap_cc = gdpcap * (gdpr - gdpr_damage_cc)
  # -(gdpcap_imp - gdpcap_cc) * pop and then discounting 
  value = 0.01844812 } # value for djo option

# test values from datatable cscc and wscc to say whether the test has passed
# check for default values
if (test_opt == "t0") {
  for (i in 1:nrow(cscc)){
    if (cscc[[1]][i] != 0){
      raise_error[[(length(raise_error) + 1)]] <- i # append the row number to list to know where error occurred
    }
  }
} else if (test_opt == "t1"){
  raise_error = list()
  for (i in 1:nrow(cscc)){
    if ((cscc[[1]][i] != 0) & (grepl("AGO",cscc[[2]][i]) == FALSE)){
      raise_error[[(length(raise_error) + 1)]] <- i # append the row number to list to know where error occurred
    }
    if (grepl("AGO",cscc[[2]][i])){
      if (!(cscc$scc[2] < (value + 1e-10) & (cscc$scc[2] > (value - 1e-10)))){ # answer should be around value, with 1e-10 for computational error
        raise_error[[(length(raise_error) + 1)]] <- i # append the row number to list 
      }
    }
  }
} 

if (length(raise_error) == 0){
    print("Test passed")
} else {
  rows_error = 0
  for (i in raise_error){
    row = raise_error[[1]]
    rows_error = paste0(rows_error, row)
  }
  print(paste0("Test has failed. The errors occured in row(s): ", rows_error))
}
