
# generate test_result and compare to analytical results
# all temp increases are set to zero
# temp pulses are set to zero except for Angola which gets a temperature pulse of 1 degree in 2021

# default option is RCP 4.5, SSP1, eta of 1 and bhm damage function
# change to option to choose different values? But how to do so within a source function

# change ssp, rcp, dam_func, eta
test_options <- function(opts){
  opts["-s"] = "SSP1"
  opts["-c"] = "rcp45"
  opts["-r"] = NULL
  opts["-t"] = "t1" # default test method where angola increases by 1 degree for one year
  opts["-p"] = NULL
  opts["-l"] = NULL
  opts["-e"] = "1"
  opts["-f"] = "bhm"
  return(opts)
}

generate_test = TRUE
source("generate_cscc.R")

if (opts["-f"] == "bhm"){
  value = 0.08361
} else if (opts["-f"] == "dice"){
  value = 0.0002548864
} else { value = 0.01844812 } # value for djo option

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
      if (!(cscc$scc[2] < (value*1.01)) & (cscc$scc[2] > (value*0.99))){ # answer should be around value, so take 1% margin
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
  print(paste0("Test has failed. The errors occured in row(s): ", rows_error))}
