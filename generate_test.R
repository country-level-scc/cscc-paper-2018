
# generate test_result and compare to analytical results
# all temp increases are set to zero
# temp pulses are set to zero except for Angola which gets a temperature pulse of 1 degree in 2021

# default option is RCP 4.5, SSP1, eta of 1 and bhm damage function
# change to option to choose different values? But how to do so within a source function
source(generate_cscc_test.R)

View(cscc)
# test values from datatable cscc and wscc to say whether the test has passed]
if (cscc$scc[2] < 1){
  print("Test passed")
}
