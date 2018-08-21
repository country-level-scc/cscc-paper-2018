# Compute statistics via bayesan bootstrap resampling.
require(data.table)
require(bayesboot)
require(pbapply)

print(dr)

# Compute statistics 

comp_stat <- function(.data){
  return(c(
    mean=mean(.data),
    quantile(.data,probs = c(0.167,0.5,0.833))
  ))
}


load("res_stat/raw_scc_SSP1_rcp45_constant_bootstrap_climmean.RData")

res.samp <- pblapply(store_scc_flat, bayesboot, comp_stat, cl = cl, R=10000, R2=10000)

sres.samp <- lapply(res.samp, summary)
tres.samp <- lapply(sres.samp, as.data.table)

stat_cscc_bootstrap = rbindlist(tres.samp)
