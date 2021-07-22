# DICE Damage function on level of economic productivity
# Uses DICE-2016R

# http://www.econ.yale.edu/~nordhaus/homepage/homepage/DICE2016R-091916ap.gms
# Model: impact of climate change = -0.19Tˆ2
# economic impact, damage coefficient = D/(1+D) #DICE 2016R
# Damfrac, D = 0.00236Tˆ2 Damage as fraction of gross output
# damage function = 1 - damfrac

# Damages = Ygross * damfrac in trillions of USD per year
# Ynet = Ygross * (damage function) output net in trillions of USD per year

library(dplyr)

warming_effect <- function(temp, temp_prev, gdp_tm1, nid, out_of_sample=T){
  dam = 0.00236 * temp^2
  dam_prev =0.00236* temp_prev^2
  damcoeff = dam - dam_prev
  return(damcoeff)
}

