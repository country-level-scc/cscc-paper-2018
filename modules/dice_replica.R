# DICE Damage function on level of economic productivity
# Uses DICE-2016R

#http://www.econ.yale.edu/~nordhaus/homepage/homepage/DICE2016R-091916ap.gms
# economic impact, damage coefficient = D/(1+D) #DICE 2016R
# T is the temperature of atmosphere increase (degrees C from 1900)
# Damfrac, D = 0.00236Tˆ2 Damage as fraction of gross output
# damage function = 1 - damfrac
# Damages = Ygross * damfrac in trillions of USD per year
# Ynet = Ygross * (damage function) output net in trillions of USD per year

# Dataset accessed 2021-07-22 at https://data.giss.nasa.gov/gistemp/
# contains temperature anomalies in Celcius compared to mean from 1951-1980

temp_csv = fread(file.path('data','NASA_temp_change_1900-2020.csv'))
temp_anomaly = select(temp_csv, "Year","J-D") # select yearly anomaly 
anomaly_years <- c(1900,2020)
temp_anomaly <- temp_anomaly[Year %in% anomaly_years] 
# find increase of temperature from 1900 to 2020 by taking the difference of the anomalies
temp_history <- as.numeric(temp_anomaly[Year == 2020, "J-D"][[1]]) - as.numeric(temp_anomaly[Year == 1900, "D-N"][[1]])

warming_effect <- function(temp, temp_prev, gdp_tm1, nid, out_of_sample=T, temp_history){
  # temperature increase since 1900. temp_history is global T increase, from 2020 is national increase
  t_diff = temp_history + (temp - temp_prev)
  damcoeff = 0.00236 * (t_diff) ^2
  return(damcoeff)  # Damages as fraction of gross output
}
