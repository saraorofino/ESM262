# Function to randomly generate a mix of energy from different sources to meet the global energy demand 
# Where energy is a vector of the different energy sources (i.e. s = solar, w = wind etc) 
# Probs is the probability of drawing each type of energy - this can be adjusted based on global energy potential 
# N is the amount of energy in TWh that you want to draw (i.e. the global energy demand)  
# Each time a type of energy drawn it represents 1 TWh of energy from that source



energy_mixes <- function(energy, demand, probs){
  library(tidyverse)
  if(length(probs) != length(energy)){
    return("length of probabilities must match energy types")
  }
  
  if(demand < 0){
    return("energy demand must be positive")
  }
 
  out <- sample(energy, size = demand, replace = TRUE, prob = probs) # randomly draw a mix of energy to meet global demand
  return(as.data.frame(out) %>% 
           rename(energy_type = "out") %>%
           group_by(energy_type) %>% # determine the amount of energy in TWh from each source
           tally() %>% 
           rename(TWh = "n") %>% 
           mutate(percent_of_demand = round(((TWh/23000)*100), 2))) # calculate what percent of the demand comes from each source
}



# Test the function:
# Create your vector of energy types, size and probabilty:
# cc = carbon capture, s = solar, w = wind, sw = solar/wind co-generation, g = geothermal, b = biofuels, n = nuclear
energy_types <- c("cc", "s", "w", "sw", "g", "b", "n")
global_demand <- 23000 #definee the amount of energy in TWh needed to meet global energy demand
energy_prob <- c(0.30, 0.50, 0.50, 0.25, 0.15, 0.45, 0.20) # probability for each type - must be the same length/in the same order as 'energy'

mix1 <- energy_mixes(energy = energy_types, demand = global_demand, probs = energy_prob)
mix2 <- energy_mixes(energy = energy_types)


