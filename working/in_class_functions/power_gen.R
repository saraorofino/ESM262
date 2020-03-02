power_gen = function(height, flow, rho=1000, g=9.8, Keff=0.8) {
  # make sure inputs are positive and if not set as NA so result will be NA
  height = ifelse( (height < 0), NA, height)
  
  # an alterative is to exit the function prematurely 
  flow = ifelse((flow < 0), return("flow must be greater than zero"), flow)
  
  #    return("Height cannot be less than 0")
  
  # calculate power
  result = rho * height * flow * g * Keff
  return(result)
}
