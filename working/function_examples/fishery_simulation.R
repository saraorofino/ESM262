#Function to simulate a fishery over a 100 year time period given a different magnitude and velocity of climate change.

sim_fishery <- function(b, r, r_s, error, hcr, p = 0.2, k = 10000, years = 100){ 
  
  results <- data.frame(
    b = rep(NA, years), c = rep(NA, years), 
    year = 1:years, r = rep(NA, years), f = rep(NA, years),
    f_msy = rep(NA, years), f_ratio = rep(NA, years), f_ratio_p = rep(NA, years), f_ratio_err = rep(NA, years)) #Setup the results dataframe 
  
  #Set the initial result for the outputs in year 1
  results$b[1] = b
  results$r[1] = r
  f_int = (results$r[1] / p) * (1 - ((results$b[1] / k) ^ p)) #initial f assuming catch = surplus
  
  
  fmsy <-function(r,p){r * (1 / (1+p))} #set up the function to calculate Fmsy based on growth (r) and shape parameter (p)
  r_calc1 <- results$r[1]
  results$f_msy[1] <- fmsy(r=r_calc1, p=p) #calculate fmsy in year 1 based on historical parameters
  results$f_ratio[1] = f_int/results$f_msy[1] #calculate the actual f_ratio from initial fishing pressure 
  results$f_ratio_p[1] = results$f_ratio[1] #this is the perceived f ratio by the fisheries managers - with climate change, this perceived ratio will not adjust fmsy as climate shifts productivity
  
  #Log transform the mean (m) and stdev (s):
  mu_1 <- log(results$f_ratio_p[1]) 
  cv <- error
  sd_1 <- sqrt(log(cv^2+1))
  
  #Draw the f_ratio_err:
  results$f_ratio_err[1] <- rlnorm(1, meanlog = mu_1, sdlog = sd_1)
  
  #Add an error check:
  if(is.na(results$f_ratio_err[1]) == T){
    return("Error in rlnorm calculation in time 1")
  }
  
  #Decide how to change f based on the f ratio estimate with error:
  if(results$f_ratio_err[1] >= 2){
    results$f[1] = hcr*f_int #Close the fishery
  } 
  if(results$f_ratio_err[1] > 1.1 & results$f_ratio_err[1] < 2){
    results$f[1] = hcr*f_int #Reduce by hcr
  }  
  if(results$f_ratio_err[1] > 1 & results$f_ratio_err[1] < 1.1){
    results$f[1] = f_int  #f stays the same in as last year
  }
  if(results$f_ratio_err[1] < 1){
    results$f[1] = 1.05*f_int #f increases by 5% from last year
  } 
  
  #Calculate catch in year one based on the new f:
  results$c[1] = results$f[1] * results$b[1] 
  
  ## Loop the model over the specified number of years - repeat based on assessment interval
  for (t in 2:years) {
    if(results$year[t] %in% assess_int){
      
      results$r[t] = results$r[t-1] + (r_s*results$r[t-1])
      
      ##HCR decision will change how f is calculated - this step must come before calculating c 
      # Calculate fmsy in time t from current r 
      r_calc2 <- results$r[t]
      results$f_msy[t] <- fmsy(r=r_calc2, p=p)
      results$f_ratio[t] <- results$f[t-1]/results$f_msy[t-1] #the ratio at the beginning of the year is based on last years f and fmsy
      results$f_ratio_p[t] <- results$f[t-1]/results$f_msy[1] #constant simulation - NOT peceiving productivity changes 
      
      #Transform to new mean (mu)
      if(results$f_ratio_p[t] != 0){
        #Assign values to normal dist mean (m):
        mu_2 <- log(results$f_ratio_p[t])
        #Draw the f_ratio_err
        results$f_ratio_err[t] <- rlnorm(1, meanlog = mu_2, sdlog = sd_1)
      }
      if(results$f_ratio_p[t] == 0){
        return("Error in f_ratio_p calculation in time t")  
      }
      
      #Add an additional error check:
      if(is.na(results$f_ratio_err[t]) == T){
        return("Error in rlnorm calculation in time t")
      }
      
      #Decisions for f this year based on the ratio with error in the previous year 
      if(results$f_ratio_err[t] >= 2){
        results$f[t] = 0 
      } 
      if(results$f_ratio_err[t] > 1.1 & results$f_ratio_err[t] < 2){
        results$f[t] = hcr*results$f[t-1] #Reduce by hcr - same thing as keeping (hcr%) of previous fishing mortality
      }  
      if(results$f_ratio_err[t] > 1 & results$f_ratio_err[t] < 1.1){
        results$f[t] = results$f[t-1]  #f stays the same in as last year
      }
      if(results$f_ratio_err[t] < 1){
        results$f[t] = 1.05*results$f[t-1] #f increases by 5% from last year
      } 
      
      
      #Calculate remaining results - pay attention to the order!
      results$b[t] = results$b[t-1] + (results$r[t-1] / p)*results$b[t-1]*(1 - ((results$b[t-1]/k) ^ p))-results$c[t-1]
      results$c[t] = results$f[t] * results$b[t]
    } 
    
    if(results$year[t] %not_in% assess_int){
      results$r[t] = results$r[t-1] + (r_s*results$r[t-1])
      results$b[t] = results$b[t-1] + (results$r[t-1] / p)*results$b[t-1]*(1 - ((results$b[t-1]/k) ^ p))-results$c[t-1]
      results$f[t] = results$f[t-1]
      results$c[t] = results$f[t] * results$b[t]
      r_calc3 <- results$r[t]
      results$f_msy[t] = fmsy(r=r_calc3, p=p) #Fmsy needs to update every year to capture productivity changes
      results$f_ratio_err[t] = results$f_ratio_err[t-1]
      results$f_ratio[t] = results$f[t-1]/results$f_msy[t-1]
      results$f_ratio_p[t] = results$f_ratio_p[t-1]
      
    }
  }
  return(results)
}