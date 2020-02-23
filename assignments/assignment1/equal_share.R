# Function to calculate a theoretical equal share of FEMA funding for all disasters in Puerto Rico since 2001
# A theoretical equal share is the per capita funding a municipality should get if all the funding awarded after the disaster was allocated equally across all municipalities where the disaster was declared

equal_share <- function(disasters, fund1, fund2, input_df){
  
  # Error check
  ## Loop is based on length of the inputs so they need to be the same
  if(length(disasters) != length(fund1) | length(disasters) != length(fund2)){
    return("disasters and funding must have the same number of observations")
  }

  # Set up the results dataframe
  results <- data.frame(
    pop_sum  = rep(NA, length(disasters)),
    funding1 = rep(NA, length(disasters)),
    funding2 = rep(NA, length(disasters)),
    fund_sum = rep(NA, length(disasters)),
    equal_share_mil = rep(NA, length(disasters)),
    equal_share_dollar = rep(NA, length(disasters))
  )
  
  # Calculate those results for every federally declared disaster since 2001
  for(i in 1:length(disasters)){
    
    # Error check
    ## Subsetting requires the disasters input to be binary 0/1 
    check_disaster <- unique(input_df[,disasters[i]])
    if(check_disaster > 2){
      return("input data for disaster inclusion must be binary")
    }
    
    # First subset the data to include only municipalities included in the declaration
    # 0 = not included, 1 = included
    input_df$disaster <- input_df[,disasters[i]]
    data_sub <- subset(input_df, disaster == 1)
    
    # Find the total population of people across all included municipalities
    results$pop_sum[i] = sum(data_sub$totpop)
    
    # Subset the first type of funding (Public Assistance) and find the total
    fund_type1 <- data_sub[,fund1[i]]
    results$funding1[i] = sum(fund_type1, na.rm = T)
    
    # Subset the second type of funding (Hazard Mitigation Grants Program) and find the total
    fund_type2 <- data_sub[,fund2[i]]
    results$funding2[i] = sum(fund_type2, na.rm = T)
    
    # Add both funding types to get a sum of FEMA funding paid after each disaster
    results$fund_sum[i] = sum(results$funding1[i]) + sum(results$funding2[i])
    
    # Calculate the equal share 
    ## Store the values from the results to use in the calculation
    fundsum_calc <- results$fund_sum[i]
    pop_calc <- results$pop_sum[i]
    
    ## Set up the calculation
    es <- function(funding, population){equal_share = funding/population}
    
    ## Calculate the equal share per capita funding in millions and in dollars
    results$equal_share_mil[i] = es(funding = fundsum_calc, population = pop_calc)
    results$equal_share_dollar[i] = results$equal_share_mil[i] * 1000000

  }
  return(results)
}




