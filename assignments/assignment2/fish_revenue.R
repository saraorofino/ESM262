# Function to find the most frequently caught fish at each location, the total revenue for each fishery, and total revenue by location
# Inputs:
## price - a table of prices for different fish
## fish_data - a table of the number of fish caught in each species at each location 


#Outputs:
#most frequently caught fish in each location use names(which.max(summary(df)))
#total revenue for each location
#total fisheries revenue sum -- this doesn't make sense to me based on every other output being location dependent
#probably wants outputs as a list 

##3.2.20 - the fish_data input is supposed to be locations as columns and just number of each type of fish caught - don't need to subset by location. Finishing making input data in correct form then revise function

fish_revenue <- function(fish_data, fish_prices){
  
  #only want results for the number of unique locations (not entire length of input data)
  unique_loc <- unique(fish_data$location)
  
  #Set up a results dataframe for the location dependent information
  results <- data.frame(location = rep(NA, length(unique_loc)),
                        common_fish = rep(NA, length(unique_loc)),
                        total_rev = rep(NA, length(unique_loc)))
  
  #Fishery results should be based on the number of unique fisheries
  unique_fish <- unique(fish_data$fish)
  
  #Set up a dataframe for the fishery dependent information
  fish.results = data.frame(fishery = rep(NA, length(unique_fish)),
                            fishery_rev = rep(NA, length(unique_fish)))
  
  #Want to have the fish data as a factor:
  fish_data$fish = as.factor(fish_data$fish)
  
  #Compute all the location depedent results:
  for(i in 1:length(unique_loc)){
    
    results$location[i] <- unique_loc[i] #set the location
    
    #Subset by location to find which fish had the highest catch for each location
    location_name <- results$location[i]
    sub_location <- subset(fish_data, location == location_name)
    sub_max <- names(which.max(summary(sub_location$fish)))
    results$common_fish[i] <- sub_max
    
    #Calculate the revenue for each location - add a price column to the subsetted data, find price for each fish & calc revenue
    for(j in 1:7){
    fish.name <- sub_location$fish[j]
    fish.price <- fish_prices$price[fish_prices$fish == fish.name]
    sub_location$fish_price[j] <- fish.price
    fish.catch <- summary(sub_location$fish)[j]
    sub_location$revenue[j] <- fish.price * fish.catch
    }

    total_rev <- sum(sub_location$revenue)
    results$total_rev[i] <- total_rev
  }
  
  #Compute the fishery dependent results
  for(f in 1:length(unique_fish)){
    fish.results$fishery <- unique_fish[f] #set the fishery
    fishname <- fish.results$fishery[f]
    sub_fishery <- subset(fish_data, fish == fishname)
    fishprice <- fish_prices$price[fish_prices$fish == fishname]
    fishcatch <- summary(sub_fishery$fish)[f]
    sub_fishery$fishrev[f] <- fishprice * fishcatch
    
    fish.results$fishery_rev[f] <- sub_fishery$fishrev[f]
  }
  
  return(list(location_results = results, fishery_results = fish.results))
}


#Test:
test <- fish_revenue(fish_data = catch, fish_prices = fish.prices)
