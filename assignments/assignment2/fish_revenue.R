# Function to find the most frequently caught fish at each location, the total revenue for each fishery, and total revenue by location
# Inputs:
## price - a table of prices for different fish
## fish_data - a table of the number of fish caught in each species at each location 


#Outputs:
#most frequently caught fish in each location
#total revenue for each location
#total fisheries revenue sum

fish_revenue <- function(fish_data, price){
  
  #create the results table - length should only be the number of unique values for the location
  unique_loc <- unique(fish_data$location)
  
  results <- data.frame(location = rep(NA, length(unique_loc)),
                        common_fish = rep(NA, length(unique_loc)),
                        total_rev = rep(NA, length(unique_loc)))
  
  for(i in 1:length(unique_loc)){
    
    results$location[i] <- unique_loc[i] #set the location
    
    #Subset by location to find which fish had the highest catch for each location
    location_value <- results$location[i]
    sub_location <- subset(fish_data, location == location_value)
    sub_max <- sub_location$fish[which.max(sub_location$catch)]
    results$common_fish[i] <- sub_max
    
    #Calculate the revenue for each location - add a price column to the subsetted data, find price for each fish & calc revenue
    for(f in 1:7){
    fish <- sub_location$fish[f] 
    fish.price <- price$price[price$fish == fish]
    sub_location$price[f] <- fish.price
    fish.catch <- sub_location$catch[f]
    sub_location$revenue[f] <- fish.price * fish.catch
    }
    
    total_rev <- sum(sub_location$revenue)
    results$total_rev[i] <- total_rev
  }
  
  return(results)
}


#Test:
test <- fish_revenue(fish_data = catch, price = prices)
