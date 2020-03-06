# Function to find the most frequently caught fish at each location, the total revenue for each fishery, and total revenue by location
# Inputs:
## price - a table of prices for different fish
## fish_data - a table of the number of fish caught in each species at each location 


#Outputs:
#most frequently caught fish in each location
#total revenue for each location
#total fisheries revenue sum

fish_revenue <- function(catch_data, price_data){
  
  #create lists for lengths
  #one of locations
  #one of fish types
  places <- c("Alaska", "California", "Mexico", "North Carolina", "Maine")
  fish <- rownames(catch_data)
  
  #transpose catch data, to be able to use which.max
  catch_data_t <- t(catch_data)
  
  #create the results table - length should only be the number of locations
  results <- data.frame(location = rep(NA, length(places)),
                        common_fish = rep(NA, length(places)),
                        total_rev = rep(NA, length(places)))
  
  #use which.max on transposed data, for every row this function finds the column with the highest value, and then takes the column name (in this case, the fish name)
  common_fish <- colnames(catch_data_t)[apply(catch_data_t, 1, which.max)]
  
  #insert the most commmon fish by location into the results data frame
  results$common_fish <- common_fish
  
  #creat column names to make sure the function calls the correct information from the input data set
  col_names <- c("fish", "price")
  
  #set the column names of the price_data input so that when we call from the data, it actually grabs the price info
  colnames(price_data) <- col_names
  
  #create a new dataset, identical to the input catch data, that we can then manipulate to get the info we need
  catch_data_2 <- catch_data
  
  #add a column to the new catch data frame, that pulls the price info from the fish price data
  catch_data_2$price <- price_data$price
  
  #create an empty column in the new catch data frame, to fill up with catch revenues for the fish during the for loop
  catch_data_2$fish_rev <- rep(NA, length(fish))
  
  #now, run a loop for each location, with a nested loop for each fish, to calculate total revenue by location
  for(i in 1:length(places)){
    
    #fill in the location column of the results dataframe
    results$location[i] <- places[i]
    
    #Calculate the revenue for each fish catch at each location, entered into the blank column of the new catch data frame
    for(f in 1:length(fish)){
      #multiply the price of the fish * the amount of that fish caught at that location, and enter it temporarily into the data frame
      catch_data_2$fish_rev[f] <- catch_data_2$price[f]*catch_data_2[f, i]
    }
    #sum the revenue of each fish at that location
    rev <- sum(catch_data_2$fish_rev)
    #record the total fishery revenue at that location into the results data frame, for every location
    results$total_rev[i] <- rev
  }
  #create column names for the results data frame
  result_colnames <- c("Location", "Most Frequent Fish", "Total Fisheries Revenue")
  #apply those column names
  colnames(results) <- result_colnames
  #return the results
  return(results)
}


#Test:
#test <- fish_revenue(catch_data = catch_data, price_data = fish_prices)
