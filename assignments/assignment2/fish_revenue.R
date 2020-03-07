#' This is a function to find the most frequently caught fish at each location, the revenue by location, 
#' and total revenue for all locations  
#' 
#' @param catch_data a table of number of fish caught for each species at each location - uses fish names as rownames 
#' @param price_data a table of prices for different fish with "fish" and "price" as column names
#' @param showplot option to show a plot of fishery revenue by location default=FALSE
#' @return list with the following elements
#' \describe{
#' \item{rev_results}{a table of locations with the most frequently caught fish at each location
#' the total revenue of fisheries at that location, and the total revenue from all locations}
#' \item{plt}{Plot of fishery revenue by location with the total revenue as a caption text}
#' }

fish_revenue <- function(catch_data, price_data, showplot = FALSE){
  
  #create vectors for lengths
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
  
  #use which.max on transposed data - for every row finds the column with the highest value, and take the column name 
  #(in this case which.max gives the fish name)
  common_fish <- colnames(catch_data_t)[apply(catch_data_t, 1, which.max)]
  
  #insert the most commmon fish by location into the results data frame
  results$common_fish <- common_fish
  
  #create column names to make sure the function calls the correct information from the input data set
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
    
    #Calculate the revenue for each fish at each location, entered into the blank column of the new catch data frame
    for(f in 1:length(fish)){
      
      #find the revenue for each fish and enter it temporarily into the data frame
      catch_data_2$fish_rev[f] <- round(catch_data_2$price[f]*catch_data_2[f, i], 2) #round to 2 digits
    }
    
    #sum the revenue of each fish at that location
    rev <- sum(catch_data_2$fish_rev)
    #record the total fishery revenue at that location into the results data frame, for every location
    results$total_rev[i] <- rev
  }
  
  #calculate total fisheries revenue by summing location revenues, and create a vector to bind to the results dataset
  total_fish_rev <- c("Total", "", sum(results$total_rev))
  
  #bind the total rev to the end of the results dataframe
  results <- rbind(results, total_fish_rev)
  
  #create column names for the results data frame
  result_colnames <- c("location", "most_frequent_fish", "total_revenue")
  #apply those column names
  colnames(results) <- result_colnames
  
  #If showplot is True plot the revenue for each location with total revenue as text
  if(showplot == TRUE){
    plot_df <- results %>% filter(location != "Total") #filter out the total revenue column
    rev.label <- results$total_revenue[6] #get the total revenue value for the figure caption
    p=ggplot(plot_df,aes(x=location,y=as.numeric(total_revenue))) +
      geom_col(fill = "steelblue4", alpha = 0.9) + 
      scale_x_discrete(expand = c(0,0)) + 
      scale_y_continuous(expand = c(0,0)) +
      labs(x = "Location", y = "Total Fishery Revenue",
           caption = paste("Total Revenue = $", sep = "", rev.label)) + #add the total rev as caption
      theme_light()
  }


  if(showplot == FALSE){
    p=NULL #empty if showplot is false
  }
 
  
  #return the results plus the plot as a list
  return(list(rev_results = results, plt = p))
}

