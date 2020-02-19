# This set of functions was used to calculate a z-score-index of social vulnerability based on four different metrics of socioeconomic status. # The z-score for each metric is calculated and then mean of each individual z-score is taken to give the z-score-index 


# Function to compute the z-score for a single vector
# Can be used alone for just one variable or run to feed into the z-score index function below
z_score <- function(vec) {
  
  if(length(vec) == 1){
    return("input must be a vector") #Return error message if the input is a single value
  }
  
  # Can only use on numeric variables 
  if(is.numeric(vec)==T){
    vec <- vec
  } else {
    vec <- as.numeric(vec)
  }
  u <- mean(vec, na.rm=T) # find the mean of the vector
  stdev <- sd(vec, na.rm=T) # find the standard deviation of the vector
  
  z <- (vec-u)/stdev # calculate z-score for each number in the vector
  return(z)
}

# Test the function:
test_data <- c(NA, rnorm(10), NA) # randomly draw a vector of 10 numbers from a normal distribution, include NA's to test function
z_test <- z_score(vec = test_data)
z_score(vec = test_data)


# Use this function to compute a z-score index that combines the z-scores of multiple columns 
z_score_index <- function(dta, vars){
  f_dat <- dta[,vars] # subset dataframe 
  z_mat <- matrix(nrow = nrow(f_dat), ncol = ncol(f_dat)) # create a matrix for the z-scores
  for (i in 1:length(vars)){
    z_mat[,i] <- z_score(f_dat[,vars[i]]) # calculate the z-score for each number
  }
  index <- rowMeans(z_mat, na.rm = T) # take the mean of all the individual z-scores
  return(index)
}

# Test the function:
a <- rnorm(100,5,2)
b <- rnorm(100,3,8)
c <- rnorm(100,1,4)
dt <- data.frame(a,b,c)

z_score_index(dt, vars = c("a", "b", "c"))
