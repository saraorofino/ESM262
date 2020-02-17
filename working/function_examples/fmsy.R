# Function to calculate the fishing mortality rate that gives maximum sustainable yield
# Where r = intrinisic growth rate and p = shape parameter (depends on the type of Suprlus Production Model you choose)
# Default shape parameter is 0.2, the global meta-analytic mean for fish, which maximizes productivity at 40% of carrying capacity

fmsy <-function(r,p = 0.2){r * (1 / (1+p))} 

# Test the function:
test_fmsy <- fmsy(r = 0.3)
