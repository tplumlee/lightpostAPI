# Set base request endpoint URL
BASE_URL <- "https://api.lightcast.io/jpa/"

# Create new environment to store persistent variables
# see https://r-pkgs.org/data.html#sec-data-state 
the <- new.env(parent = emptyenv())