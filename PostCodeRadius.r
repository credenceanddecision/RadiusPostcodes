library(geosphere)
library(dplyr)
library(pbapply)

setwd("your/directory/path")

# Function to write results to file
write_results_to_file <- function(results, filename) {
  if (file.exists(filename)) {
    write.table(results, filename, row.names = FALSE, col.names = FALSE, sep = ",", append = TRUE)
  } else {
    write.csv(results, filename, row.names = FALSE)
  }
}

# Function to find UK postcodes within 10 miles of a football club
find_nearby_postcodes <- function(club, uk_postcodes_df) {
  coords_club <- c(club$longitude, club$latitude)
  distances <- distm(as.matrix(uk_postcodes_df[, c("longitude", "latitude")]), coords_club, fun = distHaversine) / 1609.34 # Convert meters to miles
  nearby_postcodes <- uk_postcodes_df[distances <= 10, ]
  nearby_postcodes$nearest_club <- club$club_name # Add nearest club name
  nearby_postcodes$distance_to_club <- distances[distances <= 10]
  return(nearby_postcodes)
}

# Read the UK postcodes data
uk_postcodes <- read.csv('ukpostcodes.csv')

# Load the football clubs data
football_clubs <- read.csv('addresses.csv') # Assuming the file name is addresses.csv

# Initialize an empty dataframe to store results
combined_results <- data.frame()

# Setup progress bar
total_clubs <- nrow(football_clubs)
progress_bar <- txtProgressBar(min = 0, max = total_clubs, style = 3)

# Process and update file every 100k records with progress bar
for (i in 1:total_clubs) {
  club_results <- find_nearby_postcodes(football_clubs[i, ], uk_postcodes)
  combined_results <- rbind(combined_results, club_results)
  
  # Update progress bar
  setTxtProgressBar(progress_bar, i)
  
  # Check if the number of records exceeds 100k and write to file
  if (nrow(combined_results) >= 100000) {
    write_results_to_file(combined_results, 'nearby_postcodes.csv')
    combined_results <- data.frame() # Reset the dataframe after writing
  }
}

# Write any remaining data that didn't reach the 100k threshold
if (nrow(combined_results) > 0) {
  write_results_to_file(combined_results, 'nearby_postcodes.csv')
}

# Close progress bar
close(progress_bar)
