#### Preparation ####

# Load the required libraries
library(dplyr) # Manipulating data
library(stringr)# common string operations

# Set working directoty to locaiton of current script
setwd(file.path(dirname(rstudioapi::getSourceEditorContext()$path)))

# Note the secure data path
secureDataFolder <- file.path("..", "data", "secure")

# Read in the raw trade data from secure folder of the repository 
cpi_file <- file.path(secureDataFolder, "CPI.csv")
cpi_stats <- read.csv(cpi_file)

### Look at the data ####

# Summary of the data frame.
summary(cpi_stats)
sapply(cpi_stats, class)

#### Process the data ####

#Convert columns to numeric
#Using the transform function for the converting
column_name_combinations <- expand.grid(c("Mar", "Jun", "Sep", "Dec"), c("08", "09", 10:19))
quarterly_columns <- paste(column_name_combinations[, 1], column_name_combinations[, 2], sep = ".")
quarterly_columns <- quarterly_columns[quarterly_columns %in% colnames(cpi_stats)]
cpi_stats[quarterly_columns] <- sapply(cpi_stats[quarterly_columns], FUN = as.numeric)

# Remove duplicated rows from the CPI data
duplicated_rows <- duplicated(cpi_stats) 
cpi_stats <- cpi_stats[duplicated_rows == FALSE, ]

#### Estimate missing values ####

# Count number of missing values
sum(is.na(cpi_stats_without_duplicates)) #Finding the total sum of the NA.

# Function to get previous 6 column names
get_previous_columns <- function(current_column, column_names, n = 6){
  
  # Get index of current column
  current_column_index <- which(column_names == current_column)
  
  # Note the indices of the previous columns
  indices_of_previous_columns <- (current_column_index - n):(current_column_index - 1)
  indices_of_previous_columns <- indices_of_previous_columns[indices_of_previous_columns > 0]
  
  # Initialise a vector to store the previous columns
  previous_columns <- NULL
  
  # Check if previous columns available
  if(length(indices_of_previous_columns) > 0){
    
    # Select the previous n values
    previous_columns <- column_names[indices_of_previous_columns]
  }
  
  return(previous_columns)
}

# Function to replace NA values with estimates based on the previous quarters
estimate_NAs_based_previous_quarters <- function(cpi_stats, quarterly_columns, 
                                                 n_previous_columns_to_use){
  
  # Make a copy of the CPI data - don't want to be based estimate means on previously estimate values
  cpi_stats_with_estimates <- cpi_stats
  
  # Examine each quarter column - note ignoring the first one as no previous data available to estimate from
  for(quarterly_column in quarterly_columns[-1]){
    
    # Get the indices of NA values in current column
    indices_of_NAs <- which(is.na(cpi_stats[, quarterly_column]))
    
    # Get the previous quarterly value columns that we're going to use to estimate from
    previous_columns <- get_previous_columns(quarterly_column, quarterly_columns,
                                             n = n_previous_columns_to_use)
    
    # If in second quarter - use previous quarter directly, no need to calculate mean as only one value available
    if(length(previous_columns) == 1){
      cpi_stats_with_estimates[indices_of_NAs, quarterly_column] <- cpi_stats[indices_of_NAs, previous_columns]
      
    # Otherwise replace with estimated values based on previous quarters
    }else{
      
      # Count the number of NA values in the previous columns for each row
      na_counts_by_row <- rowSums(is.na(cpi_stats[indices_of_NAs, previous_columns]))
      
      # Remove the indices of NA values for which the previous columns are also NAs
      # Can't estimate value for NA when the previous quarters used also had NA values
      indices_of_NAs <- indices_of_NAs[na_counts_by_row < n_previous_columns_to_use]
      
      # Replace the remaining NAs with estimates based on the previous quarters
      cpi_stats_with_estimates[indices_of_NAs, quarterly_column] <- rowMeans(cpi_stats[indices_of_NAs, previous_columns], na.rm = TRUE)
    }
  }
  
  return(cpi_stats_with_estimates)
}

# Estimate NA values for each quarter based on the estimates of the previous (6) quarters
cpi_stats_with_estimates <- estimate_NAs_based_previous_quarters(cpi_stats, quarterly_columns, 
                                                                 n_previous_columns_to_use = 6)





