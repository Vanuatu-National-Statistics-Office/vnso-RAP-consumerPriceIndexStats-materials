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

#### Process the data ####

# Convert columns to numeric
column_name_combinations <- expand.grid(c("Mar", "Jun", "Sep", "Dec"), c("08", "09", 10:21))
quarterly_columns <- paste(column_name_combinations[, 1], column_name_combinations[, 2], sep = ".")
quarterly_columns <- quarterly_columns[quarterly_columns %in% colnames(cpi_stats)]
cpi_stats[quarterly_columns] <- sapply(cpi_stats[quarterly_columns], FUN = as.numeric)

# Remove duplicated rows from the CPI data
cpi_stats <- cpi_stats[duplicated(cpi_stats) == FALSE, ]

#### Estimate missing values ####

# Function to get previous column names
get_previous_columns <- function(current_column, column_names, n = 6){
  
  # Generate lagged list of column names
  lagged_column_names <- c(rep(NA, n), column_names)
  
  # Select the previous n column names
  current_column_lagged_index <- which(lagged_column_names == current_column)
  previous_columns <- lagged_column_names[(current_column_lagged_index - n):(current_column_lagged_index - 1)]
  
  # Remove NA values (for when column within n of start)
  previous_columns <- previous_columns[is.na(previous_columns) == FALSE]
  
  return(previous_columns)
}

# Function to replace NA values with estimates based on the previous quarters
estimate_NAs_based_previous_quarters <- function(cpi_stats, quarterly_columns, 
                                                 n_previous_columns_to_use){
  
  # Make a copy of the CPI data - don't want to be base estimated means on previously estimate values
  cpi_stats_with_estimates <- cpi_stats
  
  # Examine each quarter column - note ignoring the first one as no previous data available to estimate from
  for(quarterly_column in quarterly_columns[-1]){
    
    # Get the indices of NA values in current column
    row_indices_of_NAs <- which(is.na(cpi_stats[, quarterly_column]))
    
    # Get the previous quarterly value columns that we're going to use to estimate from
    previous_columns <- get_previous_columns(quarterly_column, quarterly_columns,
                                             n = n_previous_columns_to_use)
    
    # If in second quarter - use previous quarter directly, no need to calculate mean as only one value available
    if(length(previous_columns) == 1){
      cpi_stats_with_estimates[row_indices_of_NAs, quarterly_column] <- cpi_stats[row_indices_of_NAs, previous_columns]
      
    # Otherwise replace with estimated values based on previous quarters
    }else{
      
      # Count the number of NA values in the previous columns for each row
      na_counts_by_row <- rowSums(is.na(cpi_stats[row_indices_of_NAs, previous_columns]))
      
      # Remove the indices of NA values for which the previous columns are also NAs
      # Can't estimate value for NA when the previous quarters used also had NA values
      row_indices_of_NAs <- row_indices_of_NAs[na_counts_by_row < length(previous_columns)]
      
      # Replace the remaining NAs with estimates based on the previous quarters
      cpi_stats_with_estimates[row_indices_of_NAs, quarterly_column] <- 
        rowMeans(cpi_stats[row_indices_of_NAs, previous_columns], na.rm = TRUE)
    }
  }
  
  return(cpi_stats_with_estimates)
}

# Estimate NA values for each quarter based on the estimates of the previous (6) quarters
cpi_stats_with_estimates <- estimate_NAs_based_previous_quarters(cpi_stats, quarterly_columns, 
                                                                 n_previous_columns_to_use = 6)
