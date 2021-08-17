#### Preparation ####

# Clear the environment
rm(list = ls())

# Load the required libraries
library(dplyr) # Manipulating data
library(stringr)# common string operations


# Note where VNSO code/data is on current computer
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path), "..", "..")
setwd(repository) # Required for file.choose() function

# Load the general R functions
source(file.path(repository, "R", "functions.R"))

# Note the secure data path
secureDataFolder <- file.path(repository, "data", "secure")

# Note the open data path
openDataFolder <- file.path(repository, "data", "open")

#### Forthnight Prices and Weights####

# Read in file for Prices
forthnightFolder <- file.path(repository, "data", "secure", "Forthnight Collections")

firstForthnightPrice_file <- file.path(forthnightFolder, "CPI_Forthnightly_Price_F1.csv")
portVilaF1Price_stats <- read.csv (firstForthnightPrice_file)

secondForthnightPrice_file <- file.path(forthnightFolder, "CPI_Forthnightly_Price_F2.csv")
portVilaF2Price_stats <- read.csv (secondForthnightPrice_file)

thirdForthnightPrice_file <- file.path(forthnightFolder, "CPI_Forthnightly_Price_F3.csv")
portVilaF3Price_stats <- read.csv (thirdForthnightPrice_file)

fourthForthnightPrice_file <- file.path(forthnightFolder, "CPI_Forthnightly_Price_F4.csv")
portVilaF4Price_stats <- read.csv (fourthForthnightPrice_file)

fifthForthnightPrice_file <- file.path(forthnightFolder, "CPI_Forthnightly_Price_F5.csv")
portVilaF5Price_stats <- read.csv (fifthForthnightPrice_file)

sixthForthnightPrice_file <- file.path(forthnightFolder, "CPI_Forthnightly_Price_F6.csv")
portVilaF6Price_stats <- read.csv (sixthForthnightPrice_file)

#Read in the file for weights

firstForthnightWeights_file <- file.path(forthnightFolder, "CPI_Forthnightly_Weights_F1.csv")
portVilaF1Weights_stats <- read.csv (firstForthnightWeights_file)

secondForthnightWeights_file <- file.path(forthnightFolder, "CPI_Forthnightly_Weights_F2.csv")
portVilaF2Weights_stats <- read.csv (secondForthnightWeights_file)

thirdForthnightWeights_file <- file.path(forthnightFolder, "CPI_Forthnightly_Weights_F3.csv")
portVilaF3Weights_stats <- read.csv (thirdForthnightWeights_file)

fourthForthnightWeights_file <- file.path(forthnightFolder, "CPI_Forthnightly_Weights_F4.csv")
portVilaF4Weights_stats <- read.csv (fourthForthnightWeights_file)

fifthForthnightWeights_file <- file.path(forthnightFolder, "CPI_Forthnightly_Weights_F5.csv")
portVilaF5Weights_stats <- read.csv (fifthForthnightWeights_file)

sixthForthnightWeights_file <- file.path(forthnightFolder, "CPI_Forthnightly_Weights_F6.csv")
portVilaF6Weights_stats <- read.csv (sixthForthnightWeights_file)


####Convert the raw data to numeric ####

column_name_combinations <- expand.grid(c("Mar", "Jun", "Sep", "Dec"), c("08", "09", 10:21))
quarterly_columns <- paste(column_name_combinations[, 1], column_name_combinations[, 2], sep = ".")
quarterly_columns <- quarterly_columns[quarterly_columns %in% colnames(portVilaF1Price_stats)]
portVilaF1Price_stats[quarterly_columns] <- sapply(portVilaF1Price_stats[quarterly_columns], FUN = as.numeric)

column_name_combinations <- expand.grid(c("Mar", "Jun", "Sep", "Dec"), c("08", "09", 10:21))
quarterly_columns <- paste(column_name_combinations[, 1], column_name_combinations[, 2], sep = ".")
quarterly_columns <- quarterly_columns[quarterly_columns %in% colnames(portVilaF2Price_stats)]
portVilaF2Price_stats[quarterly_columns] <- sapply(portVilaF2Price_stats[quarterly_columns], FUN = as.numeric)

column_name_combinations <- expand.grid(c("Mar", "Jun", "Sep", "Dec"), c("08", "09", 10:21))
quarterly_columns <- paste(column_name_combinations[, 1], column_name_combinations[, 2], sep = ".")
quarterly_columns <- quarterly_columns[quarterly_columns %in% colnames(portVilaF3Price_stats)]
portVilaF3Price_stats[quarterly_columns] <- sapply(portVilaF3Price_stats[quarterly_columns], FUN = as.numeric)

column_name_combinations <- expand.grid(c("Mar", "Jun", "Sep", "Dec"), c("08", "09", 10:21))
quarterly_columns <- paste(column_name_combinations[, 1], column_name_combinations[, 2], sep = ".")
quarterly_columns <- quarterly_columns[quarterly_columns %in% colnames(portVilaF4Price_stats)]
portVilaF4Price_stats[quarterly_columns] <- sapply(portVilaF4Price_stats[quarterly_columns], FUN = as.numeric)

column_name_combinations <- expand.grid(c("Mar", "Jun", "Sep", "Dec"), c("08", "09", 10:21))
quarterly_columns <- paste(column_name_combinations[, 1], column_name_combinations[, 2], sep = ".")
quarterly_columns <- quarterly_columns[quarterly_columns %in% colnames(portVilaF5Price_stats)]
portVilaF5Price_stats[quarterly_columns] <- sapply(portVilaF5Price_stats[quarterly_columns], FUN = as.numeric)

column_name_combinations <- expand.grid(c("Mar", "Jun", "Sep", "Dec"), c("08", "09", 10:21))
quarterly_columns <- paste(column_name_combinations[, 1], column_name_combinations[, 2], sep = ".")
quarterly_columns <- quarterly_columns[quarterly_columns %in% colnames(portVilaF6Price_stats)]
portVilaF6Price_stats[quarterly_columns] <- sapply(portVilaF6Price_stats[quarterly_columns], FUN = as.numeric)


# Read in the raw trade data from secure folder of the repository 
cpi_file <- file.path(secureDataFolder, "CPI.csv")
cpi_stats <- read.csv(cpi_file) #replace blank cells with missing values-NA

# Convert columns to numeric
column_name_combinations <- expand.grid(c("Mar", "Jun", "Sep", "Dec"), c("08", "09", 10:21))
quarterly_columns <- paste(column_name_combinations[, 1], column_name_combinations[, 2], sep = ".")
quarterly_columns <- quarterly_columns[quarterly_columns %in% colnames(cpi_stats)]
cpi_stats[quarterly_columns] <- sapply(cpi_stats[quarterly_columns], FUN = as.numeric)

#### Remove duplicated rows from the CPI data####
cpi_stats <- cpi_stats[duplicated(cpi_stats) == FALSE, ]
portVilaF1Price_stats <- portVilaF1Price_stats[duplicated(portVilaF1Price_stats) == FALSE, ]
portVilaF2Price_stats <- portVilaF2Price_stats[duplicated(portVilaF2Price_stats) == FALSE, ]
portVilaF3Price_stats <- portVilaF3Price_stats[duplicated(portVilaF3Price_stats) == FALSE, ]
portVilaF4Price_stats <- portVilaF4Price_stats[duplicated(portVilaF4Price_stats) == FALSE, ]
portVilaF5Price_stats <- portVilaF5Price_stats[duplicated(portVilaF5Price_stats) == FALSE, ]
portVilaF6Price_stats <- portVilaF6Price_stats[duplicated(portVilaF6Price_stats) == FALSE, ]

####Restructing the datasets for the weights forthnight####

#Remove the last row of this dataframe, so that it can have the same size. The last row was an NA.
portVilaF6Wgts_new <- head(portVilaF6Weights_stats, - 1)              

colQtrWgts_name <- data.frame(portVilaF2Price_stats$ItemId, portVilaF2Price_stats$Eas, portVilaF2Price_stats$Specification,portVilaF2Price_stats$LocalDescription, portVilaF2Price_stats$Outlet, 
                              portVilaF2Price_stats$Address, portVilaF2Price_stats$CollDay, portVilaF2Price_stats$Active)
names(colQtrWgts_name) <- c("ItemId", "Eas", "Specification", "Local Description", "Outlet", "Address", "Col Day", "Active")


colQtr_Dec08 <- data.frame(portVilaF1Weights_stats$Dec.08, portVilaF2Weights_stats$Dec.08, portVilaF3Price_stats$Dec.08,
                              portVilaF4Weights_stats$Dec.08, portVilaF5Weights_stats$Dec.08, portVilaF6Wgts_new$Dec.08)
names(colQtr_Dec08) <- c("Dec.08F1", "Dec.08F2", "Dec.08F3", "Dec.08F4", "Dec.08F5", "Dec.08F6")


ColQtr_Mar09 <- data.frame(portVilaF1Weights_stats$Mar.09, portVilaF2Weights_stats$Mar.09, portVilaF3Price_stats$Mar.09,
                              portVilaF4Weights_stats$Mar.09, portVilaF5Weights_stats$Mar.09, portVilaF6Wgts_new$Mar.09)
names(ColQtr_Mar09) <- c("Mar.09F1", "Mar.09F2", "Mar.09F3", "Mar.09F4", "Mar.09F5", "Mar.09F6")

colQtr_Jun09 <- data.frame(portVilaF1Weights_stats$Jun.09, portVilaF2Weights_stats$Jun.09, portVilaF3Price_stats$Jun.09,
                              portVilaF4Weights_stats$Jun.09, portVilaF5Weights_stats$Jun.09, portVilaF6Wgts_new$Jun.09)
names(colQtr_Jun09) <- c("Jun.09F1", "Jun.09F2", "Jun.09F3", "Jun.09F4", "Jun.09F5", "Jun.09F6")                              
  
colQtr_Sep09 <- data.frame(portVilaF1Weights_stats$Sep.09, portVilaF2Weights_stats$Sep.09, portVilaF3Price_stats$Sep.09,
                              portVilaF4Weights_stats$Sep.09, portVilaF5Weights_stats$Sep.09, portVilaF6Wgts_new$Sep.09)
names(colQtr_Sep09) <- c("Sep.09F1", "Sep.09F2", "Sep.09F3", "Sep.09F4", "Sep.09F5", "Sep.09F6")                           

                              
colQtr_Dec09 <- data.frame(portVilaF1Weights_stats$Dec.09, portVilaF2Weights_stats$Dec.09, portVilaF3Price_stats$Dec.09,
                              portVilaF4Weights_stats$Dec.09, portVilaF5Weights_stats$Dec.09, portVilaF6Wgts_new$Dec.09)
names(colQtr_Dec09) <- c("Dec.09F1", "Dec.09F2", "Dec.09F3", "Dec.09F4", "Dec.09F5", "Dec.09F6")                              

colQtr_Mar10 <- data.frame (portVilaF1Weights_stats$Mar.10, portVilaF2Weights_stats$Mar.10, portVilaF3Price_stats$Mar.10,
                              portVilaF4Weights_stats$Mar.10, portVilaF5Weights_stats$Mar.10, portVilaF6Wgts_new$Mar.10) 
names(colQtr_Mar10) <- c ( "Mar.10F1", "Mar.10F2", "Mar.10F3", "Mar.10F4", "Mar.10F5", "Mar.10F6" )                             
                              
colQtr_Jun10 <- data.frame (portVilaF1Weights_stats$Jun.10, portVilaF2Weights_stats$Jun.10, portVilaF3Price_stats$Jun.10,
                              portVilaF4Weights_stats$Jun.10, portVilaF5Weights_stats$Jun.10, portVilaF6Wgts_new$Jun.10)
names(colQtr_Jun10) <- c("Jun.10F1", "Jun.10F2", "Jun.10F3", "Jun.10F4", "Jun.10F5", "Jun.10F6")
                              
colQtr_Sep10 <- data.frame(portVilaF1Weights_stats$Sep.10, portVilaF2Weights_stats$Sep.10, portVilaF3Price_stats$Sep.10,
                              portVilaF4Weights_stats$Sep.10, portVilaF5Weights_stats$Sep.10, portVilaF6Wgts_new$Sep.10)
names(colQtr_Sep10) <- c("Sep.10F1", "Sep.10F2", "Sep.10F3", "Sep.10F4", "Sep.10F5", "Sep.10F6")


colQtr_Dec10 <- data.frame(portVilaF1Weights_stats$Dec.10, portVilaF2Weights_stats$Dec.10, portVilaF3Price_stats$Dec.10,
                           portVilaF4Weights_stats$Dec.10, portVilaF5Weights_stats$Dec.10, portVilaF6Wgts_new$Dec.10)
names(colQtr_Dec10) <- c("Dec.10F1", "Dec.10F2", "Dec.10F3", "Dec.10F4", "Dec.10F5", "Dec.10F6")                              

colQtr_Mar11 <- data.frame (portVilaF1Weights_stats$Mar.11, portVilaF2Weights_stats$Mar.11, portVilaF3Price_stats$Mar.11,
                            portVilaF4Weights_stats$Mar.11, portVilaF5Weights_stats$Mar.11, portVilaF6Wgts_new$Mar.11) 
names(colQtr_Mar11) <- c ( "Mar.11F1", "Mar.11F2", "Mar.11F3", "Mar.11F4", "Mar.11F5", "Mar.11F6" )                             

colQtr_Jun11 <- data.frame (portVilaF1Weights_stats$Jun.11, portVilaF2Weights_stats$Jun.11, portVilaF3Price_stats$Jun.11,
                            portVilaF4Weights_stats$Jun.11, portVilaF5Weights_stats$Jun.11, portVilaF6Wgts_new$Jun.11)
names(colQtr_Jun11) <- c("Jun.11F1", "Jun.11F2", "Jun.11F3", "Jun.11F4", "Jun.11F5", "Jun.11F6")

colQtr_Sep11 <- data.frame(portVilaF1Weights_stats$Sep.11, portVilaF2Weights_stats$Sep.11, portVilaF3Price_stats$Sep.11,
                           portVilaF4Weights_stats$Sep.11, portVilaF5Weights_stats$Sep.11, portVilaF6Wgts_new$Sep.11)
names(colQtr_Sep11) <- c("Sep.11F1", "Sep.11F2", "Sep.11F3", "Sep.11F4", "Sep.11F5", "Sep.11F6")

colQtr_Dec11 <- data.frame(portVilaF1Weights_stats$Dec.11, portVilaF2Weights_stats$Dec.11, portVilaF3Price_stats$Dec.11,
                           portVilaF4Weights_stats$Dec.11, portVilaF5Weights_stats$Dec.11, portVilaF6Wgts_new$Dec.11)
names(colQtr_Dec11) <- c("Dec.11F1", "Dec.11F2", "Dec.11F3", "Dec.11F4", "Dec.11F5", "Dec.11F6")                              

colQtr_Mar12 <- data.frame (portVilaF1Weights_stats$Mar.12, portVilaF2Weights_stats$Mar.12, portVilaF3Price_stats$Mar.12,
                            portVilaF4Weights_stats$Mar.12, portVilaF5Weights_stats$Mar.12, portVilaF6Wgts_new$Mar.12) 
names(colQtr_Mar11) <- c ( "Mar.11F1", "Mar.11F2", "Mar.11F3", "Mar.11F4", "Mar.11F5", "Mar.11F6" )                             

colQtr_Jun11 <- data.frame (portVilaF1Weights_stats$Jun.11, portVilaF2Weights_stats$Jun.11, portVilaF3Price_stats$Jun.11,
                            portVilaF4Weights_stats$Jun.11, portVilaF5Weights_stats$Jun.11, portVilaF6Wgts_new$Jun.11)
names(colQtr_Jun11) <- c("Jun.11F1", "Jun.11F2", "Jun.11F3", "Jun.11F4", "Jun.11F5", "Jun.11F6")

colQtr_Sep10 <- data.frame(portVilaF1Weights_stats$Sep.10, portVilaF2Weights_stats$Sep.10, portVilaF3Price_stats$Sep.10,
                           portVilaF4Weights_stats$Sep.10, portVilaF5Weights_stats$Sep.10, portVilaF6Wgts_new$Sep.10)
names(colQtr_Sep10) <- c("Sep.10F1", "Sep.10F2", "Sep.10F3", "Sep.10F4", "Sep.10F5", "Sep.10F6")







####Aggregate of Weights over the 4 quarters####





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


#### Price Quality Check: Check that the none of the prices are 20% higher or lower than the previous pices collected and idetify them####

currentPriceColumn <- 
previousPriceColumn <- cpi_stats_with_estimates
maxPriceRange <- CpiStatsNoDup$Jun.20 + (0.2 * CpiStatsNoDup$Jun.20)



CpiStatsNoDup$MaxValue <- CpiStatsNoDup %>%
  mutate(max_values = Jun.20 +(0.2 * CpiStatsNoDup$Jun.20)) %>%
  mutate(min_values = Jun.20 - (0.2 * CpiStatsNoDup$Jun.20)) 

#This creates the values of max and min of the range of threshold.
#But R, does not recognise it as new columns to the dataframe due to the function being used "mutate"

#Method of creating the variable of max_values for the June.20 quarter, to add this as a new column in the new Data sets.
#abnormalPriceMovement_df 


#1. Create the variable of max and min with the calculations in it.  
max_values <- CpiStatsNoDup$Jun.20 + (0.2 * CpiStatsNoDup$Jun.20)
head (max_values, n=4)

min_values <- CpiStatsNoDup$Jun.20 - (0.2 * CpiStatsNoDup$Jun.20)
head(min_values, n=4)

#Trying to add these new variable as a new columns.
CpiStatsNoDup[max_values] <- max_values
head(CpiStatsNoDup, n=4)

CpiStatsNoDup[min_values] <- min_values
head(CpiStatsNoDup, n=4)


##Practic Run on Abnormal Movements##
#Creating two extra column for the max and min threshold for the 20 percent range

CpiStatsNoDup$MaxValue <- CpiStatsNoDup %>%
    mutate(max_values = Jun.20 +(0.2 * CpiStatsNoDup$Jun.20)) %>%
  mutate(min_values = Jun.20 - (0.2 * CpiStatsNoDup$Jun.20)) 
  
#This creates the values of max and min of the range of threshold.
#But R, does not recognise it as new columns to the dataframe due to the function being used "mutate"

#Method of creating the variable of max_values for the June.20 quarter, to add this as a new column in the new Data sets.
#abnormalPriceMovement_df 


#1. Create the variable of max and min with the calculations in it.  
max_values <- CpiStatsNoDup$Jun.20 + (0.2 * CpiStatsNoDup$Jun.20)
head (max_values, n=4)

min_values <- CpiStatsNoDup$Jun.20 - (0.2 * CpiStatsNoDup$Jun.20)
head(min_values, n=4)

#Trying to add these new variable as a new columns.
CpiStatsNoDup[max_values] <- max_values
head(CpiStatsNoDup, n=4)

CpiStatsNoDup[min_values] <- min_values
head(CpiStatsNoDup, n=4)






















