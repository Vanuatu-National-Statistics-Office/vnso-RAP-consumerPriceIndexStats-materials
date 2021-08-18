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
names(colQtr_Mar12) <- c ( "Mar.12F1", "Mar.12F2", "Mar.12F3", "Mar.12F4", "Mar.12F5", "Mar.12F6" )                             

colQtr_Jun12 <- data.frame (portVilaF1Weights_stats$Jun.12, portVilaF2Weights_stats$Jun.12, portVilaF3Price_stats$Jun.12,
                            portVilaF4Weights_stats$Jun.12, portVilaF5Weights_stats$Jun.12, portVilaF6Wgts_new$Jun.12)
names(colQtr_Jun12) <- c("Jun.12F1", "Jun.12F2", "Jun.12F3", "Jun.12F4", "Jun.12F5", "Jun.12F6")

colQtr_Sep12 <- data.frame(portVilaF1Weights_stats$Sep.12, portVilaF2Weights_stats$Sep.12, portVilaF3Price_stats$Sep.12,
                           portVilaF4Weights_stats$Sep.12, portVilaF5Weights_stats$Sep.12, portVilaF6Wgts_new$Sep.12)
names(colQtr_Sep12) <- c("Sep.12F1", "Sep.12F2", "Sep.12F3", "Sep.12F4", "Sep.12F5", "Sep.12F6")

colQtr_Dec12 <- data.frame(portVilaF1Weights_stats$Dec.12, portVilaF2Weights_stats$Dec.12, portVilaF3Price_stats$Dec.12,
                           portVilaF4Weights_stats$Dec.12, portVilaF5Weights_stats$Dec.12, portVilaF6Wgts_new$Dec.12)
names(colQtr_Dec12) <- c("Dec.12F1", "Dec.12F2", "Dec.12F3", "Dec.12F4", "Dec.12F5", "Dec.12F6")                              

colQtr_Mar13 <- data.frame (portVilaF1Weights_stats$Mar.13, portVilaF2Weights_stats$Mar.13, portVilaF3Price_stats$Mar.13,
                            portVilaF4Weights_stats$Mar.13, portVilaF5Weights_stats$Mar.13, portVilaF6Wgts_new$Mar.13) 
names(colQtr_Mar13) <- c ( "Mar.13F1", "Mar.13F2", "Mar.13F3", "Mar.13F4", "Mar.13F5", "Mar.13F6" )                             

colQtr_Jun13 <- data.frame (portVilaF1Weights_stats$Jun.13, portVilaF2Weights_stats$Jun.13, portVilaF3Price_stats$Jun.13,
                            portVilaF4Weights_stats$Jun.13, portVilaF5Weights_stats$Jun.13, portVilaF6Wgts_new$Jun.13)
names(colQtr_Jun13) <- c("Jun.13F1", "Jun.13F2", "Jun.13F3", "Jun.13F4", "Jun.13F5", "Jun.13F6")

colQtr_Sep13 <- data.frame(portVilaF1Weights_stats$Sep.13, portVilaF2Weights_stats$Sep.13, portVilaF3Price_stats$Sep.13,
                           portVilaF4Weights_stats$Sep.13, portVilaF5Weights_stats$Sep.13, portVilaF6Wgts_new$Sep.13)
names(colQtr_Sep13) <- c("Sep.13F1", "Sep.13F2", "Sep.13F3", "Sep.13F4", "Sep.13F5", "Sep.13F6")

colQtr_Dec13 <- data.frame(portVilaF1Weights_stats$Dec.13, portVilaF2Weights_stats$Dec.13, portVilaF3Price_stats$Dec.13,
                           portVilaF4Weights_stats$Dec.13, portVilaF5Weights_stats$Dec.13, portVilaF6Wgts_new$Dec.13)
names(colQtr_Dec13) <- c("Dec.13F1", "Dec.13F2", "Dec.13F3", "Dec.13F4", "Dec.13F5", "Dec.13F6")                              

colQtr_Mar14 <- data.frame (portVilaF1Weights_stats$Mar.14, portVilaF2Weights_stats$Mar.14, portVilaF3Price_stats$Mar.14,
                            portVilaF4Weights_stats$Mar.14, portVilaF5Weights_stats$Mar.14, portVilaF6Wgts_new$Mar.14) 
names(colQtr_Mar14) <- c ( "Mar.14F1", "Mar.14F2", "Mar.14F3", "Mar.14F4", "Mar.14F5", "Mar.14F6" )                             

colQtr_Jun14 <- data.frame (portVilaF1Weights_stats$Jun.14, portVilaF2Weights_stats$Jun.14, portVilaF3Price_stats$Jun.14,
                            portVilaF4Weights_stats$Jun.14, portVilaF5Weights_stats$Jun.14, portVilaF6Wgts_new$Jun.14)
names(colQtr_Jun14) <- c("Jun.14F1", "Jun.14F2", "Jun.14F3", "Jun.14F4", "Jun.14F5", "Jun.14F6")

colQtr_Sep14 <- data.frame(portVilaF1Weights_stats$Sep.14, portVilaF2Weights_stats$Sep.14, portVilaF3Price_stats$Sep.14,
                           portVilaF4Weights_stats$Sep.14, portVilaF5Weights_stats$Sep.14, portVilaF6Wgts_new$Sep.14)
names(colQtr_Sep14) <- c("Sep.14F1", "Sep.14F2", "Sep.14F3", "Sep.14F4", "Sep.14F5", "Sep.14F6")

colQtr_Dec14 <- data.frame(portVilaF1Weights_stats$Dec.14, portVilaF2Weights_stats$Dec.14, portVilaF3Price_stats$Dec.14,
                           portVilaF4Weights_stats$Dec.14, portVilaF5Weights_stats$Dec.14, portVilaF6Wgts_new$Dec.14)
names(colQtr_Dec14) <- c("Dec.14F1", "Dec.14F2", "Dec.14F3", "Dec.14F4", "Dec.14F5", "Dec.14F6")                              

colQtr_Mar15 <- data.frame (portVilaF1Weights_stats$Mar.15, portVilaF2Weights_stats$Mar.15, portVilaF3Price_stats$Mar.15,
                            portVilaF4Weights_stats$Mar.15, portVilaF5Weights_stats$Mar.15, portVilaF6Wgts_new$Mar.15) 
names(colQtr_Mar14) <- c ( "Mar.14F1", "Mar.14F2", "Mar.14F3", "Mar.14F4", "Mar.14F5", "Mar.14F6" )                             

colQtr_Jun15 <- data.frame (portVilaF1Weights_stats$Jun.15, portVilaF2Weights_stats$Jun.15, portVilaF3Price_stats$Jun.15,
                            portVilaF4Weights_stats$Jun.15, portVilaF5Weights_stats$Jun.15, portVilaF6Wgts_new$Jun.15)
names(colQtr_Jun15) <- c("Jun.15F1", "Jun.15F2", "Jun.15F3", "Jun.15F4", "Jun.15F5", "Jun.15F6")

colQtr_Sep15 <- data.frame(portVilaF1Weights_stats$Sep.15, portVilaF2Weights_stats$Sep.15, portVilaF3Price_stats$Sep.15,
                           portVilaF4Weights_stats$Sep.15, portVilaF5Weights_stats$Sep.15, portVilaF6Wgts_new$Sep.15)
names(colQtr_Sep15) <- c("Sep.15F1", "Sep.15F2", "Sep.15F3", "Sep.15F4", "Sep.15F5", "Sep.15F6")

colQtr_Dec15 <- data.frame(portVilaF1Weights_stats$Dec.15, portVilaF2Weights_stats$Dec.15, portVilaF3Price_stats$Dec.15,
                           portVilaF4Weights_stats$Dec.15, portVilaF5Weights_stats$Dec.15, portVilaF6Wgts_new$Dec.15)
names(colQtr_Dec15) <- c("Dec.15F1", "Dec.15F2", "Dec.15F3", "Dec.15F4", "Dec.15F5", "Dec.15F6")                              

colQtr_Mar16 <- data.frame (portVilaF1Weights_stats$Mar.16, portVilaF2Weights_stats$Mar.16, portVilaF3Price_stats$Mar.16,
                            portVilaF4Weights_stats$Mar.16, portVilaF5Weights_stats$Mar.16, portVilaF6Wgts_new$Mar.16) 
names(colQtr_Mar16) <- c ( "Mar.16F1", "Mar.16F2", "Mar.16F3", "Mar.16F4", "Mar.16F5", "Mar.16F6" )                             

colQtr_Jun16 <- data.frame (portVilaF1Weights_stats$Jun.16, portVilaF2Weights_stats$Jun.16, portVilaF3Price_stats$Jun.16,
                            portVilaF4Weights_stats$Jun.16, portVilaF5Weights_stats$Jun.16, portVilaF6Wgts_new$Jun.16)
names(colQtr_Jun15) <- c("Jun.16F1", "Jun.16F2", "Jun.16F3", "Jun.16F4", "Jun.16F5", "Jun.16F6")

colQtr_Sep16 <- data.frame(portVilaF1Weights_stats$Sep.16, portVilaF2Weights_stats$Sep.16, portVilaF3Price_stats$Sep.16,
                           portVilaF4Weights_stats$Sep.16, portVilaF5Weights_stats$Sep.16, portVilaF6Wgts_new$Sep.16)
names(colQtr_Sep16) <- c("Sep.16F1", "Sep.16F2", "Sep.16F3", "Sep.16F4", "Sep.16F5", "Sep.16F6")

colQtr_Dec16 <- data.frame(portVilaF1Weights_stats$Dec.16, portVilaF2Weights_stats$Dec.16, portVilaF3Price_stats$Dec.16,
                           portVilaF4Weights_stats$Dec.16, portVilaF5Weights_stats$Dec.16, portVilaF6Wgts_new$Dec.16)
names(colQtr_Dec16) <- c("Dec.16F1", "Dec.16F2", "Dec.16F3", "Dec.16F4", "Dec.16F5", "Dec.16F6")                              

colQtr_Mar17 <- data.frame (portVilaF1Weights_stats$Mar.17, portVilaF2Weights_stats$Mar.17, portVilaF3Price_stats$Mar.17,
                            portVilaF4Weights_stats$Mar.17, portVilaF5Weights_stats$Mar.17, portVilaF6Wgts_new$Mar.17) 
names(colQtr_Mar17) <- c ( "Mar.17F1", "Mar.17F2", "Mar.17F3", "Mar.17F4", "Mar.17F5", "Mar.17F6" )                             

colQtr_Jun17 <- data.frame (portVilaF1Weights_stats$Jun.17, portVilaF2Weights_stats$Jun.17, portVilaF3Price_stats$Jun.17,
                            portVilaF4Weights_stats$Jun.17, portVilaF5Weights_stats$Jun.17, portVilaF6Wgts_new$Jun.17)
names(colQtr_Jun17) <- c("Jun.17F1", "Jun.17F2", "Jun.17F3", "Jun.17F4", "Jun.17F5", "Jun.17F6")

colQtr_Sep17 <- data.frame(portVilaF1Weights_stats$Sep.17, portVilaF2Weights_stats$Sep.17, portVilaF3Price_stats$Sep.17,
                           portVilaF4Weights_stats$Sep.17, portVilaF5Weights_stats$Sep.17, portVilaF6Wgts_new$Sep.17)
names(colQtr_Sep17) <- c("Sep.17F1", "Sep.17F2", "Sep.17F3", "Sep.17F4", "Sep.17F5", "Sep.17F6")

colQtr_Dec17 <- data.frame(portVilaF1Weights_stats$Dec.17, portVilaF2Weights_stats$Dec.17, portVilaF3Price_stats$Dec.17,
                           portVilaF4Weights_stats$Dec.17, portVilaF5Weights_stats$Dec.17, portVilaF6Wgts_new$Dec.17)
names(colQtr_Dec17) <- c("Dec.17F1", "Dec.17F2", "Dec.17F3", "Dec.17F4", "Dec.17F5", "Dec.17F6")                              

colQtr_Mar18 <- data.frame (portVilaF1Weights_stats$Mar.18, portVilaF2Weights_stats$Mar.18, portVilaF3Price_stats$Mar.18,
                            portVilaF4Weights_stats$Mar.18, portVilaF5Weights_stats$Mar.18, portVilaF6Wgts_new$Mar.18) 
names(colQtr_Mar18) <- c ( "Mar.18F1", "Mar.18F2", "Mar.18F3", "Mar.18F4", "Mar.18F5", "Mar.18F6" )                             

colQtr_Jun18 <- data.frame (portVilaF1Weights_stats$Jun.18, portVilaF2Weights_stats$Jun.18, portVilaF3Price_stats$Jun.18,
                            portVilaF4Weights_stats$Jun.18, portVilaF5Weights_stats$Jun.18, portVilaF6Wgts_new$Jun.18)
names(colQtr_Jun18) <- c("Jun.18F1", "Jun.18F2", "Jun.18F3", "Jun.18F4", "Jun.18F5", "Jun.18F6")

colQtr_Sep18 <- data.frame(portVilaF1Weights_stats$Sep.18, portVilaF2Weights_stats$Sep.18, portVilaF3Price_stats$Sep.18,
                           portVilaF4Weights_stats$Sep.18, portVilaF5Weights_stats$Sep.18, portVilaF6Wgts_new$Sep.18)
names(colQtr_Sep18) <- c("Sep.18F1", "Sep.18F2", "Sep.18F3", "Sep.18F4", "Sep.18F5", "Sep.18F6")

colQtr_Dec18 <- data.frame(portVilaF1Weights_stats$Dec.18, portVilaF2Weights_stats$Dec.18, portVilaF3Price_stats$Dec.18,
                           portVilaF4Weights_stats$Dec.18, portVilaF5Weights_stats$Dec.18, portVilaF6Wgts_new$Dec.18)
names(colQtr_Dec18) <- c("Dec.18F1", "Dec.18F2", "Dec.18F3", "Dec.18F4", "Dec.18F5", "Dec.18F6")                              

colQtr_Mar19 <- data.frame (portVilaF1Weights_stats$Mar.19, portVilaF2Weights_stats$Mar.19, portVilaF3Price_stats$Mar.19,
                            portVilaF4Weights_stats$Mar.19, portVilaF5Weights_stats$Mar.19, portVilaF6Wgts_new$Mar.19) 
names(colQtr_Mar19) <- c ( "Mar.19F1", "Mar.19F2", "Mar.19F3", "Mar.19F4", "Mar.19F5", "Mar.19F6" )                             

colQtr_Jun19 <- data.frame (portVilaF1Weights_stats$Jun.19, portVilaF2Weights_stats$Jun.19, portVilaF3Price_stats$Jun.19,
                            portVilaF4Weights_stats$Jun.19, portVilaF5Weights_stats$Jun.19, portVilaF6Wgts_new$Jun.19)
names(colQtr_Jun19) <- c("Jun.19F1", "Jun.19F2", "Jun.19F3", "Jun.19F4", "Jun.19F5", "Jun.19F6")

colQtr_Sep19 <- data.frame(portVilaF1Weights_stats$Sep.19, portVilaF2Weights_stats$Sep.19, portVilaF3Price_stats$Sep.19,
                           portVilaF4Weights_stats$Sep.19, portVilaF5Weights_stats$Sep.19, portVilaF6Wgts_new$Sep.19)
names(colQtr_Sep19) <- c("Sep.19F1", "Sep.19F2", "Sep.19F3", "Sep.19F4", "Sep.19F5", "Sep.19F6")

colQtr_Dec19 <- data.frame(portVilaF1Weights_stats$Dec.19, portVilaF2Weights_stats$Dec.19, portVilaF3Price_stats$Dec.19,
                           portVilaF4Weights_stats$Dec.19, portVilaF5Weights_stats$Dec.19, portVilaF6Wgts_new$Dec.19)
names(colQtr_Dec19) <- c("Dec.19F1", "Dec.19F2", "Dec.19F3", "Dec.19F4", "Dec.19F5", "Dec.19F6")                              

colQtr_Mar20 <- data.frame (portVilaF1Weights_stats$Mar.20, portVilaF2Weights_stats$Mar.20, portVilaF3Price_stats$Mar.20,
                            portVilaF4Weights_stats$Mar.20, portVilaF5Weights_stats$Mar.20, portVilaF6Wgts_new$Mar.20) 
names(colQtr_Mar20) <- c ( "Mar.20F1", "Mar.20F2", "Mar.20F3", "Mar.20F4", "Mar.20F5", "Mar.20F6" )                             

colQtr_Jun20 <- data.frame (portVilaF1Weights_stats$Jun.20, portVilaF2Weights_stats$Jun.20, portVilaF3Price_stats$Jun.20,
                            portVilaF4Weights_stats$Jun.20, portVilaF5Weights_stats$Jun.20, portVilaF6Wgts_new$Jun.20)
names(colQtr_Jun20) <- c("Jun.20F1", "Jun.20F2", "Jun.20F3", "Jun.20F4", "Jun.20F5", "Jun.20F6")


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






















