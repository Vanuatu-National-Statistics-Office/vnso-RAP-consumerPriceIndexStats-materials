#### Preparation ####

# Clear the environment
rm(list = ls())

# Load the required libraries'
library(tidyverse)
library(dplyr) # Manipulating data
library(stringr)# common string operations
library(RSQLite)#sql accessing data
library(DBI) #R database Interface
library(openxlsx) #read, write and edit Excel
library(readxl)

# Note where VNSO code/data is on current computer
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path), "..", "..")
setwd(repository) # Required for file.choose() function

# Note the secure data path
secureDataFolder <- file.path(repository, "data", "secure")

# Note the open data path
openDataFolder <- file.path(repository, "data", "open")

#### Stage 1: Fortnight Prices and Weights####

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

# Read in the file for weights

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

# Convert the raw data to numeric #

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
names(cpi_stats) [1] <- "ItemId"  #Rename the first column to become ItemId, for the purpose of consistency
names(cpi_stats)

# Convert columns to numeric

column_name_combinations <- expand.grid(c("Mar", "Jun", "Sep", "Dec"), c("08", "09", 10:21))
quarterly_columns <- paste(column_name_combinations[, 1], column_name_combinations[, 2], sep = ".")
quarterly_columns <- quarterly_columns[quarterly_columns %in% colnames(cpi_stats)]
cpi_stats[quarterly_columns] <- sapply(cpi_stats[quarterly_columns], FUN = as.numeric)

# Remove duplicated rows from the CPI data#

cpi_stats <- cpi_stats[duplicated(cpi_stats) == FALSE, ]
portVilaF1Price_stats <- portVilaF1Price_stats[duplicated(portVilaF1Price_stats) == FALSE, ]
portVilaF2Price_stats <- portVilaF2Price_stats[duplicated(portVilaF2Price_stats) == FALSE, ]
portVilaF3Price_stats <- portVilaF3Price_stats[duplicated(portVilaF3Price_stats) == FALSE, ]
portVilaF4Price_stats <- portVilaF4Price_stats[duplicated(portVilaF4Price_stats) == FALSE, ]
portVilaF5Price_stats <- portVilaF5Price_stats[duplicated(portVilaF5Price_stats) == FALSE, ]
portVilaF6Price_stats <- portVilaF6Price_stats[duplicated(portVilaF6Price_stats) == FALSE, ]

# Restructing the datasets for the weights fortnight #

# Remove the last row of this dataframe, so that it can have the same size. The last row was an NA.
portVilaF6Wgts_new <- head(portVilaF6Weights_stats, - 1)              

colQtrWgts_name <- data.frame(portVilaF2Price_stats$ItemId, portVilaF2Price_stats$Eas, portVilaF2Price_stats$Specification,portVilaF2Price_stats$LocalDescription, portVilaF2Price_stats$Outlet, 
                              portVilaF2Price_stats$Address, portVilaF2Price_stats$CollDay, portVilaF2Price_stats$Active)
names(colQtrWgts_name) <- c("ItemId", "Eas", "Specification", "Local Description", "Outlet", "Address", "Col Day", "Active")

colQtr_Dec08 <- data.frame(portVilaF1Weights_stats$Dec.08, portVilaF2Weights_stats$Dec.08, portVilaF3Weights_stats$Dec.08,
                              portVilaF4Weights_stats$Dec.08, portVilaF5Weights_stats$Dec.08, portVilaF6Wgts_new$Dec.08)
names(colQtr_Dec08) <- c("Dec.08F1", "Dec.08F2", "Dec.08F3", "Dec.08F4", "Dec.08F5", "Dec.08F6")

ColQtr_Mar09 <- data.frame(portVilaF1Weights_stats$Mar.09, portVilaF2Weights_stats$Mar.09, portVilaF3Weights_stats$Mar.09,
                              portVilaF4Weights_stats$Mar.09, portVilaF5Weights_stats$Mar.09, portVilaF6Wgts_new$Mar.09)
names(ColQtr_Mar09) <- c("Mar.09F1", "Mar.09F2", "Mar.09F3", "Mar.09F4", "Mar.09F5", "Mar.09F6")

colQtr_Jun09 <- data.frame(portVilaF1Weights_stats$Jun.09, portVilaF2Weights_stats$Jun.09, portVilaF3Weights_stats$Jun.09,
                              portVilaF4Weights_stats$Jun.09, portVilaF5Weights_stats$Jun.09, portVilaF6Wgts_new$Jun.09)
names(colQtr_Jun09) <- c("Jun.09F1", "Jun.09F2", "Jun.09F3", "Jun.09F4", "Jun.09F5", "Jun.09F6")                              
  
colQtr_Sep09 <- data.frame(portVilaF1Weights_stats$Sep.09, portVilaF2Weights_stats$Sep.09, portVilaF3Weights_stats$Sep.09,
                              portVilaF4Weights_stats$Sep.09, portVilaF5Weights_stats$Sep.09, portVilaF6Wgts_new$Sep.09)
names(colQtr_Sep09) <- c("Sep.09F1", "Sep.09F2", "Sep.09F3", "Sep.09F4", "Sep.09F5", "Sep.09F6")                           

colQtr_Dec09 <- data.frame(portVilaF1Weights_stats$Dec.09, portVilaF2Weights_stats$Dec.09, portVilaF3Weights_stats$Dec.09,
                              portVilaF4Weights_stats$Dec.09, portVilaF5Weights_stats$Dec.09, portVilaF6Wgts_new$Dec.09)
names(colQtr_Dec09) <- c("Dec.09F1", "Dec.09F2", "Dec.09F3", "Dec.09F4", "Dec.09F5", "Dec.09F6")                              

colQtr_Mar10 <- data.frame (portVilaF1Weights_stats$Mar.10, portVilaF2Weights_stats$Mar.10, portVilaF3Weights_stats$Mar.10,
                              portVilaF4Weights_stats$Mar.10, portVilaF5Weights_stats$Mar.10, portVilaF6Wgts_new$Mar.10) 
names(colQtr_Mar10) <- c ( "Mar.10F1", "Mar.10F2", "Mar.10F3", "Mar.10F4", "Mar.10F5", "Mar.10F6" )                             
                              
colQtr_Jun10 <- data.frame (portVilaF1Weights_stats$Jun.10, portVilaF2Weights_stats$Jun.10, portVilaF3Weights_stats$Jun.10,
                              portVilaF4Weights_stats$Jun.10, portVilaF5Weights_stats$Jun.10, portVilaF6Wgts_new$Jun.10)
names(colQtr_Jun10) <- c("Jun.10F1", "Jun.10F2", "Jun.10F3", "Jun.10F4", "Jun.10F5", "Jun.10F6")
                              
colQtr_Sep10 <- data.frame(portVilaF1Weights_stats$Sep.10, portVilaF2Weights_stats$Sep.10, portVilaF3Weights_stats$Sep.10,
                              portVilaF4Weights_stats$Sep.10, portVilaF5Weights_stats$Sep.10, portVilaF6Wgts_new$Sep.10)
names(colQtr_Sep10) <- c("Sep.10F1", "Sep.10F2", "Sep.10F3", "Sep.10F4", "Sep.10F5", "Sep.10F6")

colQtr_Dec10 <- data.frame(portVilaF1Weights_stats$Dec.10, portVilaF2Weights_stats$Dec.10, portVilaF3Weights_stats$Dec.10,
                           portVilaF4Weights_stats$Dec.10, portVilaF5Weights_stats$Dec.10, portVilaF6Wgts_new$Dec.10)
names(colQtr_Dec10) <- c("Dec.10F1", "Dec.10F2", "Dec.10F3", "Dec.10F4", "Dec.10F5", "Dec.10F6")                              

colQtr_Mar11 <- data.frame (portVilaF1Weights_stats$Mar.11, portVilaF2Weights_stats$Mar.11, portVilaF3Weights_stats$Mar.11,
                            portVilaF4Weights_stats$Mar.11, portVilaF5Weights_stats$Mar.11, portVilaF6Wgts_new$Mar.11) 
names(colQtr_Mar11) <- c ( "Mar.11F1", "Mar.11F2", "Mar.11F3", "Mar.11F4", "Mar.11F5", "Mar.11F6" )                             

colQtr_Jun11 <- data.frame (portVilaF1Weights_stats$Jun.11, portVilaF2Weights_stats$Jun.11, portVilaF3Weights_stats$Jun.11,
                            portVilaF4Weights_stats$Jun.11, portVilaF5Weights_stats$Jun.11, portVilaF6Wgts_new$Jun.11)
names(colQtr_Jun11) <- c("Jun.11F1", "Jun.11F2", "Jun.11F3", "Jun.11F4", "Jun.11F5", "Jun.11F6")

colQtr_Sep11 <- data.frame(portVilaF1Weights_stats$Sep.11, portVilaF2Weights_stats$Sep.11, portVilaF3Weights_stats$Sep.11,
                           portVilaF4Weights_stats$Sep.11, portVilaF5Weights_stats$Sep.11, portVilaF6Wgts_new$Sep.11)
names(colQtr_Sep11) <- c("Sep.11F1", "Sep.11F2", "Sep.11F3", "Sep.11F4", "Sep.11F5", "Sep.11F6")

colQtr_Dec11 <- data.frame(portVilaF1Weights_stats$Dec.11, portVilaF2Weights_stats$Dec.11, portVilaF3Weights_stats$Dec.11,
                           portVilaF4Weights_stats$Dec.11, portVilaF5Weights_stats$Dec.11, portVilaF6Wgts_new$Dec.11)
names(colQtr_Dec11) <- c("Dec.11F1", "Dec.11F2", "Dec.11F3", "Dec.11F4", "Dec.11F5", "Dec.11F6")                              

colQtr_Mar12 <- data.frame (portVilaF1Weights_stats$Mar.12, portVilaF2Weights_stats$Mar.12, portVilaF3Weights_stats$Mar.12,
                            portVilaF4Weights_stats$Mar.12, portVilaF5Weights_stats$Mar.12, portVilaF6Wgts_new$Mar.12) 
names(colQtr_Mar12) <- c ( "Mar.12F1", "Mar.12F2", "Mar.12F3", "Mar.12F4", "Mar.12F5", "Mar.12F6" )                             

colQtr_Jun12 <- data.frame (portVilaF1Weights_stats$Jun.12, portVilaF2Weights_stats$Jun.12, portVilaF3Weights_stats$Jun.12,
                            portVilaF4Weights_stats$Jun.12, portVilaF5Weights_stats$Jun.12, portVilaF6Wgts_new$Jun.12)
names(colQtr_Jun12) <- c("Jun.12F1", "Jun.12F2", "Jun.12F3", "Jun.12F4", "Jun.12F5", "Jun.12F6")

colQtr_Sep12 <- data.frame(portVilaF1Weights_stats$Sep.12, portVilaF2Weights_stats$Sep.12, portVilaF3Weights_stats$Sep.12,
                           portVilaF4Weights_stats$Sep.12, portVilaF5Weights_stats$Sep.12, portVilaF6Wgts_new$Sep.12)
names(colQtr_Sep12) <- c("Sep.12F1", "Sep.12F2", "Sep.12F3", "Sep.12F4", "Sep.12F5", "Sep.12F6")

colQtr_Dec12 <- data.frame(portVilaF1Weights_stats$Dec.12, portVilaF2Weights_stats$Dec.12, portVilaF3Weights_stats$Dec.12,
                           portVilaF4Weights_stats$Dec.12, portVilaF5Weights_stats$Dec.12, portVilaF6Wgts_new$Dec.12)
names(colQtr_Dec12) <- c("Dec.12F1", "Dec.12F2", "Dec.12F3", "Dec.12F4", "Dec.12F5", "Dec.12F6")                              

colQtr_Mar13 <- data.frame (portVilaF1Weights_stats$Mar.13, portVilaF2Weights_stats$Mar.13, portVilaF3Weights_stats$Mar.13,
                            portVilaF4Weights_stats$Mar.13, portVilaF5Weights_stats$Mar.13, portVilaF6Wgts_new$Mar.13) 
names(colQtr_Mar13) <- c ( "Mar.13F1", "Mar.13F2", "Mar.13F3", "Mar.13F4", "Mar.13F5", "Mar.13F6" )                             

colQtr_Jun13 <- data.frame (portVilaF1Weights_stats$Jun.13, portVilaF2Weights_stats$Jun.13, portVilaF3Weights_stats$Jun.13,
                            portVilaF4Weights_stats$Jun.13, portVilaF5Weights_stats$Jun.13, portVilaF6Wgts_new$Jun.13)
names(colQtr_Jun13) <- c("Jun.13F1", "Jun.13F2", "Jun.13F3", "Jun.13F4", "Jun.13F5", "Jun.13F6")

colQtr_Sep13 <- data.frame(portVilaF1Weights_stats$Sep.13, portVilaF2Weights_stats$Sep.13, portVilaF3Weights_stats$Sep.13,
                           portVilaF4Weights_stats$Sep.13, portVilaF5Weights_stats$Sep.13, portVilaF6Wgts_new$Sep.13)
names(colQtr_Sep13) <- c("Sep.13F1", "Sep.13F2", "Sep.13F3", "Sep.13F4", "Sep.13F5", "Sep.13F6")

colQtr_Dec13 <- data.frame(portVilaF1Weights_stats$Dec.13, portVilaF2Weights_stats$Dec.13, portVilaF3Weights_stats$Dec.13,
                           portVilaF4Weights_stats$Dec.13, portVilaF5Weights_stats$Dec.13, portVilaF6Wgts_new$Dec.13)
names(colQtr_Dec13) <- c("Dec.13F1", "Dec.13F2", "Dec.13F3", "Dec.13F4", "Dec.13F5", "Dec.13F6")                              

colQtr_Mar14 <- data.frame (portVilaF1Weights_stats$Mar.14, portVilaF2Weights_stats$Mar.14, portVilaF3Weights_stats$Mar.14,
                            portVilaF4Weights_stats$Mar.14, portVilaF5Weights_stats$Mar.14, portVilaF6Wgts_new$Mar.14) 
names(colQtr_Mar14) <- c ( "Mar.14F1", "Mar.14F2", "Mar.14F3", "Mar.14F4", "Mar.14F5", "Mar.14F6" )                             

colQtr_Jun14 <- data.frame (portVilaF1Weights_stats$Jun.14, portVilaF2Weights_stats$Jun.14, portVilaF3Weights_stats$Jun.14,
                            portVilaF4Weights_stats$Jun.14, portVilaF5Weights_stats$Jun.14, portVilaF6Wgts_new$Jun.14)
names(colQtr_Jun14) <- c("Jun.14F1", "Jun.14F2", "Jun.14F3", "Jun.14F4", "Jun.14F5", "Jun.14F6")

colQtr_Sep14 <- data.frame(portVilaF1Weights_stats$Sep.14, portVilaF2Weights_stats$Sep.14, portVilaF3Weights_stats$Sep.14,
                           portVilaF4Weights_stats$Sep.14, portVilaF5Weights_stats$Sep.14, portVilaF6Wgts_new$Sep.14)
names(colQtr_Sep14) <- c("Sep.14F1", "Sep.14F2", "Sep.14F3", "Sep.14F4", "Sep.14F5", "Sep.14F6")

colQtr_Dec14 <- data.frame(portVilaF1Weights_stats$Dec.14, portVilaF2Weights_stats$Dec.14, portVilaF3Weights_stats$Dec.14,
                           portVilaF4Weights_stats$Dec.14, portVilaF5Weights_stats$Dec.14, portVilaF6Wgts_new$Dec.14)
names(colQtr_Dec14) <- c("Dec.14F1", "Dec.14F2", "Dec.14F3", "Dec.14F4", "Dec.14F5", "Dec.14F6")                              

colQtr_Mar15 <- data.frame (portVilaF1Weights_stats$Mar.15, portVilaF2Weights_stats$Mar.15, portVilaF3Weights_stats$Mar.15,
                            portVilaF4Weights_stats$Mar.15, portVilaF5Weights_stats$Mar.15, portVilaF6Wgts_new$Mar.15) 
names(colQtr_Mar14) <- c ( "Mar.14F1", "Mar.14F2", "Mar.14F3", "Mar.14F4", "Mar.14F5", "Mar.14F6" )                             

colQtr_Jun15 <- data.frame (portVilaF1Weights_stats$Jun.15, portVilaF2Weights_stats$Jun.15, portVilaF3Weights_stats$Jun.15,
                            portVilaF4Weights_stats$Jun.15, portVilaF5Weights_stats$Jun.15, portVilaF6Wgts_new$Jun.15)
names(colQtr_Jun15) <- c("Jun.15F1", "Jun.15F2", "Jun.15F3", "Jun.15F4", "Jun.15F5", "Jun.15F6")

colQtr_Sep15 <- data.frame(portVilaF1Weights_stats$Sep.15, portVilaF2Weights_stats$Sep.15, portVilaF3Weights_stats$Sep.15,
                           portVilaF4Weights_stats$Sep.15, portVilaF5Weights_stats$Sep.15, portVilaF6Wgts_new$Sep.15)
names(colQtr_Sep15) <- c("Sep.15F1", "Sep.15F2", "Sep.15F3", "Sep.15F4", "Sep.15F5", "Sep.15F6")

colQtr_Dec15 <- data.frame(portVilaF1Weights_stats$Dec.15, portVilaF2Weights_stats$Dec.15, portVilaF3Weights_stats$Dec.15,
                           portVilaF4Weights_stats$Dec.15, portVilaF5Weights_stats$Dec.15, portVilaF6Wgts_new$Dec.15)
names(colQtr_Dec15) <- c("Dec.15F1", "Dec.15F2", "Dec.15F3", "Dec.15F4", "Dec.15F5", "Dec.15F6")                              

colQtr_Mar16 <- data.frame (portVilaF1Weights_stats$Mar.16, portVilaF2Weights_stats$Mar.16, portVilaF3Weights_stats$Mar.16,
                            portVilaF4Weights_stats$Mar.16, portVilaF5Weights_stats$Mar.16, portVilaF6Wgts_new$Mar.16) 
names(colQtr_Mar16) <- c ( "Mar.16F1", "Mar.16F2", "Mar.16F3", "Mar.16F4", "Mar.16F5", "Mar.16F6" )                             

colQtr_Jun16 <- data.frame (portVilaF1Weights_stats$Jun.16, portVilaF2Weights_stats$Jun.16, portVilaF3Weights_stats$Jun.16,
                            portVilaF4Weights_stats$Jun.16, portVilaF5Weights_stats$Jun.16, portVilaF6Wgts_new$Jun.16)
names(colQtr_Jun15) <- c("Jun.16F1", "Jun.16F2", "Jun.16F3", "Jun.16F4", "Jun.16F5", "Jun.16F6")

colQtr_Sep16 <- data.frame(portVilaF1Weights_stats$Sep.16, portVilaF2Weights_stats$Sep.16, portVilaF3Weights_stats$Sep.16,
                           portVilaF4Weights_stats$Sep.16, portVilaF5Weights_stats$Sep.16, portVilaF6Wgts_new$Sep.16)
names(colQtr_Sep16) <- c("Sep.16F1", "Sep.16F2", "Sep.16F3", "Sep.16F4", "Sep.16F5", "Sep.16F6")

colQtr_Dec16 <- data.frame(portVilaF1Weights_stats$Dec.16, portVilaF2Weights_stats$Dec.16, portVilaF3Weights_stats$Dec.16,
                           portVilaF4Weights_stats$Dec.16, portVilaF5Weights_stats$Dec.16, portVilaF6Wgts_new$Dec.16)
names(colQtr_Dec16) <- c("Dec.16F1", "Dec.16F2", "Dec.16F3", "Dec.16F4", "Dec.16F5", "Dec.16F6")                              

colQtr_Mar17 <- data.frame (portVilaF1Weights_stats$Mar.17, portVilaF2Weights_stats$Mar.17, portVilaF3Weights_stats$Mar.17,
                            portVilaF4Weights_stats$Mar.17, portVilaF5Weights_stats$Mar.17, portVilaF6Wgts_new$Mar.17) 
names(colQtr_Mar17) <- c ( "Mar.17F1", "Mar.17F2", "Mar.17F3", "Mar.17F4", "Mar.17F5", "Mar.17F6" )                             

colQtr_Jun17 <- data.frame (portVilaF1Weights_stats$Jun.17, portVilaF2Weights_stats$Jun.17, portVilaF3Weights_stats$Jun.17,
                            portVilaF4Weights_stats$Jun.17, portVilaF5Weights_stats$Jun.17, portVilaF6Wgts_new$Jun.17)
names(colQtr_Jun17) <- c("Jun.17F1", "Jun.17F2", "Jun.17F3", "Jun.17F4", "Jun.17F5", "Jun.17F6")

colQtr_Sep17 <- data.frame(portVilaF1Weights_stats$Sep.17, portVilaF2Weights_stats$Sep.17, portVilaF3Weights_stats$Sep.17,
                           portVilaF4Weights_stats$Sep.17, portVilaF5Weights_stats$Sep.17, portVilaF6Wgts_new$Sep.17)
names(colQtr_Sep17) <- c("Sep.17F1", "Sep.17F2", "Sep.17F3", "Sep.17F4", "Sep.17F5", "Sep.17F6")

colQtr_Dec17 <- data.frame(portVilaF1Weights_stats$Dec.17, portVilaF2Weights_stats$Dec.17, portVilaF3Weights_stats$Dec.17,
                           portVilaF4Weights_stats$Dec.17, portVilaF5Weights_stats$Dec.17, portVilaF6Wgts_new$Dec.17)
names(colQtr_Dec17) <- c("Dec.17F1", "Dec.17F2", "Dec.17F3", "Dec.17F4", "Dec.17F5", "Dec.17F6")                              

colQtr_Mar18 <- data.frame (portVilaF1Weights_stats$Mar.18, portVilaF2Weights_stats$Mar.18, portVilaF3Weights_stats$Mar.18,
                            portVilaF4Weights_stats$Mar.18, portVilaF5Weights_stats$Mar.18, portVilaF6Wgts_new$Mar.18) 
names(colQtr_Mar18) <- c ( "Mar.18F1", "Mar.18F2", "Mar.18F3", "Mar.18F4", "Mar.18F5", "Mar.18F6" )                             

colQtr_Jun18 <- data.frame (portVilaF1Weights_stats$Jun.18, portVilaF2Weights_stats$Jun.18, portVilaF3Weights_stats$Jun.18,
                            portVilaF4Weights_stats$Jun.18, portVilaF5Weights_stats$Jun.18, portVilaF6Wgts_new$Jun.18)
names(colQtr_Jun18) <- c("Jun.18F1", "Jun.18F2", "Jun.18F3", "Jun.18F4", "Jun.18F5", "Jun.18F6")

colQtr_Sep18 <- data.frame(portVilaF1Weights_stats$Sep.18, portVilaF2Weights_stats$Sep.18, portVilaF3Weights_stats$Sep.18,
                           portVilaF4Weights_stats$Sep.18, portVilaF5Weights_stats$Sep.18, portVilaF6Wgts_new$Sep.18)
names(colQtr_Sep18) <- c("Sep.18F1", "Sep.18F2", "Sep.18F3", "Sep.18F4", "Sep.18F5", "Sep.18F6")

colQtr_Dec18 <- data.frame(portVilaF1Weights_stats$Dec.18, portVilaF2Weights_stats$Dec.18, portVilaF3Weights_stats$Dec.18,
                           portVilaF4Weights_stats$Dec.18, portVilaF5Weights_stats$Dec.18, portVilaF6Wgts_new$Dec.18)
names(colQtr_Dec18) <- c("Dec.18F1", "Dec.18F2", "Dec.18F3", "Dec.18F4", "Dec.18F5", "Dec.18F6")                              

colQtr_Mar19 <- data.frame (portVilaF1Weights_stats$Mar.19, portVilaF2Weights_stats$Mar.19, portVilaF3Weights_stats$Mar.19,
                            portVilaF4Weights_stats$Mar.19, portVilaF5Weights_stats$Mar.19, portVilaF6Wgts_new$Mar.19) 
names(colQtr_Mar19) <- c ( "Mar.19F1", "Mar.19F2", "Mar.19F3", "Mar.19F4", "Mar.19F5", "Mar.19F6" )                             

colQtr_Jun19 <- data.frame (portVilaF1Weights_stats$Jun.19, portVilaF2Weights_stats$Jun.19, portVilaF3Weights_stats$Jun.19,
                            portVilaF4Weights_stats$Jun.19, portVilaF5Weights_stats$Jun.19, portVilaF6Wgts_new$Jun.19)
names(colQtr_Jun19) <- c("Jun.19F1", "Jun.19F2", "Jun.19F3", "Jun.19F4", "Jun.19F5", "Jun.19F6")

colQtr_Sep19 <- data.frame(portVilaF1Weights_stats$Sep.19, portVilaF2Weights_stats$Sep.19, portVilaF3Price_stats$Sep.19,
                           portVilaF4Weights_stats$Sep.19, portVilaF5Weights_stats$Sep.19, portVilaF6Wgts_new$Sep.19)
names(colQtr_Sep19) <- c("Sep.19F1", "Sep.19F2", "Sep.19F3", "Sep.19F4", "Sep.19F5", "Sep.19F6")

colQtr_Dec19 <- data.frame(portVilaF1Weights_stats$Dec.19, portVilaF2Weights_stats$Dec.19, portVilaF3Weights_stats$Dec.19,
                           portVilaF4Weights_stats$Dec.19, portVilaF5Weights_stats$Dec.19, portVilaF6Wgts_new$Dec.19)
names(colQtr_Dec19) <- c("Dec.19F1", "Dec.19F2", "Dec.19F3", "Dec.19F4", "Dec.19F5", "Dec.19F6")                              

colQtr_Mar20 <- data.frame (portVilaF1Weights_stats$Mar.20, portVilaF2Weights_stats$Mar.20, portVilaF3Weights_stats$Mar.20,
                            portVilaF4Weights_stats$Mar.20, portVilaF5Weights_stats$Mar.20, portVilaF6Wgts_new$Mar.20) 
names(colQtr_Mar20) <- c ( "Mar.20F1", "Mar.20F2", "Mar.20F3", "Mar.20F4", "Mar.20F5", "Mar.20F6" )                             

colQtr_Jun20 <- data.frame (portVilaF1Weights_stats$Jun.20, portVilaF2Weights_stats$Jun.20, portVilaF3Weights_stats$Jun.20,
                            portVilaF4Weights_stats$Jun.20, portVilaF5Weights_stats$Jun.20, portVilaF6Wgts_new$Jun.20)
names(colQtr_Jun20) <- c("Jun.20F1", "Jun.20F2", "Jun.20F3", "Jun.20F4", "Jun.20F5", "Jun.20F6")


# Aggregate of Weights for Port Vila over the 4 quarters #

colQtr_Dec08$Dec08Q4 = rowSums(colQtr_Dec08, na.rm = TRUE, dims = 1L)

ColQtr_Mar09$Mar09Q1 = rowSums(ColQtr_Mar09, na.rm = TRUE, dims = 1L)
colQtr_Jun09$Jun09Q2 = rowSums(colQtr_Jun09, na.rm = TRUE, dims = 1L)
colQtr_Sep09$Sep09Q3 = rowSums(colQtr_Sep09, na.rm = TRUE, dims = 1L)
colQtr_Dec09$Dec09Q4 = rowSums(colQtr_Dec09, na.rm = TRUE, dims = 1L)

colQtr_Mar10$Mar10Q1 = rowSums(colQtr_Mar10, na.rm = TRUE, dims = 1L)
colQtr_Jun10$Jun10Q2 = rowSums(colQtr_Jun10, na.rm = TRUE, dims = 1L)
colQtr_Sep10$Sep10Q3 = rowSums(colQtr_Sep10, na.rm = TRUE, dims = 1L)
colQtr_Dec10$Dec10Q4 = rowSums(colQtr_Dec10, na.rm = TRUE, dims = 1L)

colQtr_Mar11$Mar11Q1 = rowSums(colQtr_Mar11, na.rm = TRUE, dims = 1L)
colQtr_Jun11$Jun11Q2 = rowSums(colQtr_Jun11, na.rm = TRUE, dims = 1L)
colQtr_Sep11$Sep11Q3 = rowSums(colQtr_Sep11, na.rm = TRUE, dims = 1L)
colQtr_Dec11$Dec11Q4 = rowSums(colQtr_Dec11, na.rm = TRUE, dims = 1L)

colQtr_Mar12$Mar12Q1 = rowSums(colQtr_Mar12, na.rm = TRUE, dims = 1L)
colQtr_Jun12$Jun12Q2 = rowSums(colQtr_Jun12, na.rm = TRUE, dims = 1L)
colQtr_Sep12$Sep12Q3 = rowSums(colQtr_Sep12, na.rm = TRUE, dims = 1L)
colQtr_Dec12$Dec12Q4 = rowSums(colQtr_Dec12, na.rm = TRUE, dims = 1L)

colQtr_Mar13$Mar13Q1 = rowSums(colQtr_Mar13, na.rm = TRUE, dims = 1L)
colQtr_Jun13$Jun13Q2 = rowSums(colQtr_Jun13, na.rm = TRUE, dims = 1L)
colQtr_Sep13$Sep13Q3 = rowSums(colQtr_Sep13, na.rm = TRUE, dims = 1L)
colQtr_Dec13$Dec13Q4 = rowSums(colQtr_Dec13, na.rm = TRUE, dims = 1L)

colQtr_Mar14$Mar14Q1 = rowSums(colQtr_Mar14, na.rm = TRUE, dims = 1L)
colQtr_Jun14$Jun14Q2 = rowSums(colQtr_Jun14, na.rm = TRUE, dims = 1L)
colQtr_Sep14$Sep14Q3 = rowSums(colQtr_Sep14, na.rm = TRUE, dims = 1L)
colQtr_Dec14$Dec14Q4 = rowSums(colQtr_Dec14, na.rm = TRUE, dims = 1L)

colQtr_Mar15$Mar15Q1 = rowSums(colQtr_Mar15, na.rm = TRUE, dims = 1L)
colQtr_Jun15$Jun15Q2 = rowSums(colQtr_Jun15, na.rm = TRUE, dims = 1L)
colQtr_Sep15$Sep15Q3 = rowSums(colQtr_Sep15, na.rm = TRUE, dims = 1L)
colQtr_Dec15$Dec15Q4 = rowSums(colQtr_Dec15, na.rm = TRUE, dims = 1L)

colQtr_Mar16$Mar16Q1 = rowSums(colQtr_Mar16, na.rm = TRUE, dims = 1L)
colQtr_Jun16$Jun16Q2 = rowSums(colQtr_Jun16, na.rm = TRUE, dims = 1L)
colQtr_Sep16$Sep16Q3 = rowSums(colQtr_Sep16, na.rm = TRUE, dims = 1L)
colQtr_Dec16$Dec16Q4 = rowSums(colQtr_Dec16, na.rm = TRUE, dims = 1L)

colQtr_Mar17$Mar17Q1 = rowSums(colQtr_Mar17, na.rm = TRUE, dims = 1L)
colQtr_Jun17$Jun17Q2 = rowSums(colQtr_Jun17, na.rm = TRUE, dims = 1L)
colQtr_Sep17$Sep17Q3 = rowSums(colQtr_Sep17, na.rm = TRUE, dims = 1L)
colQtr_Dec17$Dec17Q4 = rowSums(colQtr_Dec17, na.rm = TRUE, dims = 1L)

colQtr_Mar18$Mar18Q1 = rowSums(colQtr_Mar18, na.rm = TRUE, dims = 1L)
colQtr_Jun18$Jun18Q2 = rowSums(colQtr_Jun18, na.rm = TRUE, dims = 1L)
colQtr_Sep18$Sep18Q3 = rowSums(colQtr_Sep18, na.rm = TRUE, dims = 1L)
colQtr_Dec18$Dec18Q4 = rowSums(colQtr_Dec18, na.rm = TRUE, dims = 1L)

colQtr_Mar19$Mar19Q1 = rowSums(colQtr_Mar19, na.rm = TRUE, dims = 1L)
colQtr_Jun19$Jun19Q2 = rowSums(colQtr_Jun19, na.rm = TRUE, dims = 1L)
colQtr_Sep19$Sep19Q3 = rowSums(colQtr_Sep19, na.rm = TRUE, dims = 1L)
colQtr_Dec19$Dec19Q4 = rowSums(colQtr_Dec19, na.rm = TRUE, dims = 1L)

colQtr_Mar20$Mar20Q1 = rowSums(colQtr_Mar20, na.rm = TRUE, dims = 1L)
colQtr_Jun20$Jun20Q2 = rowSums(colQtr_Jun20, na.rm = TRUE, dims = 1L)

# Create the new dataset with constant column for the Fortnights weight#

Dec08Q4Wgts <- cbind(colQtrWgts_name, colQtr_Dec08)
Dec09Q4Wgts <- cbind(colQtrWgts_name, colQtr_Dec09)
Dec10Q4Wgts <- cbind(colQtrWgts_name, colQtr_Dec10)
Dec11Q4Wgts <- cbind(colQtrWgts_name, colQtr_Dec11)
Dec12Q4Wgts <- cbind(colQtrWgts_name, colQtr_Dec12)
Dec13Q4Wgts <- cbind(colQtrWgts_name, colQtr_Dec13)
Dec14Q4Wgts <- cbind(colQtrWgts_name, colQtr_Dec14)
Dec15Q4Wgts <- cbind(colQtrWgts_name, colQtr_Dec15)
Dec16Q4Wgts <- cbind(colQtrWgts_name, colQtr_Dec16)
Dec17Q4Wgts <- cbind(colQtrWgts_name, colQtr_Dec17)
Dec18Q4Wgts <- cbind(colQtrWgts_name, colQtr_Dec18)
Dec19Q4Wgts <- cbind(colQtrWgts_name, colQtr_Dec19)

Mar09Q1Wgts <- cbind(colQtrWgts_name, ColQtr_Mar09)
Mar10Q1Wgts <- cbind(colQtrWgts_name, colQtr_Mar10)
Mar11Q1Wgts <- cbind(colQtrWgts_name, colQtr_Mar11)
Mar12Q1Wgts <- cbind(colQtrWgts_name, colQtr_Mar12)
Mar13Q1Wgts <- cbind(colQtrWgts_name, colQtr_Mar13)
Mar14Q1Wgts <- cbind(colQtrWgts_name, colQtr_Mar14)
Mar15Q1Wgts<- cbind(colQtrWgts_name, colQtr_Mar15)
Mar16Q1Wgts <- cbind(colQtrWgts_name, colQtr_Mar16)
Mar17Q1Wgts <- cbind(colQtrWgts_name, colQtr_Mar17)
Mar18Q1Wgts <- cbind(colQtrWgts_name, colQtr_Mar18)
Mar19Q1Wgts <- cbind(colQtrWgts_name, colQtr_Mar19)
Mar20Q1Wgts <- cbind(colQtrWgts_name, colQtr_Mar20)

Jun09Q2Wgts <- cbind(colQtrWgts_name, colQtr_Jun09)
Jun10Q2Wgts <- cbind(colQtrWgts_name, colQtr_Jun10)
Jun11Q2Wgts <- cbind(colQtrWgts_name, colQtr_Jun11)
Jun12Q2Wgts <- cbind(colQtrWgts_name, colQtr_Jun12)
Jun13Q2Wgts <- cbind(colQtrWgts_name, colQtr_Jun13)
Jun14Q2Wgts <- cbind(colQtrWgts_name, colQtr_Jun14)
Jun15Q2Wgts <- cbind(colQtrWgts_name, colQtr_Jun15)
Jun16Q2Wgts <- cbind(colQtrWgts_name, colQtr_Jun16)
Jun17Q2Wgts <- cbind(colQtrWgts_name, colQtr_Jun17)
Jun18Q2Wgts <- cbind(colQtrWgts_name, colQtr_Jun18)
Jun19Q2Wgts <- cbind(colQtrWgts_name, colQtr_Jun19)
Jun20Q2Wgts <- cbind(colQtrWgts_name, colQtr_Jun20)

Sep09Q3Wgts<- cbind(colQtrWgts_name, colQtr_Sep09)
Sep10Q3Wgts <- cbind(colQtrWgts_name, colQtr_Sep10)
Sep11Q3Wgts <- cbind(colQtrWgts_name, colQtr_Sep11)
Sep12Q3Wgts <- cbind(colQtrWgts_name, colQtr_Sep12)
Sep13Q3Wgts <- cbind(colQtrWgts_name, colQtr_Sep13)
Sep14Q3Wgts <- cbind(colQtrWgts_name, colQtr_Sep14)
Sep15Q3Wgts <- cbind(colQtrWgts_name, colQtr_Sep15)
Sep16Q3Wgts <- cbind(colQtrWgts_name, colQtr_Sep16)
Sep17Q3Wgts <- cbind(colQtrWgts_name, colQtr_Sep17)
Sep18Q3Wgts <- cbind(colQtrWgts_name, colQtr_Sep18)
Sep19Q3Wgts <- cbind(colQtrWgts_name, colQtr_Sep19)


# Aggregate of Prices for Port Vila for the Forthnight #

colQtr4_08 <- data.frame(portVilaF1Price_stats$Dec.08, portVilaF2Price_stats$Dec.08, portVilaF3Price_stats$Dec.08,
                         portVilaF4Price_stats$Dec.08, portVilaF5Price_stats$Dec.08, portVilaF6Price_stats$Dec.08)
names(colQtr4_08) <- c("Dec.08F1", "Dec.08F2", "Dec.08F3", "Dec.08F4", "Dec.08F5", "Dec.08F6")
colQtr4_08$Dec08Q4 = rowSums(colQtr4_08,na.rm = TRUE, dims = 1L)
Dec08Q4Prices <- cbind(colQtrWgts_name,colQtr4_08)

colQtr1_09 <- data.frame(portVilaF1Price_stats$Mar.09, portVilaF2Price_stats$Mar.09, portVilaF3Price_stats$Mar.09,
                         portVilaF4Price_stats$Mar.09, portVilaF5Price_stats$Mar.09, portVilaF6Price_stats$Mar.09)
names(colQtr1_09) <- c("Mar.09F1", "Mar.09F2", "Mar.09F3", "Mar.09F4", "Mar.09F5", "Mar.09F6")
colQtr1_09$rMar09Q1 = rowSums(colQtr1_09,na.rm = TRUE, dims = 1L)
Mar09Q1Prices <- cbind(colQtrWgts_name,colQtr1_09)

colQtr2_09 <- data.frame(portVilaF1Price_stats$Jun.09, portVilaF2Price_stats$Jun.09, portVilaF3Price_stats$Jun.09,
                         portVilaF4Price_stats$Jun.09, portVilaF5Price_stats$Jun.09, portVilaF6Price_stats$Jun.09)
names(colQtr2_09) <- c("Jun.09F1", "Jun.09F2", "Jun.09F3", "Jun.09F4", "Jun.09F5", "Jun.09F6")
colQtr2_09$Jun09Q2 = rowSums(colQtr2_09,na.rm = TRUE, dims = 1L)
Jun09Q2Prices <- cbind(colQtrWgts_name,colQtr2_09)

colQtr3_09 <- data.frame(portVilaF1Price_stats$Sep.09, portVilaF2Price_stats$Sep.09, portVilaF3Price_stats$Sep.09,
                         portVilaF4Price_stats$Sep.09, portVilaF5Price_stats$Sep.09, portVilaF6Price_stats$Sep.09)
names(colQtr3_09) <- c("Sep.09F1", "Sep.09F2", "Sep.09F3", "Sep.09F4", "Sep.09F5", "Sep.09F6")
colQtr3_09$Sep09Q3 = rowSums(colQtr3_09,na.rm = TRUE, dims = 1L)
Sep09Q3Prices <- cbind(colQtrWgts_name,colQtr3_09)

colQtr4_09 <- data.frame(portVilaF1Price_stats$Dec.09, portVilaF2Price_stats$Dec.09, portVilaF3Price_stats$Dec.09,
                         portVilaF4Price_stats$Dec.09, portVilaF5Price_stats$Dec.09, portVilaF6Price_stats$Dec.09)
names(colQtr4_09) <- c("Dec.09F1", "Dec.09F2", "Dec.09F3", "Dec.09F4", "Dec.09F5", "Dec.09F6")
colQtr4_09$Dec09Q4 = rowSums(colQtr4_09,na.rm = TRUE, dims = 1L)
Dec09Q4Prices <- cbind(colQtrWgts_name,colQtr4_09)

colQtr1_10 <- data.frame(portVilaF1Price_stats$Mar.10, portVilaF2Price_stats$Mar.10, portVilaF3Price_stats$Mar.10,
                         portVilaF4Price_stats$Mar.10, portVilaF5Price_stats$Mar.10, portVilaF6Price_stats$Mar.10)
names(colQtr1_10) <- c("Mar.10F1", "Mar.10F2", "Mar.10F3", "Mar.10F4", "Mar.10F5", "Mar.10F6")
colQtr1_10$rMar09Q1 = rowSums(colQtr1_10,na.rm = TRUE, dims = 1L)
Mar10Q1Prices <- cbind(colQtrWgts_name,colQtr1_10)

colQtr2_10 <- data.frame(portVilaF1Price_stats$Jun.10, portVilaF2Price_stats$Jun.10, portVilaF3Price_stats$Jun.10,
                         portVilaF4Price_stats$Jun.10, portVilaF5Price_stats$Jun.10, portVilaF6Price_stats$Jun.10)
names(colQtr2_10) <- c("Jun.10F1", "Jun.10F2", "Jun.10F3", "Jun.10F4", "Jun.10F5", "Jun.10F6")
colQtr2_10$Jun10Q2 = rowSums(colQtr2_10,na.rm = TRUE, dims = 1L)
Jun10Q2Prices <- cbind(colQtrWgts_name,colQtr2_10)

colQtr3_10 <- data.frame(portVilaF1Price_stats$Sep.10, portVilaF2Price_stats$Sep.10, portVilaF3Price_stats$Sep.10,
                         portVilaF4Price_stats$Sep.10, portVilaF5Price_stats$Sep.10, portVilaF6Price_stats$Sep.10)
names(colQtr3_10) <- c("Sep.10F1", "Sep.10F2", "Sep.10F3", "Sep.10F4", "Sep.10F5", "Sep.10F6")
colQtr3_10$Sep10Q3 = rowSums(colQtr3_10,na.rm = TRUE, dims = 1L)
Sep10Q3Prices <- cbind(colQtrWgts_name,colQtr3_10)

colQtr4_10 <- data.frame(portVilaF1Price_stats$Dec.10, portVilaF2Price_stats$Dec.10, portVilaF3Price_stats$Dec.10,
                         portVilaF4Price_stats$Dec.10, portVilaF5Price_stats$Dec.10, portVilaF6Price_stats$Dec.10)
names(colQtr4_10) <- c("Dec.10F1", "Dec.10F2", "Dec.10F3", "Dec.10F4", "Dec.10F5", "Dec.10F6")
colQtr4_10$Dec10Q4= rowSums(colQtr4_10,na.rm = TRUE, dims = 1L)
Dec10Q4Prices <- cbind(colQtrWgts_name,colQtr4_10)

colQtr1_11 <- data.frame(portVilaF1Price_stats$Mar.11, portVilaF2Price_stats$Mar.11, portVilaF3Price_stats$Mar.11,
                         portVilaF4Price_stats$Mar.11, portVilaF5Price_stats$Mar.11, portVilaF6Price_stats$Mar.11)
names(colQtr1_11) <- c("Mar.11F1", "Mar.11F2", "Mar.11F3", "Mar.11F4", "Mar.11F5", "Mar.11F6")
colQtr1_11$Mar11Q1 = rowSums(colQtr1_11,na.rm = TRUE, dims = 1L)
Mar11Q1Prices<- cbind(colQtrWgts_name,colQtr1_11)

colQtr2_11 <- data.frame(portVilaF1Price_stats$Jun.11, portVilaF2Price_stats$Jun.11, portVilaF3Price_stats$Jun.11,
                         portVilaF4Price_stats$Jun.11, portVilaF5Price_stats$Jun.11, portVilaF6Price_stats$Jun.11)
names(colQtr2_11) <- c("Jun.11F1", "Jun.11F2", "Jun.11F3", "Jun.11F4", "Jun.11F5", "Jun.11F6")
colQtr2_11$Jun11Q2 = rowSums(colQtr2_11,na.rm = TRUE, dims = 1L)
Jun11Q2Prices <- cbind(colQtrWgts_name,colQtr2_11)

colQtr3_11 <- data.frame(portVilaF1Price_stats$Sep.11, portVilaF2Price_stats$Sep.11, portVilaF3Price_stats$Sep.11,
                         portVilaF4Price_stats$Sep.11, portVilaF5Price_stats$Sep.11, portVilaF6Price_stats$Sep.11)
names(colQtr3_11) <- c("Sep.11F1", "Sep.11F2", "Sep.11F3", "Sep.11F4", "Sep.11F5", "Sep.11F6")
colQtr3_11$Sep11Q3 = rowSums(colQtr3_11,na.rm = TRUE, dims = 1L)
Sep11Q3Prices <- cbind(colQtrWgts_name,colQtr3_11)

colQtr4_11 <- data.frame(portVilaF1Price_stats$Dec.11, portVilaF2Price_stats$Dec.11, portVilaF3Price_stats$Dec.11,
                         portVilaF4Price_stats$Dec.11, portVilaF5Price_stats$Dec.11, portVilaF6Price_stats$Dec.11)
names(colQtr4_11) <- c("Dec.11F1", "Dec.11F2", "Dec.11F3", "Dec.11F4", "Dec.11F5", "Dec.11F6")
colQtr4_11$Dec11Q4 = rowSums(colQtr4_11,na.rm = TRUE, dims = 1L)
Dec11Q4Prices <- cbind(colQtrWgts_name,colQtr4_11)

colQtr1_12 <- data.frame(portVilaF1Price_stats$Mar.12, portVilaF2Price_stats$Mar.12, portVilaF3Price_stats$Mar.12,
                         portVilaF4Price_stats$Mar.12, portVilaF5Price_stats$Mar.12, portVilaF6Price_stats$Mar.12)
names(colQtr1_12) <- c("Mar.12F1", "Mar.12F2", "Mar.12F3", "Mar.12F4", "Mar.12F5", "Mar.12F6")
colQtr1_12$Mar12Q1 = rowSums(colQtr1_12,na.rm = TRUE, dims = 1L)
Mar12Q1Prices <- cbind(colQtrWgts_name,colQtr1_12)

colQtr2_12 <- data.frame(portVilaF1Price_stats$Jun.12, portVilaF2Price_stats$Jun.12, portVilaF3Price_stats$Jun.12,
                         portVilaF4Price_stats$Jun.12, portVilaF5Price_stats$Jun.12, portVilaF6Price_stats$Jun.12)
names(colQtr2_12) <- c("Jun.12F1", "Jun.12F2", "Jun.12F3", "Jun.12F4", "Jun.12F5", "Jun.12F6")
colQtr2_12$Jun12Q2 = rowSums(colQtr2_12,na.rm = TRUE, dims = 1L)
Jun12Q2Prices <- cbind(colQtrWgts_name,colQtr2_12)

colQtr3_12 <- data.frame(portVilaF1Price_stats$Sep.12, portVilaF2Price_stats$Sep.12, portVilaF3Price_stats$Sep.12,
                         portVilaF4Price_stats$Sep.12, portVilaF5Price_stats$Sep.12, portVilaF6Price_stats$Sep.12)
names(colQtr3_12) <- c("Sep.12F1", "Sep.12F2", "Sep.12F3", "Sep.12F4", "Sep.12F5", "Sep.12F6")
colQtr3_12$Sep12Q3 = rowSums(colQtr3_12,na.rm = TRUE, dims = 1L)
sep12Q3Prices <- cbind(colQtrWgts_name,colQtr3_12)

colQtr4_12 <- data.frame(portVilaF1Price_stats$Dec.12, portVilaF2Price_stats$Dec.12, portVilaF3Price_stats$Dec.12,
                         portVilaF4Price_stats$Dec.12, portVilaF5Price_stats$Dec.12, portVilaF6Price_stats$Dec.12)
names(colQtr4_12) <- c("Dec.12F1", "Dec.12F2", "Dec.12F3", "Dec.12F4", "Dec.12F5", "Dec.12F6")
colQtr4_12$Dec12Q4 = rowSums(colQtr4_12,na.rm = TRUE, dims = 1L)
Dec12Q4Prices <- cbind(colQtrWgts_name,colQtr4_12)

colQtr1_13 <- data.frame(portVilaF1Price_stats$Mar.13, portVilaF2Price_stats$Mar.13, portVilaF3Price_stats$Mar.13,
                         portVilaF4Price_stats$Mar.13, portVilaF5Price_stats$Mar.13, portVilaF6Price_stats$Mar.13)
names(colQtr1_13) <- c("Mar.13F1", "Mar.13F2", "Mar.13F3", "Mar.13F4", "Mar.13F5", "Mar.13F6")
colQtr1_13$Mar13Q1 = rowSums(colQtr1_13,na.rm = TRUE, dims = 1L)
Mar13Q1Prices <- cbind(colQtrWgts_name,colQtr1_13)

colQtr2_13 <- data.frame(portVilaF1Price_stats$Jun.13, portVilaF2Price_stats$Jun.13, portVilaF3Price_stats$Jun.13,
                         portVilaF4Price_stats$Jun.13, portVilaF5Price_stats$Jun.13, portVilaF6Price_stats$Jun.13)
names(colQtr2_13) <- c("Jun.13F1", "Jun.13F2", "Jun.13F3", "Jun.13F4", "Jun.13F5", "Jun.13F6")
colQtr2_13$Jun13Q2 = rowSums(colQtr2_13,na.rm = TRUE, dims = 1L)
Jun13Q2Prices <- cbind(colQtrWgts_name,colQtr2_13)

colQtr3_13 <- data.frame(portVilaF1Price_stats$Sep.13, portVilaF2Price_stats$Sep.13, portVilaF3Price_stats$Sep.13,
                         portVilaF4Price_stats$Sep.13, portVilaF5Price_stats$Sep.13, portVilaF6Price_stats$Sep.13)
names(colQtr3_13) <- c("Sep.13F1", "Sep.13F2", "Sep.13F3", "Sep.13F4", "Sep.13F5", "Sep.13F6")
colQtr3_13$Sep13Q3 = rowSums(colQtr3_13,na.rm = TRUE, dims = 1L)
Sep13Q3Prices <- cbind(colQtrWgts_name,colQtr3_13)     

colQtr4_13 <- data.frame(portVilaF1Price_stats$Dec.13, portVilaF2Price_stats$Dec.13, portVilaF3Price_stats$Dec.13,
                         portVilaF4Price_stats$Dec.13, portVilaF5Price_stats$Dec.13, portVilaF6Price_stats$Dec.13)
names(colQtr4_13) <- c("Dec.13F1", "Dec.13F2", "Dec.13F3", "Dec.13F4", "Dec.13F5", "Dec.13F6")
colQtr4_13$Dec13Q4 = rowSums(colQtr4_13,na.rm = TRUE, dims = 1L)
Dec13Q4Prices <- cbind(colQtrWgts_name,colQtr4_13) 

colQtr1_14 <- data.frame(portVilaF1Price_stats$Mar.14, portVilaF2Price_stats$Mar.14, portVilaF3Price_stats$Mar.14,
                         portVilaF4Price_stats$Mar.14, portVilaF5Price_stats$Mar.14, portVilaF6Price_stats$Mar.14)
names(colQtr1_14) <- c("Mar.14F1", "Mar.14F2", "Mar.14F3", "Mar.14F4", "Mar.14F5", "Mar.14F6")
colQtr1_14$Mar14Q1q = rowSums(colQtr1_14,na.rm = TRUE, dims = 1L)
Mar14Q1Prices <- cbind(colQtrWgts_name,colQtr1_14) 

colQtr2_14 <- data.frame(portVilaF1Price_stats$Jun.14, portVilaF2Price_stats$Jun.14, portVilaF3Price_stats$Jun.14,
                         portVilaF4Price_stats$Jun.14, portVilaF5Price_stats$Jun.14, portVilaF6Price_stats$Jun.14)
names(colQtr2_14) <- c("Jun.14F1", "Jun.14F2", "Jun.14F3", "Jun.14F4", "Jun.14F5", "Jun.14F6")
colQtr2_14$Jun14Q2 = rowSums(colQtr2_14,na.rm = TRUE, dims = 1L)
Jun14Q2Prices <- cbind(colQtrWgts_name,colQtr2_14) 

colQtr3_14 <- data.frame(portVilaF1Price_stats$Sep.14, portVilaF2Price_stats$Sep.14, portVilaF3Price_stats$Sep.14,
                         portVilaF4Price_stats$Sep.14, portVilaF5Price_stats$Sep.14, portVilaF6Price_stats$Sep.14)
names(colQtr3_14) <- c("Sep.14F1", "Sep.14F2", "Sep.14F3", "Sep.14F4", "Sep.14F5", "Sep.14F6")
colQtr3_14$Sep14Q3 = rowSums(colQtr3_14,na.rm = TRUE, dims = 1L)
Sep14Q3Prices <- cbind(colQtrWgts_name,colQtr3_14)

colQtr4_14 <- data.frame(portVilaF1Price_stats$Dec.14, portVilaF2Price_stats$Dec.14, portVilaF3Price_stats$Dec.14,
                         portVilaF4Price_stats$Dec.14, portVilaF5Price_stats$Dec.14, portVilaF6Price_stats$Dec.14)
names(colQtr4_14) <- c("Dec.14F1", "Dec.14F2", "Dec.14F3", "Dec.14F4", "Dec.14F5", "Dec.14F6")
colQtr4_14$Dec14Q4 = rowSums(colQtr4_14,na.rm = TRUE, dims = 1L)
Dec14Q4Prices <- cbind(colQtrWgts_name,colQtr4_14)

colQtr1_15 <- data.frame(portVilaF1Price_stats$Mar.15, portVilaF2Price_stats$Mar.15, portVilaF3Price_stats$Mar.15,
                         portVilaF4Price_stats$Mar.15, portVilaF5Price_stats$Mar.15, portVilaF6Price_stats$Mar.15)
names(colQtr1_15) <- c("Mar.15F1", "Mar.15F2", "Mar.15F3", "Mar.15F4", "Mar.15F5", "Mar.15F6")
colQtr1_15$Mar15Q1 = rowSums(colQtr1_15,na.rm = TRUE, dims = 1L)
Mar15Q1Prices <- cbind(colQtrWgts_name,colQtr1_15)

colQtr2_15 <- data.frame(portVilaF1Price_stats$Jun.15, portVilaF2Price_stats$Jun.15, portVilaF3Price_stats$Jun.15,
                         portVilaF4Price_stats$Jun.15, portVilaF5Price_stats$Jun.15, portVilaF6Price_stats$Jun.15)
names(colQtr2_15) <- c("Jun.15F1", "Jun.15F2", "Jun.15F3", "Jun.15F4", "Jun.15F5", "Jun.15F6")
colQtr2_15$Jun15Q2 = rowSums(colQtr2_15,na.rm = TRUE, dims = 1L)
Jun15Q2Prices <- cbind(colQtrWgts_name,colQtr2_15)

colQtr3_15 <- data.frame(portVilaF1Price_stats$Sep.15, portVilaF2Price_stats$Sep.15, portVilaF3Price_stats$Sep.15,
                         portVilaF4Price_stats$Sep.15, portVilaF5Price_stats$Sep.15, portVilaF6Price_stats$Sep.15)
names(colQtr3_15) <- c("Sep.15F1", "Sep.15F2", "Sep.15F3", "Sep.15F4", "Sep.15F5", "Sep.15F6")
colQtr3_15$Sep15Q3 = rowSums(colQtr3_15,na.rm = TRUE, dims = 1L)
Sep15Q3Prices <- cbind(colQtrWgts_name,colQtr3_15)

colQtr4_15 <- data.frame(portVilaF1Price_stats$Dec.15, portVilaF2Price_stats$Dec.15, portVilaF3Price_stats$Dec.15,
                         portVilaF4Price_stats$Dec.15, portVilaF5Price_stats$Dec.15, portVilaF6Price_stats$Dec.15)
names(colQtr4_15) <- c("Dec.15F1", "Dec.15F2", "Dec.15F3", "Dec.15F4", "Dec.15F5", "Dec.15F6")
colQtr4_15$row_sums = rowSums(colQtr4_15,na.rm = TRUE, dims = 1L)
Dec15Q4Prices <- cbind(colQtrWgts_name,colQtr4_15)

colQtr1_16 <- data.frame(portVilaF1Price_stats$Mar.16, portVilaF2Price_stats$Mar.16, portVilaF3Price_stats$Mar.16,
                         portVilaF4Price_stats$Mar.16, portVilaF5Price_stats$Mar.16, portVilaF6Price_stats$Mar.16)
names(colQtr1_16) <- c("Mar.16F1", "Mar.16F2", "Mar.16F3", "Mar.16F4", "Mar.16F5", "Mar.16F6")
colQtr1_16$Mar16Q1 = rowSums(colQtr1_16,na.rm = TRUE, dims = 1L)
Mar16Q1Prices <- cbind(colQtrWgts_name,colQtr1_16)

colQtr2_16 <- data.frame(portVilaF1Price_stats$Jun.16, portVilaF2Price_stats$Jun.16, portVilaF3Price_stats$Jun.16,
                         portVilaF4Price_stats$Jun.16, portVilaF5Price_stats$Jun.16, portVilaF6Price_stats$Jun.16)
names(colQtr2_16) <- c("Jun.16F1", "Jun.16F2", "Jun.16F3", "Jun.16F4", "Jun.16F5", "Jun.16F6")
colQtr2_16$Jun16Q2 = rowSums(colQtr2_16,na.rm = TRUE, dims = 1L)
Jun16Q2Prices <- cbind(colQtrWgts_name,colQtr2_16)

colQtr3_16 <- data.frame(portVilaF1Price_stats$Sep.16, portVilaF2Price_stats$Sep.16, portVilaF3Price_stats$Sep.16,
                         portVilaF4Price_stats$Sep.16, portVilaF5Price_stats$Sep.16, portVilaF6Price_stats$Sep.16)
names(colQtr3_16) <- c("Sep.16F1", "Sep.16F2", "Sep.16F3", "Sep.16F4", "Sep.16F5", "Sep.16F6")
colQtr3_16$Sep16Q3 = rowSums(colQtr3_16,na.rm = TRUE, dims = 1L)
Sep16Q3Prices <- cbind(colQtrWgts_name,colQtr3_16)

colQtr4_16 <- data.frame(portVilaF1Price_stats$Dec.16, portVilaF2Price_stats$Dec.16, portVilaF3Price_stats$Dec.16,
                         portVilaF4Price_stats$Dec.16, portVilaF5Price_stats$Dec.16, portVilaF6Price_stats$Dec.16)
names(colQtr4_16) <- c("Dec.16F1", "Dec.16F2", "Dec.16F3", "Dec.16F4", "Dec.16F5", "Dec.16F6")
colQtr4_16$Dec16Q4 = rowSums(colQtr4_16,na.rm = TRUE, dims = 1L)
Dec16Q4Prices <- cbind(colQtrWgts_name,colQtr4_16)

colQtr1_17 <- data.frame(portVilaF1Price_stats$Mar.17, portVilaF2Price_stats$Mar.17, portVilaF3Price_stats$Mar.17,
                         portVilaF4Price_stats$Mar.17, portVilaF5Price_stats$Mar.17, portVilaF6Price_stats$Mar.17)
names(colQtr1_17) <- c("Mar.17F1", "Mar.17F2", "Mar.17F3", "Mar.17F4", "Mar.17F5", "Mar.17F6")
colQtr1_17$Mar17Q1 = rowSums(colQtr1_17,na.rm = TRUE, dims = 1L)
Mar17Q1Prices <- cbind(colQtrWgts_name,colQtr1_17)

colQtr2_17 <- data.frame(portVilaF1Price_stats$Jun.17, portVilaF2Price_stats$Jun.17, portVilaF3Price_stats$Jun.17,
                         portVilaF4Price_stats$Jun.17, portVilaF5Price_stats$Jun.17, portVilaF6Price_stats$Jun.17)
names(colQtr2_17) <- c("Jun.17F1", "Jun.17F2", "Jun.17F3", "Jun.17F4", "Jun.17F5", "Jun.17F6")
colQtr2_17$Jun17Q2 = rowSums(colQtr2_17,na.rm = TRUE, dims = 1L)
Jun17Q2Prices <- cbind(colQtrWgts_name,colQtr2_17)

colQtr3_17 <- data.frame(portVilaF1Price_stats$Sep.17, portVilaF2Price_stats$Sep.17, portVilaF3Price_stats$Sep.17,
                         portVilaF4Price_stats$Sep.17, portVilaF5Price_stats$Sep.17, portVilaF6Price_stats$Sep.17)
names(colQtr3_17) <- c("Sep.17F1", "Sep.17F2", "Sep.17F3", "Sep.17F4", "Sep.17F5", "Sep.17F6")
colQtr3_17$Sep17Q3 = rowSums(colQtr3_17,na.rm = TRUE, dims = 1L)
Sep17Q3Prices <- cbind(colQtrWgts_name,colQtr3_17)

colQtr4_17 <- data.frame(portVilaF1Price_stats$Dec.17, portVilaF2Price_stats$Dec.17, portVilaF3Price_stats$Dec.17,
                         portVilaF4Price_stats$Dec.17, portVilaF5Price_stats$Dec.17, portVilaF6Price_stats$Dec.17)
names(colQtr4_17) <- c("Dec.17F1", "Dec.17F2", "Dec.17F3", "Dec.17F4", "Dec.17F5", "Dec.17F6")
colQtr4_17$Dec17Q4 = rowSums(colQtr4_17,na.rm = TRUE, dims = 1L)
Dec17Q4Prices <- cbind(colQtrWgts_name,colQtr4_17)

colQtr1_18 <- data.frame(portVilaF1Price_stats$Mar.18, portVilaF2Price_stats$Mar.18, portVilaF3Price_stats$Mar.18,
                         portVilaF4Price_stats$Mar.18, portVilaF5Price_stats$Mar.18, portVilaF6Price_stats$Mar.18)
names(colQtr1_18) <- c("Mar.18F1", "Mar.18F2", "Mar.18F3", "Mar.18F4", "Mar.18F5", "Mar.18F6")
colQtr1_18$Mar18Q1 = rowSums(colQtr1_18,na.rm = TRUE, dims = 1L)
Mar18Q1Prices <- cbind(colQtrWgts_name,colQtr1_18)

colQtr2_18 <- data.frame(portVilaF1Price_stats$Jun.18, portVilaF2Price_stats$Jun.18, portVilaF3Price_stats$Jun.18,
                         portVilaF4Price_stats$Jun.18, portVilaF5Price_stats$Jun.18, portVilaF6Price_stats$Jun.18)
names(colQtr2_18) <- c("Jun.18F1", "Jun.18F2", "jun.18F3", "Jun.18F4", "Jun.18F5", "Jun.18F6")
colQtr2_18$Jun18Q2 = rowSums(colQtr2_18,na.rm = TRUE, dims = 1L)
Jun18Q2Prices <- cbind(colQtrWgts_name,colQtr2_18)

colQtr3_18 <- data.frame(portVilaF1Price_stats$Sep.18, portVilaF2Price_stats$Sep.18, portVilaF3Price_stats$Sep.18,
                         portVilaF4Price_stats$Sep.18, portVilaF5Price_stats$Sep.18, portVilaF6Price_stats$Sep.18)
names(colQtr3_18) <- c("Sep.18F1", "Sep.18F2", "Sep.18F3", "Sep.18F4", "Sep.18F5", "Sep.18F6")
colQtr3_18$Sep18Q3 = rowSums(colQtr3_18,na.rm = TRUE, dims = 1L)
Sep18Q3Prices <- cbind(colQtrWgts_name,colQtr3_18)

colQtr4_18 <- data.frame(portVilaF1Price_stats$Dec.18, portVilaF2Price_stats$Dec.18, portVilaF3Price_stats$Dec.18,
                         portVilaF4Price_stats$Dec.18, portVilaF5Price_stats$Dec.18, portVilaF6Price_stats$Dec.18)
names(colQtr4_18) <- c("Dec.18F1", "Dec.18F2", "Dec.18F3", "Dec.18F4", "Dec.18F5", "Dec.18F6")
colQtr4_18$Dec18Q4 = rowSums(colQtr4_18,na.rm = TRUE, dims = 1L)
Dec18Q4Prices <- cbind(colQtrWgts_name,colQtr4_18)

colQtr1_19 <- data.frame(portVilaF1Price_stats$Mar.19, portVilaF2Price_stats$Mar.19, portVilaF3Price_stats$Mar.19,
                         portVilaF4Price_stats$Mar.19, portVilaF5Price_stats$Mar.19, portVilaF6Price_stats$Mar.19)
names(colQtr1_19) <- c("Mar.19F1", "Mar.19F2", "Mar.19F3", "Mar.19F4", "Mar.19F5", "Mar.19F6")
colQtr1_19$Mar19Q1 = rowSums(colQtr1_19,na.rm = TRUE, dims = 1L)
Mar19Q1Prices <- cbind(colQtrWgts_name,colQtr1_19)

colQtr2_19 <- data.frame(portVilaF1Price_stats$Jun.19, portVilaF2Price_stats$Jun.19, portVilaF3Price_stats$Jun.19,
                         portVilaF4Price_stats$Jun.19, portVilaF5Price_stats$Jun.19, portVilaF6Price_stats$Jun.19)
names(colQtr2_19) <- c("Jun.19F1", "Jun.19F2", "Jun.19F3", "Jun.19F4", "Jun.19F5", "Jun.19F6")
colQtr2_19$Jun19Q2 = rowSums(colQtr2_19,na.rm = TRUE, dims = 1L)
Jun19Q2Prices <- cbind(colQtrWgts_name,colQtr2_19)

colQtr3_19 <- data.frame(portVilaF1Price_stats$Sep.19, portVilaF2Price_stats$Sep.19, portVilaF3Price_stats$Sep.19,
                         portVilaF4Price_stats$Sep.19, portVilaF5Price_stats$Sep.19, portVilaF6Price_stats$Sep.19)
names(colQtr3_19) <- c("Sep.19F1", "Sep.19F2", "Sep.19F3", "Sep.19F4", "Sep.19F5", "Sep.19F6")
colQtr3_19$Sep19Q3 = rowSums(colQtr3_19,na.rm = TRUE, dims = 1L)
Sep19Q3Prices <- cbind(colQtrWgts_name,colQtr3_19)

colQtr4_19 <- data.frame(portVilaF1Price_stats$Dec.19, portVilaF2Price_stats$Dec.19, portVilaF3Price_stats$Dec.19,
                         portVilaF4Price_stats$Dec.19, portVilaF5Price_stats$Dec.19, portVilaF6Price_stats$Dec.19)
names(colQtr4_19) <- c("Dec.19F1", "Dec.19F2", "Dec.19F3", "Dec.19F4", "Dec.19F5", "Dec.19F6")
colQtr4_19$Dec19Q4 = rowSums(colQtr4_19,na.rm = TRUE, dims = 1L)
Dec19Q4Prices <- cbind(colQtrWgts_name,colQtr4_19)

colQtr1_20 <- data.frame(portVilaF1Price_stats$Mar.20, portVilaF2Price_stats$Mar.20, portVilaF3Price_stats$Mar.20,
                         portVilaF4Price_stats$Mar.20, portVilaF5Price_stats$Mar.20, portVilaF6Price_stats$Mar.20)
names(colQtr1_20) <- c("Mar.20F1", "Mar.20F2", "Mar.20F3", "Mar.20F4", "Mar.20F5", "Mar.20F6")
colQtr1_20$rMar20Q1 = rowSums(colQtr1_20,na.rm = TRUE, dims = 1L)
Mar20Q1Prices <- cbind(colQtrWgts_name,colQtr1_20)

colQtr2_20 <- data.frame(portVilaF1Price_stats$Jun.20, portVilaF2Price_stats$Jun.20, portVilaF3Price_stats$Jun.20,
                         portVilaF4Price_stats$Jun.20, portVilaF5Price_stats$Jun.20, portVilaF6Price_stats$Jun.20)
names(colQtr2_20) <- c("Jun.20F1", "Jun.20F2", "Jun.20F3", "Jun.20F4", "Jun.20F5", "Jun.20F6")
colQtr2_20$Jun20Q2 = rowSums(colQtr2_20,na.rm = TRUE, dims = 1L)
Jun20Q2Prices <- cbind(colQtrWgts_name,colQtr2_20)


# Dividing the Aggregate Price over Aggregate Weights for Quarter for Port Vila #

#Following the mutate functions, allow for us to keep the constant Columns and be able to
# create a new column where we take Price and divide it by Weight of their respective Quarters.

#1. Dividing the price over weights for the Quarter 4 over the past years.
Dec08PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Dec08Q4Prices$Dec08Q4/ Dec08Q4Wgts$Dec08Q4)
Dec09PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Dec09Q4Prices$Dec09Q4/ Dec09Q4Wgts$Dec09Q4)
Dec10PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Dec10Q4Prices$Dec10Q4/ Dec10Q4Wgts$Dec10Q4)
Dec11PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Dec11Q4Prices$Dec11Q4/ Dec11Q4Wgts$Dec11Q4)
Dec12PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Dec12Q4Prices$Dec12Q4/ Dec12Q4Wgts$Dec12Q4)
Dec13PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Dec13Q4Prices$Dec13Q4/ Dec13Q4Wgts$Dec13Q4)
Dec14PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Dec14Q4Prices$Dec14Q4/ Dec14Q4Wgts$Dec14Q4)
Dec15PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Dec15Q4Prices$row_sums/ Dec15Q4Wgts$Dec15Q4) #row_sums is the title of the column where we add all the forthnights
Dec16PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Dec16Q4Prices$Dec16Q4/ Dec16Q4Wgts$Dec16Q4)
Dec17PriceWgts <- mutate(colQtrWgts_name, pricePerKilo= Dec17Q4Prices$Dec17Q4/ Dec17Q4Wgts$Dec17Q4)
Dec18PriceWgts <- mutate(colQtrWgts_name, pricePerKilo= Dec18Q4Prices$Dec18Q4/ Dec18Q4Wgts$Dec18Q4)
Dec19PriceWgts <- mutate(colQtrWgts_name, pricePerKilo= Dec19Q4Prices$Dec19Q4/ Dec19Q4Wgts$Dec19Q4)

#2. Dividing the price over weights for the Quarter 1 over the past years.

Mar09PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Mar09Q1Prices$rMar09Q1/ Mar09Q1Wgts$Mar09Q1) #The r in front of the rMAr09Q1 is the name of the column, this can be seen multiple times.
Mar10PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Mar10Q1Prices$rMar09Q1/ Mar10Q1Wgts$Mar10Q1)
Mar11PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Mar11Q1Prices$Mar11Q1/ Mar11Q1Wgts$Mar11Q1)
Mar12PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Mar12Q1Prices$Mar12Q1/ Mar12Q1Wgts$Mar12Q1)
Mar13PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Mar13Q1Prices$Mar13Q1/ Mar13Q1Wgts$Mar13Q1)
Mar14PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Mar14Q1Prices$Mar14Q1/ Mar14Q1Wgts$Mar14Q1)
Mar15PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Mar15Q1Prices$Mar15Q1/ Mar15Q1Wgts$Mar15Q1)
Mar16PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Mar16Q1Prices$Mar16Q1/ Mar16Q1Wgts$Mar16Q1)
Mar17PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Mar17Q1Prices$Mar17Q1/ Mar17Q1Wgts$Mar17Q1)
Mar18PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Mar18Q1Prices$Mar18Q1/ Mar18Q1Wgts$Mar18Q1)
Mar19PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Mar19Q1Prices$Mar19Q1/ Mar19Q1Wgts$Mar19Q1)
Mar20PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Mar20Q1Prices$rMar20Q1/ Mar20Q1Wgts$Mar20Q1)

#3. Dividing the price over weights for the Quarter 2 over the past years
Jun09PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Jun09Q2Prices$Jun09Q2 / Jun09Q2Wgts$Jun09Q2)
Jun10PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Jun10Q2Prices$Jun10Q2 / Jun10Q2Wgts$Jun10Q2)
Jun11PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Jun11Q2Prices$Jun11Q2 / Jun11Q2Wgts$Jun11Q2)
Jun12PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Jun12Q2Prices$Jun12Q2 / Jun12Q2Wgts$Jun12Q2)
Jun13PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Jun13Q2Prices$Jun13Q2 / Jun13Q2Wgts$Jun13Q2)
Jun14PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Jun14Q2Prices$Jun14Q2 / Jun14Q2Wgts$Jun14Q2)
Jun15PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Jun15Q2Prices$Jun15Q2 / Jun15Q2Wgts$Jun15Q2)
Jun16PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Jun16Q2Prices$Jun16Q2 / Jun16Q2Wgts$Jun16Q2)
Jun17PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Jun17Q2Prices$Jun17Q2 / Jun17Q2Wgts$Jun17Q2)
Jun18PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Jun18Q2Prices$Jun18Q2 / Jun18Q2Wgts$Jun18Q2)
Jun19PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Jun19Q2Prices$Jun19Q2 / Jun19Q2Wgts$Jun19Q2)
Jun20PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Jun20Q2Prices$Jun20Q2 / Jun20Q2Wgts$Jun20Q2)

#4. Dividing the price over weights for the Quarter 3 over the past years
Sep09PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Sep09Q3Prices$Sep09Q3 / Sep09Q3Wgts$Sep09Q3)
Sep10PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Sep10Q3Prices$Sep10Q3 / Sep10Q3Wgts$Sep10Q3)
Sep11PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Sep11Q3Prices$Sep11Q3 / Sep11Q3Wgts$Sep11Q3)
Sep12PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = sep12Q3Prices$Sep12Q3 / Sep12Q3Wgts$Sep12Q3)
Sep13PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Sep13Q3Prices$Sep13Q3 / Sep13Q3Wgts$Sep13Q3)
Sep14PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Sep14Q3Prices$Sep14Q3 / Sep14Q3Wgts$Sep14Q3)
Sep15PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Sep15Q3Prices$Sep15Q3 / Sep15Q3Wgts$Sep15Q3)
Sep16PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Sep16Q3Prices$Sep16Q3 / Sep16Q3Wgts$Sep16Q3)
Sep17PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Sep17Q3Prices$Sep17Q3 / Sep17Q3Wgts$Sep17Q3)
Sep18PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Sep18Q3Prices$Sep18Q3 / Sep18Q3Wgts$Sep18Q3)
Sep19PriceWgts <- mutate(colQtrWgts_name, pricePerKilo = Sep19Q3Prices$Sep19Q3 / Sep19Q3Wgts$Sep19Q3)

# Merging of the Quarterly Data into the Master Spreadsheet #

quarterly_data <- openxlsx::read.xlsx (
  xlsxFile = file.path(repository, "data", "secure", "CPI Port Vila.xlsx"),
  (sheets = ("PVila Item list")),
startRow = (2:1869)
  )

#1. Input all the pricePerKilo into the cpi_stats.
#2. Subset the data for the ItemId and their respective Qtr
#3. Dump the values into the cpi_stats along with the rest of the ItemId in place.

#mydb <- dbConnect(RSQLite::SQLite(), "data/secure/cpi.sqlite")

#dbListTables(mydb)

#dbWriteTable(mydb, "cpi_stats", cpi_stats, overwrite=TRUE)
#dbWriteTable(mydb, "dec08PriceWgts", Dec08PriceWgts, overwrite=TRUE)

#dbGetQuery(mydb, "UPDATE cpi_stats SET cpi_stats.[Dec.08] = dec08PriceWgts.pricePerKilo FROM dec08PriceWgts, cpi_stats WHERE cpi_stats.ItemId = dec08PriceWgts.ItemId")


#### Stage 2: Quality check of quarterly collection ####


#### Stage 3: Sample Aggregates ####

# Read in the raw data from secure folder of the repository 
cpiItemListFile <- file.path(secureDataFolder, "13.CPI_ Average_Price_PVItemList.csv")
cpiItemList <- read.csv(cpiItemListFile, header=TRUE, na.strings=c("","NA", "NULL", "null")) 
cpiSampleAggFile <- file.path(secureDataFolder, "14.CPI_Aggregated_Price_SampleAggregates.csv")
cpiSampleAgg <- read.csv(cpiSampleAggFile, header=TRUE, na.strings=c("","NA", "NULL", "null")) 
cpiElementaryAggFile<- file.path(secureDataFolder, "15.CPI_ElementaryAggregates_Index_EAs.csv")
cpiElementaryAgg <- read.csv(cpiElementaryAggFile, header=TRUE, na.strings=c("","NA", "NULL", "null")) 

# Create subset data set of just Eas & formula method
eaCompMeth <- c("EA", "Formula.method")
newdata <- cpiSampleAgg[eaCompMeth]

# Merge new dataset with item list to assign formula method 
colnames(newdata)[1] <- "Eas"
cpiItemListMerged<- merge(cpiItemList, newdata, by="Eas", all.x=TRUE)

# Calculate the geometric and arithmetic means of commodities 
cpiItemListMerged$Mar.19 <- as.numeric(gsub(",", "", cpiItemListMerged$Mar.19))
arthMeanComm<- cpiItemListMerged %>%
  group_by(Eas) %>%
  filter(Mar.19 %in% c("NA")) %>%
  filter("Formula.method" %in% c("1")) %>%
  filter("Active" %in% c("Y")) %>%
  summarise(Mar.19 = mean(Mar.19))

geoMeanComm<- cpiItemList %>%
  group_by(Eas) %>%
  filter(`Formula method` %in% c("2")) %>%
  filter(Active %in% c("Y")) %>%
  summarise(Price = exp(mean(log(`40238`))))

# Row bind the geometric and arithmetic means together
meansCombined<- bind_rows(arthMeanComm, geoMeanComm)

# Append means combined with sample aggregates
colnames(cpiSampleAggStats)[1] <- "Eas"
sampleAggMerged<- merge(cpiSampleAggStats, meansCombined, by="Eas", all.x=TRUE)


#### Stage 4: Elementary Aggregates ####

# Create new column and append it to another dataframe
cpiElementaryAgg$Mar <- sampleAggMerged$Price / sampleAggMerged$'43983'


#### Stage 5: Value Aggregates ####









