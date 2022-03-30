#Clear the enviroment
rm(list = ls())

# Load the required libraries
library(dplyr) # Manipulating data
library(stringr)# common string operations
library(openxlsx)
library(ggplot2)

# Note where VNSO code/data is on current computer
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path), "..", "..")
setwd(repository) # Required for file.choose() function

# Load the general R functions
source(file.path(repository, "R", "functions.R"))

# Note the secure data path
secureDataFolder <- file.path(repository, "data", "secure")

# Note the open data path
openDataFolder <- file.path(repository, "data", "open")

#Working with Dates.
###dates <- as.date(dataset, format = "%d%m%Y")

#Merge the Exports and Imports
tradeStatsExportFile <- choose.files(secureDataFolder, "exports_HS_summaryStats_02-10-20.csv")
tradeStatsExport <- read.csv(tradeStatsExportFile, header = TRUE, na.strings = c(" ", "NA", "NULL", "null"))

#Imports
tradeStatsImportFile <- choose.files(secureDataFolder, "imports_HS_summaryStats_02-10-20.csv")
tradeStatsImport <- read.csv(tradeStatsImportFile, header = TRUE, na.strings = c(" ", "NA", "NULL", "null"))

#Merge raw data with Export Classification.
tradeStatsFileMergeImportCountry <- choose.files(openDataFolder, "OPN_FINAL_ASY_CountryDescriptionImportClassifications_31-01-20.xlsx")
importCountryOrigin <- read.xlsx (tradeStatsFileMergeImportCountry, na.strings=c("", "NA", "NULL", "null"))

tradeStatsFileMergeExportCountry <- choose.files(openDataFolder, "OPN_FINAL_ASY_CountryDescriptionExportClassifications_31-01-20.xlsx")
exportCountryOrigin <- read.xlsx (tradeStatsFileMergeExportCountry, na.strings=c("", "NA", "NULL", "null"))

#merging for export and import.
tradeStatsCommoditiesMergedWithImportClassifications <- merge(tradeStatsImport, importCountryOrigin, by="CTY_Origin",  all.x = TRUE )
tradeStatsCommoditiesMergedWithExportClassifications <- merge(tradeStatsExport, exportCountryOrigin, by="CTY_Dest",  all.x = TRUE )

#Create Subset of swine
swineExport <- tradeStatsCommoditiesMergedWithExportClassifications[tradeStatsCommoditiesMergedWithExportClassifications$HS %in% c(16024100, 16024200, 16024900), ]
swineImport <- tradeStatsCommoditiesMergedWithImportClassifications[tradeStatsCommoditiesMergedWithImportClassifications $HS %in% c(16024100, 16024200, 16024900), ]

#Group by function:
totalMonthlyValueExportTradeAgreements <- swineExport %>%
  group_by(Month) %>%
    summarise(total = sum(Value),
              Mean = mean(Value),
              Max = max(Value),
              Median = median(Value))

totalMonthlyValueImportTradeAgreements <- swineImport %>%
  group_by(Month) %>%
  summarise(total = sum(Value),
            Mean = mean(Value),
            Max = max(Value),
            Median = median(Value))

#Calculate summary statistics of value by year and destination country.
#totalMonthlyValueImportTradeAgreements <- swineImport %>%
  #group_by(Year, CTY_dest) %>%
  #summarise(total = sum(Value),
            #Mean = mean(Value),  probs function: for the percentage of quantile upper and lower.
            #Max = max(Value),
            #Median = median(Value, na.rm = FALSE - this will remove the NAs in the Data set.
            # .groups = "drop")
           #upper =
#Why do we used the probs function, to see the pattern of distribution because of the spread of values.
#Distribution of Value is important for the sake of quality data.

##Plot commands.
##using ggplot2 function
