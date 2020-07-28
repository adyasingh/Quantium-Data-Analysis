pacman::p_load(rio,dplyr,lubridate,skimr)
library(data.table)
library(ggplot2)
library(ggmosaic)
library(readr)
library(skimr)
library(plyr)
library(stringr)



purchase <- import("~/Desktop/Quantium/QVI_purchase_behaviour.csv")
head(purchase)
skim(purchase)

#View distribution of customer lifestages
skim(purchase$LIFESTAGE)
lifestage = setDT(purchase)[, .N, by=c("LIFESTAGE")]
View(lifestage)

#View distribution of customer types
skim(purchase$PREMIUM_CUSTOMER)
customerType = setDT(purchase)[, .N, by=c("PREMIUM_CUSTOMER")]
View(customerType)

#merge datasets
data <- merge(transaction, purchase, all.x = TRUE)
View(data)

#check for NULL values
checkNULL = is.na(data)
summary(checkNULL)

#write merged data to csv
filePath="~/Desktop/Quantium"
fwrite(data, paste0(filePath,"QVI_data.csv"))
write.csv(data,"~/Desktop/Quantium/QVI_data.csv", row.names = FALSE)
