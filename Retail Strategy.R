library(data.table)
library(ggplot2)
library(ggmosaic)
library(readr)
library(skimr)
library(plyr)
library(stringr)
library(tidyr)
library(lubridate) 
library(dplyr)

pacman::p_load(rio,dplyr,lubridate,skimr)

data <- import("~/Desktop/Quantium/QVI_data.csv")
head(data)

#### Set themes for plots
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

## Select control stores


data$YEARMONTH <- year(data$DATE)* 100 + month(data$DATE)

                      
data <- data.table(data)
measureOverTime <- data[, .(totSales = sum(TOT_SALES),
                            nCustomers = uniqueN(LYLTY_CARD_NBR),
                            nTxnPerCust = uniqueN(TXN_ID)/ uniqueN(LYLTY_CARD_NBR),
                            nChipsPerTxn=sum(PROD_QTY)/uniqueN(TXN_ID),
                            avgPricePerUnit = sum(TOT_SALES)/sum(PROD_QTY) )
                        , by = c("STORE_NBR", "YEARMONTH") ][order(STORE_NBR, YEARMONTH)]

View(measureOverTime)

#### Filter to the pre-trial period and stores with full observation periods
storesWithFullObs <- unique(measureOverTime[, .N, STORE_NBR][N == 12, STORE_NBR])
preTrialMeasures <- measureOverTime[YEARMONTH < 201902 & STORE_NBR %in% storesWithFullObs, ]

#### a function to calculate correlation for a measure, looping through each control store.
calculateCorrelation <- function(inputTable, metricCol, storeComparison) {
  calcCorrTable = data.table(Store1 = numeric(), Store2 = numeric(), corr_measure =
                               numeric())
  storeNumbers <-
    for (i in storeNumbers) {
      calculatedMeasure = data.table("Store1" = storeComparison,
                                     "Store2" = i,
                                     "corr_measure" =
      )
      calcCorrTable <- rbind(calcCorrTable, calculatedMeasure)
    }
  return(calcCorrTable)
}

#### function to calculate a standardised magnitude distance for a measure

calculateMagnitudeDistance <- function(inputTable, metricCol, storeComparison) {
  calcDistTable = data.table(Store1 = numeric(), Store2 = numeric(), YEARMONTH =
                               numeric(), measure = numeric())
  storeNumbers <- unique(inputTable[, STORE_NBR])
  for (i in storeNumbers) {
    calculatedMeasure = data.table("Store1" = storeComparison
                                   , "Store2" = i
                                   , "YEARMONTH" = inputTable[STORE_NBR ==
                                                                storeComparison, YEARMONTH]
                                   , "measure" = abs(inputTable[STORE_NBR ==
                                                                  storeComparison, eval(metricCol)]
                                                     - inputTable[STORE_NBR == i,
                                                                  eval(metricCol)])
    )
    calcDistTable <- rbind(calcDistTable, calculatedMeasure)
    
    
  }


#### Standardise the magnitude distance so that the measure ranges from 0 to 1
minMaxDist <- calcDistTable[, .(minDist = min(measure), maxDist = max(measure)),
                            by = c("Store1", "YEARMONTH")]
distTable <- merge(calcDistTable, minMaxDist, by = c("Store1", "YEARMONTH"))
distTable[, magnitudeMeasure := 1 - (measure - minDist)/(maxDist - minDist)]
finalDistTable <- distTable[, .(mag_measure = mean(magnitudeMeasure)), by =
                              .(Store1, Store2)]
return(finalDistTable)
}
