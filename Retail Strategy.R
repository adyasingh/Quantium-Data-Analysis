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
  storeNumbers <-unique(inputTable[, STORE_NBR])
    for (i in storeNumbers) {
      calculatedMeasure = data.table("Store1" = storeComparison,
                                     "Store2" = i,
                                     "corr_measure" = cor(inputTable[STORE_NBR == storeComparison, eval(metricCol)], inputTable[STORE_NBR==i, eval(metricCol)])
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


#### calculate correlations against store 77 using total sales and number of customers.

trial_store <- 77
corr_nSales <- calculateCorrelation(preTrialMeasures, quote(totSales), trial_store)
corr_nCustomers <- calculateCorrelation(preTrialMeasures, quote(nCustomers), trial_store)

#### Then, use the functions for calculating magnitude.
magnitude_nSales <- calculateMagnitudeDistance(preTrialMeasures, quote(totSales),trial_store)
magnitude_nCustomers <- calculateMagnitudeDistance(preTrialMeasures, quote(nCustomers), trial_store)


####combined score composed of correlation and magnitude, by merging the correlations table with the magnitude table 
corr_weight <- 0.5
score_nSales <- merge(corr_nSales,magnitude_nSales , by = c("Store1", "Store2"))[, scoreNSales := corr_measure*corr_weight +mag_measure *(1-corr_weight)]
score_nCustomers <- merge(corr_nCustomers,magnitude_nCustomers , by = c("Store1", "Store2"))[, scoreNCust := corr_measure*corr_weight +mag_measure *(1-corr_weight)]

#### Combine scores across the drivers by first merging our sales scores and customer scores into a single table
score_Control <- merge(score_nSales,score_nCustomers , by =  c("Store1", "Store2"))
score_Control[, finalControlScore := scoreNSales * 0.5 + scoreNCust * 0.5]

#### Select control stores based on the highest matching store (closest to 1 but
#### not the store itself, i.e. the second ranked highest store)

control_store <- score_Control[Store1==trial_store, ][order(-finalControlScore)][2,Store2]
control_store

#### Visual checks on trends based on the drivers
measureOverTimeSales <- measureOverTime
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store,
                                                         "Trial",
                                                         ifelse(STORE_NBR == control_store,
                                                                "Control", "Other stores"))
][, totSales := mean(totSales), by = c("YEARMONTH",
                                       "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/%
                                        100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][YEARMONTH < 201903 , ]
ggplot(pastSales, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_line() +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")

#### number of customers
####visual checks on customer count trends by comparing the trial store to the control store and other stores.


measureOverTimeCusts <- measureOverTime
pastCustomers <- measureOverTimeCusts[, Store_type := ifelse(STORE_NBR == trial_store, "Trial", ifelse(STORE_NBR==control_store, "Control", "Other"))
][,numberCustomers :=mean(nCustomers), by = c("YEARMONTH", "Store_type")
][,TransactionMonth := as.Date(paste(YEARMONTH%%100, YEARMONTH%%100,1, sep='-'), "%Y-%m-%d")
][YEARMONTH<201903, ]

ggplot(pastCustomers, aes(TransactionMonth,numberCustomers , color = Store_type)) +
  geom_line() +
  labs(x = "Month of Operation", y = "Total Num of Customers", title = "Total Num of Customers per month")


## Assessment of trial

#### Scale pre-trial control sales to match pre-trial trial store sales
scalingFactorForControlSales <- preTrialMeasures[STORE_NBR == trial_store & YEARMONTH < 201902, sum(totSales)]/preTrialMeasures[
  STORE_NBR == control_store & YEARMONTH < 201902, sum(totSales)]
#### Apply the scaling factor
measureOverTimeSales <- measureOverTime
scaledControlSales <- measureOverTimeSales[STORE_NBR == control_store, ][ ,controlSales := totSales * scalingFactorForControlSales]

####Calculate the percentage difference between scaled control sales and trial sales
percentageDiff <- merge(scaledControlSales[, c("YEARMONTH", "controlSales")],
                        measureOverTime[STORE_NBR== trial_store, c("totSales", "YEARMONTH")],
                        by ="YEARMONTH")[, percentageDiff := abs(controlSales-totSales)/controlSales]
#### standard deviation based on the scaled percentage differencein the pre-trial period
stdDev <- sd(percentageDiff[YEARMONTH < 201902 , percentageDiff])
degreesOfFreedom <- 7

####  Calculate the t-values for the trial months

percentageDiff[, tValue :=(percentageDiff/stdDev)
][, TransactionMonth :=  as.Date(paste(YEARMONTH%%100, YEARMONTH%%100,1, sep='-'), "%Y-%m-%d")
][YEARMONTH<201905 & YEARMONTH > 201901, .(TransactionMonth, tValue)]

qt(0.95, df = degreesOfFreedom)



measureOverTimeSales <- measureOverTime 


pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                         ifelse(STORE_NBR == control_store, "Control", "Other stores"))
][, totSales := mean(totSales), by = c("YEARMONTH","Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/%
                                        100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][Store_type %in% c("Trial", "Control"), ]

#### Control store 95th percentile
pastSales_Controls95 <- pastSales[Store_type == "Control",
][, totSales := totSales * (1 + stdDev * 2)
][, Store_type := "Control 95th % confidence
interval"]

#### Control store 5th percentile
pastSales_Controls5 <- pastSales[Store_type == "Control",
][, totSales := totSales * (1 - stdDev * 2)
][, Store_type := "Control 5th % confidence
interval"]

trialAssessment <- rbind(pastSales, pastSales_Controls95, pastSales_Controls5)

ggplot(trialAssessment, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_rect(data = trialAssessment[ YEARMONTH < 201905 & YEARMONTH > 201901 ,],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0 , ymax =
                  Inf, color = NULL), show.legend = FALSE) +
  geom_line() +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")


##Number of Customers - trial assessment 
################################################################################################################
#### Scale pre-trial control customers to match pre-trial trial store customers

scalingFactorForControlCust <- preTrialMeasures[STORE_NBR==trial_store & YEARMONTH< 201902, sum(nCustomers)/ 
                                                  preTrialMeasures[STORE_NBR==control_store & YEARMONTH< 201902, sum(nCustomers)]]
  
  
measureOverTimeCusts <- measureOverTime

scaledControlCustomers <- measureOverTimeCusts[STORE_NBR==control_store,
][, controlCustomers := nCustomers*scalingFactorForControlCust
][, Store_type :=ifelse(STORE_NBR==trial_store, "Trial", ifelse(STORE_NBR==control_store, "Control", "Other"))]

percentageDiff <- merge(scaledControlCustomers[,c("YEARMONTH", "controlCustomers")], 
                        measureOverTimeCusts[STORE_NBR==trial_store, c("nCustomers", "YEARMONTH")], by="YEARMONTH")[, percentageDiff:=
                                                                                                                      abs(controlCustomers-nCustomers)/controlCustomers]
stdDev <- sd(percentageDiff[YEARMONTH < 201902 , percentageDiff])
degreesOfFreedom <- 7


#### Trial and control store number of customers
pastCustomers <- measureOverTimeCusts[, nCusts := mean(nCustomers), by =
                                        c("YEARMONTH", "Store_type")
][Store_type %in% c("Trial", "Control"), ]
#### Control store 95th percentile
pastCustomers_Controls95 <- pastCustomers[Store_type == "Control",
][, nCusts := nCusts * (1 + stdDev * 2)
][, Store_type := "Control 95th % confidence interval"]

#### Control store 5th percentile
pastCustomers_Controls5 <- pastCustomers[Store_type == "Control",
][, nCusts := nCusts * (1 - stdDev * 2)
][, Store_type := "Control 5th % confidence interval"]

trialAssessment <- rbind(pastCustomers, pastCustomers_Controls95,
                         pastCustomers_Controls5)




ggplot(trialAssessment, aes(TransactionMonth, nCusts, color = Store_type)) +
  geom_rect(data = trialAssessment[ YEARMONTH < 201905 & YEARMONTH > 201901 ,],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0 , ymax =
                  Inf, color = NULL), show.legend = FALSE) +
  geom_line() +
  labs(x = "Month of operation", y = "Total num of Customers", title = "Total customers per month")








