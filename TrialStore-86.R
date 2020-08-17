library(data.table)



measureOverTime <- data[, .(totSales = sum(TOT_SALES),
                            nCustomers = uniqueN(LYLTY_CARD_NBR),
                            nTxnPerCust = uniqueN(TXN_ID)/ uniqueN(LYLTY_CARD_NBR),
                            nChipsPerTxn=sum(PROD_QTY)/uniqueN(TXN_ID),
                            avgPricePerUnit = sum(TOT_SALES)/sum(PROD_QTY) )
                        , by = c("STORE_NBR", "YEARMONTH") ][order(STORE_NBR, YEARMONTH)]

trial_store <- 86
corr_nSales <- calculateCorrelation(preTrialMeasures, quote(totSales), trial_store)
corr_nCustomers <- calculateCorrelation(preTrialMeasures, quote(nCustomers), trial_store)
magnitude_nSales <- calculateMagnitudeDistance(preTrialMeasures, quote(totSales), trial_store)
magnitude_nCustomers <- calculateMagnitudeDistance(preTrialMeasures, quote(nCustomers), trial_store)

#### Now, create a combined score composed of correlation and magnitude

corr_weight <- 0.5
score_nSales <- merge(corr_nSales,magnitude_nSales , by = c("Store1", "Store2"))[, scoreNSales := corr_measure*corr_weight +mag_measure *(1-corr_weight)]
score_nCustomers <- merge(corr_nCustomers,magnitude_nCustomers , by = c("Store1", "Store2"))[, scoreNCust := corr_measure*corr_weight +mag_measure *(1-corr_weight)]

#### Finally, combine scores across the drivers using a simple average.
score_Control <- merge(score_nSales,score_nCustomers , by =  c("Store1", "Store2"))
score_Control[, finalControlScore := scoreNSales * 0.5 + scoreNCust * 0.5]

#### Select control store for trial store 86
control_store <- score_Control[Store1 == trial_store,
][order(-finalControlScore)][2, Store2]

print(control_store)
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
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month- Store 86")



## Number of Customers


measureOverTimeCusts <- measureOverTime
pastCustomers <- measureOverTimeCusts[, Store_type := ifelse(STORE_NBR == trial_store, "Trial", ifelse(STORE_NBR==control_store, "Control", "Other"))
][,numberCustomers :=mean(nCustomers), by = c("YEARMONTH", "Store_type")
][,TransactionMonth := as.Date(paste(YEARMONTH%%100, YEARMONTH%%100,1, sep='-'), "%Y-%m-%d")
][YEARMONTH<201903, ]

ggplot(pastCustomers, aes(TransactionMonth,numberCustomers , color = Store_type)) +
  geom_line() +
  labs(x = "Month of Operation", y = "Total Num of Customers", title = "Total Num of Customers per month- Store 86")

#### Scale pre-trial control sales to match pre-trial trial store sales
scalingFactorForControlSales <- preTrialMeasures[STORE_NBR == trial_store &
                                                   YEARMONTH < 201902, sum(totSales)]/preTrialMeasures[STORE_NBR == control_store &
                                                                                                         YEARMONTH < 201902, sum(totSales)]
#### Apply the scaling factor
measureOverTimeSales <- measureOverTime
scaledControlSales <- measureOverTimeSales[STORE_NBR == control_store, ][ ,
                                                                          controlSales := totSales * scalingFactorForControlSales]

#### Calculate the percentage difference between scaled control sales and trial sales

percentageDiff <- merge(scaledControlSales[,c("YEARMONTH", "controlSales")],
                        measureOverTime[STORE_NBR==trial_store, c("totSales", "YEARMONTH")],
                        by = "YEARMONTH"
)[, percentageDiff := abs(controlSales-totSales)/controlSales]


#### Calculate the standard deviation of percentage differences during the pre-trial period
stdDev <- sd(percentageDiff[YEARMONTH<201902, percentageDiff])
degreesOfFreedom <- 7


#### Trial and control store total sales
####  Create a table with sales by store type and month.
measureOverTimeSales <- measureOverTime

pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                         ifelse(STORE_NBR == control_store, "Control", "Other stores"))
][, totSales := mean(totSales), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][Store_type %in% c("Trial", "Control") ]

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
  geom_line(aes(linetype = Store_type)) +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month: 86")
















