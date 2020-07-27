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


transaction <- import("~/Desktop/Quantium/QVI_transaction_data.xlsx")
head(transaction)
View(transaction, "transaction")
skim(transaction)
# Exploratory Data Analysis 
str(transaction)

count(transaction$PROD_NAME)


#update data type 
transaction$DATE <- as.Date(transaction$DATE, origin="1899-12-30")
str(transaction)

#check for unique product names 
dt = setDT(transaction)[, .N, by=c("PROD_NAME", "PROD_NBR")]
View(dt)
#check if the frequencies add up 
sum(dt$N)

#order in descending order of frequency
dt=dt[order(-dt$N)]
View(dt)


#Data Cleanse: Remove all punctuation and digits
dt$PROD_NAME = str_replace_all(dt$PROD_NAME, "[[:punct:]]", " ")
dt$PROD_NAME = str_replace_all(dt$PROD_NAME, "[[:digit:]]", " ")
dt$PROD_NAME= str_replace_all(dt$PROD_NAME, " g", "")
dt$PROD_NAME= str_replace_all(dt$PROD_NAME, " G", "")

#Remove Salsa
transaction[, SALSA := grepl("salsa", tolower(PROD_NAME))]
transaction <- transaction[SALSA == FALSE, ][, SALSA := NULL]

summary(transaction)

#Check for outlier in PROD_QTY 

transaction[transaction$PROD_QTY==200]
transaction[transaction$LYLTY_CARD_NBR==226000]
transaction <- transaction[!(transaction$LYLTY_CARD_NBR==226000)]

summary(transaction)




#Check for dates 
dates = setDT(transaction)[, .N, by=c("DATE")]
View(dates)
skim(dates) #displays 364 unique days

#MISSING DATE: "2018-12-25"
d <- as.Date(dates$DATE)
date_range <- seq(min(d), max(d), by = 1) 
date_range[!date_range %in% d] 


newDates <- rbind(data.frame(DATE = as.Date("2018-12-25"), N = 0), dates)

#### Setting plot themes to format graphs
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))
#### Plot transactions over time
ggplot(newDates, aes(x = DATE, y = N)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over time") +
  scale_x_date(breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

decSales <-subset(newDates, DATE>= "2018-12-01" & DATE <= "2018-12-31")
View(decSales)

plot(decSales)
ggplot(decSales, aes(x = DATE, y = N)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions in December") +
  scale_x_date(breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


## PACK Size 
transaction[, PACK_SIZE := parse_number(PROD_NAME)]

transaction$PACK_SIZE <- parse_number(transaction$PROD_NAME)

max(transaction$PACK_SIZE) #70
min(transaction$PACK_SIZE) #380


hist(transaction$PACK_SIZE, main="Historgram of Packet Sizes", 
     xlab="Packet Size",)


#BRAND  

transaction$BRAND <- word(transaction$PROD_NAME, 1)
View(transaction)
transaction[BRAND == "RED", BRAND := "RRD"]
transaction[BRAND == "SNBTS", BRAND := "SUNBITES"]
transaction[BRAND == "INFZNS", BRAND := "INFUZIONS"]
transaction[BRAND == "WW", BRAND := "WOOLWORTHS"]
transaction[BRAND == "SMITH", BRAND := "SMITHS"]
transaction[BRAND == "NCC", BRAND := "NATURAL"]
transaction[BRAND == "DORITO", BRAND := "DORITOS"]
transaction[BRAND == "GRAIN", BRAND := "GRNWVES"]
