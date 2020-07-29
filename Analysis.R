pacman::p_load(rio,dplyr,lubridate,skimr)
library(data.table)
library(ggplot2)
library(ggmosaic)
library(readr)
library(skimr)
library(plyr)
library(stringr)


data <- import("~/Desktop/Quantium/QVI_data.csv")
head(data)


# Who spends the most on chips 

lifestage_spend = aggregate(data$TOT_SALES, list(lifestage = data$LIFESTAGE), sum)
View(lifestage_spend)
type_spend = aggregate(data$TOT_SALES, list(lifestage = data$PREMIUM_CUSTOMER), sum)
View(type_spend)


# How many customers are in each segment

#as per lifestage 
cust_segment_int = setDT(data)[, .N, by=c("LYLTY_CARD_NBR", "LIFESTAGE")]
cust_segment = setDT(cust_segment_int)[, .N, by=c( "LIFESTAGE")]

View(cust_segment)
sum(cust_segment$N)

#as per customer premium 
cust_segment_int_type = setDT(data)[, .N, by=c("LYLTY_CARD_NBR", "PREMIUM_CUSTOMER")]
cust_segment_type = setDT(cust_segment_int_type)[, .N, by=c( "PREMIUM_CUSTOMER")]
View(cust_segment_type)
sum(cust_segment_type$N)


#combined 


allcust <- data[, .(CUSTOMERS = uniqueN(LYLTY_CARD_NBR)), .(LIFESTAGE, PREMIUM_CUSTOMER)][order(-CUSTOMERS)]
View(allcust)

ggplot(allcust)


plot <- ggplot(allcust, aes(x=LIFESTAGE, y=PREMIUM_CUSTOMER)) +
  geom_mosaic(aes(weight = CUSTOMERS, x = product(PREMIUM_CUSTOMER, LIFESTAGE), fill = PREMIUM_CUSTOMER)) +
  labs(x = "Lifestage", y = "Premium", title = "No. of Customers per segment") 

plot+  geom_text(data = ggplot_build(plot)$data[[1]], aes(x = (xmin + xmax)/2 , y =(ymin + ymax)/2, label = as.character(paste(round(.wt/sum(.wt),3)*100,'%'))))

