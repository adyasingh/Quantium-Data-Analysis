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

# How many chips are bought per customer by segment

#as per lifestage per customer 

qty_lifestage_int = setDT(data)[, .N, by=c("PROD_QTY", "LIFESTAGE")]
View(qty_lifestage)
qty_lifestage= setDT(qty_lifestage_int)[, .N, by=c ("LIFESTAGE")]

lifestageSpend=NULL
for (stage in cust_segment$LIFESTAGE) {
  tmp = data[data$LIFESTAGE==stage]
  print(stage)
  sum_prod = (sum(tmp$PROD_QTY))
  sub = subset(cust_segment, LIFESTAGE==stage)
  average = sum_prod/sub$N
  
  lifestageSpend = rbind(lifestageSpend, data.frame(stage,sum_prod, average))
  
}

#Average number of units per customer by LIFESTAGE
View(lifestageSpend)


ggplot(lifestageSpend, aes(x=stage, y=average)) +
  geom_bar(stat="identity", width=0.5)+
  labs(x = "Lifestage", y = "Avg Units", title = "Avg units per customer per lifestage") 


#as per customer premium per customer 


qty_premium_int = setDT(data)[, .N, by=c("PROD_QTY", "PREMIUM_CUSTOMER")]
qty_premium= setDT(qty_premium_int)[, .N, by=c ("PREMIUM_CUSTOMER")]
View(qty_premium)

premiumSpend=NULL
for (premium in cust_segment_type$PREMIUM_CUSTOMER) {
  tmp = data[data$PREMIUM_CUSTOMER==premium]
  print(premium)
  sum_prod = (sum(tmp$PROD_QTY))
  sub = subset(cust_segment_type, PREMIUM_CUSTOMER==premium)
  average = sum_prod/sub$N
  
  premiumSpend = rbind(premiumSpend, data.frame(premium,sum_prod, average))
  
}
View(premiumSpend)


#Average number of units per customer by PREMIUM


ggplot(premiumSpend, aes(x=premium, y=average)) +
  geom_bar(stat="identity", width=0.5)+
  labs(x = "Premium Customer", y = "Avg Units", title = "Avg units per customer per premium") 

#cross check
sum(data$PROD_QTY)
sum(premiumSpend$sum_prod)
sum(lifestageSpend$sum_prod)


## Combined - units per lifestage per premium 

units <- data[, .(AVG = sum(PROD_QTY)/uniqueN(LYLTY_CARD_NBR)), .(LIFESTAGE, PREMIUM_CUSTOMER)][order(-AVG)]

ggplot(data = units, aes(weight = AVG, x = LIFESTAGE, fill = PREMIUM_CUSTOMER)) +
  geom_bar(position = position_dodge()) +
  labs(x = "Lifestage", y = "Avg units per transaction", title = "Units per customer") +
  
  
  
  #### Number of customers by LIFESTAGE
  View(cust_segment)
ggplot(cust_segment, aes(x=LIFESTAGE, y=N)) +
  geom_bar(stat="identity", width=0.5)+
  labs(x = "Lifestage", y = "Number of customers", title = "No. of customers per lifestage") 

#### Number of customers by PREMIUM_CUSTOMER
View(cust_segment_type)
ggplot(cust_segment_type, aes(x=PREMIUM_CUSTOMER, y=N)) +
  geom_bar(stat="identity", width=0.5)+
  labs(x = "Premium Customer", y = "Number of customers", title = "No. of customers per premium") 

#cross check
uniqueCust  = setDT(data)[, .N, by=c("LYLTY_CARD_NBR")]
nrow(uniqueCust)
sum(cust_segment$N)
sum(cust_segment_type$N) 

#SPEND Amount per lifestage 


spend <-aggregate(data$TOT_SALES, by=list(data$LIFESTAGE),
                  FUN=sum, na.rm=TRUE)
View(spend)
colnames(spend)[1] <- "LIFESTAGE"
colnames(spend)[2] <- "SALE"

spend$PER_CUSTOMER = spend$SALE/cust_segment[cust_segment$N, cust_segment$LIFESTAGE==spend$LIFESTAGE]

View(lifestageSpend)

lifestageMoney=NULL

for (stg in lifestageSpend$stage) {
  tmp = data[data$LIFESTAGE==stg]
  print(stg)
  sumMoney = (sum(tmp$TOT_SALES))
  sub = subset(lifestageSpend, stage==stg)
  average = sumMoney/sub$sum_prod
  print(average)
  lifestageMoney = rbind(lifestageMoney, data.frame(stg,sumMoney, average))
  
}

View(lifestageMoney)
View(cust_segment)

ggplot(spend, aes(x=LIFESTAGE, y=SALE)) +
  geom_bar(stat="identity", width=0.5)+
  labs(x = "Lifestage", y = "Total Sales", title = "Sales per lifestage") 

ggplot(lifestageMoney, aes(x=stg, y=average)) +
  geom_bar(stat="identity", width=0.5)+
  labs(x = "Lifestage", y = "Total Sales", title = "Avg. sales per lifestage per customer per unit") 


#cross check
sum(spend$SALE)
sum(data$TOT_SALES)

#SPEND Amount per premium 


premium_spend <-aggregate(data$TOT_SALES, by=list(data$PREMIUM_CUSTOMER),
                          FUN=sum, na.rm=TRUE)
View(premium_spend)
colnames(premium_spend)[1] <- "PREMIUM_CUSTOMER"
colnames(premium_spend)[2] <- "SALE"



premiumMoney=NULL
for (prem in premiumSpend$premium) {
  tmp = data[data$PREMIUM_CUSTOMER==prem]
  print(prem)
  #print(tmp)
  sum_prod = (sum(tmp$TOT_SALES))
  print(sum_prod)
  sub = subset(premiumSpend, premium==prem)
  average = sum_prod/sub$sum_prod
  print(average)
  premiumMoney = rbind(premiumMoney, data.frame(prem,sum_prod, average))
  
}
View(premiumMoney)

ggplot(premium_spend, aes(x=PREMIUM_CUSTOMER, y=SALE)) +
  geom_bar(stat="identity", width=0.5)+
  labs(x = "Premium", y = "Total Sales", title = "Sales per premium") 

ggplot(premiumMoney, aes(x=prem, y=average)) +
  geom_bar(stat="identity", width=0.5)+
  labs(x = "Premium", y = "Total Sales", title = "Avg. sales per premium per customer per unit") 


#cross check
sum(premium_spend$SALE)
sum(data$TOT_SALES)



#### Average price per unit by LIFESTAGE and PREMIUM_CUSTOMER
priceUnit <- data[, .(AVG = sum(TOT_SALES)/sum(PROD_QTY)), .(LIFESTAGE,PREMIUM_CUSTOMER)][order(-AVG)]
#### Create plot
ggplot(data = priceUnit, aes(weight = AVG, x = LIFESTAGE, fill =PREMIUM_CUSTOMER)) +
  geom_bar(position = position_dodge()) +
  labs(x = "Lifestage", y = "Avg price per unit", title = "Price per unit") 
#### Perform an independent t-test between mainstream vs premium and budget


pricePerUnit <- data[, price := TOT_SALES/PROD_QTY]
t.test(data[LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES") & PREMIUM_CUSTOMER == "Mainstream", price]
       , data[LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES") & PREMIUM_CUSTOMER != "Mainstream", price]
       , alternative = "greater")

#### Deep dive into Mainstream, young singles/couples

young_mainstream <-subset(data, PREMIUM_CUSTOMER=="Mainstream"  & LIFESTAGE=="YOUNG SINGLES/COUPLES")

summary(young_mainstream$PROD_QTY)

agg_popular<- aggregate(young_mainstream$PROD_QTY, list(BRAND = young_mainstream$BRAND), sum)
agg_popular <-agg_popular[order(-agg_popular$x),]

View(agg_popular)
View(young_mainstream)

agg_pkt<- aggregate(young_mainstream$PROD_QTY, list(PACK_SIZE = young_mainstream$PACK_SIZE), sum)
agg_pkt <-agg_pkt[order(-agg_pkt$x),]

View(agg_pkt)

#cross check 
sum(agg_pkt$x)
sum(young_mainstream$PROD_QTY)
sum(agg_popular$x)

#brand affinity 
young_mainstream <-subset(data, PREMIUM_CUSTOMER=="Mainstream"  & LIFESTAGE=="YOUNG SINGLES/COUPLES")
other <-subset(data, !(PREMIUM_CUSTOMER=="Mainstream"  & LIFESTAGE=="YOUNG SINGLES/COUPLES"))

qty_young <- young_mainstream[, sum(PROD_QTY)]
qty_other <- other[, sum(PROD_QTY)]


qty_young_brand <- young_mainstream[, .(targetSegment = sum(PROD_QTY)/qty_young), by = BRAND]
qty_other_brand <- other[, .(other = sum(PROD_QTY)/qty_other), by = BRAND]
brand_proportions <- merge(qty_young_brand, qty_other_brand)[, affinityToBrand := targetSegment/other]
brand_proportions[order(-affinityToBrand)]


#### Preferred pack size compared to the rest of the population
qty_young_pack <- young_mainstream[, .(targetSegment = sum(PROD_QTY)/qty_young), by = PACK_SIZE]
qty_other_pack <- other[, .(other = sum(PROD_QTY)/qty_other), by =PACK_SIZE]
pack_proportions <- merge(qty_young_pack, qty_other_pack)[, affinityToPack := targetSegment/other]
pack_proportions[order(-affinityToPack)]

data[PACK_SIZE == 270, unique(PROD_NAME)]
