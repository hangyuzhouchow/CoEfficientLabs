setwd("~/Desktop")
install.packages("reshape") # only need it for the first time
library(reshape)
library(dplyr)
library(lubridate)
###Load Transaction detail data###
Transaction <- read.csv("1.csv",header = T, na.string = c("","NA", " ")) 

#Change Variable name, plyr:: is used to make rename work after dplyr is loaded
Transaction <- plyr::rename(Transaction, 
                            c(Campaign.ID..LQ. = "Campaign ID",
                              X..CRE...Traditional.Filter = "Traditional",
                              Cohort..PST. = "Date",
                            X1.0.Started.Date = "Starts",
                            X1.0.Prequal.Date = "Prequals",
                            X1.0.Funded.Date = "Funded",
                            Amount..Fixed.by.ID. = "Amount"))

###Load Campaign ID data###
campaignID <- read.csv("campaignIDs.csv", sep = ",")

###Merge two datas in order to get campaign name on Transaction data###
data <- merge(Transaction, campaignID, by.x = "Campaign ID", by.y = "ID",all.x = T)


###Clean the merged data###
data$Date =mdy_hm(data$Date)
data$Amount = as.numeric(gsub(",","",as.character(data$Amount)))

#Due to the format of the file, the first date input here is one day before . 
#Ex: if you want to see the performance between 12/11/2017 and 12/17/2017, 
#you should put 12/10/2017 and 12/17/2017. Don't change the Hour-Min-Sec part.
data1 <- data %>%
  filter(Traditional == "Traditional", 
         Date >= "2018-01-07 16:00:00" & Date <= "2018-01-14 16:00:00") %>%  
  arrange(desc(Date)) %>%
  group_by(`Campaign.Name`) %>%
  summarise(n_starts = n_distinct(Starts),
            n_prequals = n_distinct(Prequals)-1,
            n_funded = n_distinct(Funded)-1,
            n_prequal_amount = sum(as.numeric(Amount[!is.na(Prequals)])))


