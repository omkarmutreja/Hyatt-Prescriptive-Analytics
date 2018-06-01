#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#install.packages('tidyr')
#install.packages('dplyr')
#install.packages('magrittr')
#install.packages('data.table')
#install.packages('shiny')
#install.packages('rsconnect')
library(tidyr)
library(dplyr)
library(magrittr)
library(data.table)
library(ggplot2)
library(sqldf)
library(shiny)
library(rsconnect)


#set directory
setwd("~/Desktop/Shiny")


#Load the Hyatt Dataset for Nov 2014
HyattDataDNov2014 <- read.csv('out-201411.csv')
#Removing irrelevant column
HyattDataNov2014Subset <- HyattDataDNov2014[,-c(2:8,15,24:44,46:53,59:66,68:69,71:75,78:107,110:112,114:125,128:131,133:136,146,148:162,164:166,170,172:181,183:197,228:231,233:237)]
#str(HyattDataNov2014Subset)
#View(HyattDataNov2014Subset)

#Load the Hyatt Dataset for Dec 2014
HyattDataDec2014 <- read.csv('out-201412.csv')
#Removing irrelevant column
HyattDataDec2014Subset <- HyattDataDec2014[,-c(2:8,15,24:44,46:53,59:66,68:69,71:75,78:107,110:112,114:125,128:131,133:136,146,148:162,164:166,170,172:181,183:197,228:231,233:237)]
#str(HyattDataDec2014Subset)
#View(HyattDataDec2014Subset)

#Load the Hyatt Dataset for Jan 2015
HyattData <- read.csv('out-201501.csv',header=TRUE)
##saveRDS(HyattData,"data.rds")
#HyattData1 <- readRDS("data.rds")
HyattData <- HyattData1
#View(HyattData)
#View(HyattData)
#View(HyattData$CHECKOUT_HEADER_ID_C)


#Removing irrelevant column
HyattDataSubset <- HyattData[,-c(2:8,15,24:44,46:53,59:66,68:69,71:75,78:107,110:112,114:125,128:131,133:136,146,148:162,164:166,170,172:181,183:197,228:231,233:237)]
#str(HyattDataSubset)
#View(HyattDataSubset)

# Final Data set of all the 3 months
Hyatt <- rbind(HyattDataNov2014Subset,HyattDataDec2014Subset,HyattDataSubset)

#Taking backup in Hotel
Hotel <- Hyatt
#View(Hotel)
#summary(Hotel)

#Creating a function to calculate NPS of different countries
NPS <- function(x)
{
  pr <- length(which(x=='Promoter'))
  pa <- length(which(x=='Passive'))
  d <- length(which(x=='Detractor'))
  NPS <- ((pr-d)*100)/(pr+pa+d)
  return(NPS)
}
NPS(Hotel$NPS_Type)


summary(Hotel$COUNTRY_CODE_R)

#Identifying which columns have blank values and replace blank values by NA's
for(i in 1:ncol(Hotel))
{
  is.na(Hotel[,i]) <- Hotel[,i]==''
}

#All the empty spaces have been replaced by NA's
#View(Hotel)

#Identifying all the columns which have NA values
colnames(Hotel)[colSums(is.na(Hotel)) > 0]

#Removing rows with NAs in NPS_Type and Likelihood_Recommend_H

remove <- complete.cases(Hotel[, "NPS_Type"])
#View(remove)
Hotel <- Hotel[remove,]
colSums(is.na(Hotel))
#View(Hotel)
#str(Hotel$NPS_Type)

#Calulating NPS for different countries and displaying the result in descending order based on the NPS value
#NPSCountry <- data.frame(tapply(Hotel$NPS_Type,Hotel$Country_PL, NPS))
#View(NPSCountry)
#colnames(NPSCountry) <- c('NPS')
#NPSCountry <- NPSCountry[order(NPSCountry$NPS ,decreasing=TRUE),]
#View(NPSCountry)
#NPSCountry <- NPSCountry[-56,]

#View(Hotel)
#Computation to create bar plot with Country and NPS
#NPSCountry2 <- data.frame(Hotel$Country_PL,NPSCountry)
#View(NPSCountry2)
#aaaa <- sqldf('Select distinct Country_PL from Hotel order by Country_PL')
#colnames(NPSCountry2) <- c('Country','NPSValue')

#Visualization Bar plot Country to NPS - Visualization 01
#visualization_01 <- ggplot(NPSCountry2, aes(x=reorder(Country,-NPSValue), y=NPSValue, fill=NPSValue,alpha=NPSValue)) +
 # geom_bar(stat='identity', position='dodge') + 
  #theme(axis.text.x = element_text(angle = 90, hjust = 1))
#visualization_01

#dd <- melt(NPSCountry2,id='Country')
#View(dd)

# Here, we see that NPS of United Stated and Japan is very low.
# USA - 57.83222
# JAPAN - 45.77743


#Creating a new df on the basis of Country Code (USA)
HotelUsa <- subset(Hotel, Hotel$Country_PL=='United States')
#View(HotelUsa)


#Identifying the state which has highest number of detractors
sqldf("Select count(NPS_TYPE),STATE_PL from HotelUsa where NPS_TYPE='Detractor' group by STATE_PL order by count(NPS_TYPE)")
#California
# Calculate Detractor ratio to choose the state

aaa <- HotelUsa %>% select(State_PL,NPS_Type) %>% count(NPS_Type,State_PL) %>% spread(NPS_Type,n)
#View(aaa)

for(i in 1:nrow(aaa))
{
  Total[i] <- aaa[i,2] + aaa[i,3] + aaa[i,4]
}
#View(Total)
#Convert rows to columns
ccc <- melt(Total)
#View(ccc)
nrow(ccc)
nrow(aaa)
#Combine total to the data frame
aaa <- data.frame(aaa,ccc)
#View(aaa)
aaa <- aaa[,-5] #Remove unnecessary column
#View(aaa)
colnames(aaa) <- c('STATE_PL','Detractor','Passive','Promoter','Total')
#View(aaa)

# Calculating the detractor ratio
for(i in 1:nrow(aaa))
{
  aaa$DetractorRatio[i] <- aaa[i,2]*100/aaa[i,5]
}
#View(aaa)
bbb <- sqldf("Select * from aaa order by DetractorRatio desc")
#View(bbb) #From this view we see that California has a good ratio of about 14.230135% with a total count of 21848

#Creating a new df having State_PL and detractor ratio
newDf <- bbb[,c(1,6)]
#View(newDf)
newDf <- newDf[order(newDf$DetractorRatio,decreasing=TRUE),]
#View(newDf)


#Visualization Statewise Detractor ratio - Visualization 02
visualization_02 <-  ggplot(newDf, aes(x=reorder(STATE_PL,-DetractorRatio), y=DetractorRatio, fill=DetractorRatio,alpha=DetractorRatio)) +
  geom_bar(stat='identity', position='dodge') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#On analysing the graph we selected California 

sqldf("Select count(*) from HotelUsa where State_PL=='California'")  

HotelCalifornia <- subset(HotelUsa, HotelUsa$State_PL=='California')
#View(HotelCalifornia)
#str(HotelCalifornia)

#Identifying the number of hotels in California
NumberOfHotels <- sqldf("Select distinct ENTRY_HOTEL_CODE_R from HotelCalifornia")
#View(NumberOfHotels)


#Identifying hotel which has the maximum number of Detractors
#Creating a new df with Hotel Entry Code and NPS_TYPE
newdf <- HotelCalifornia[,c("ENTRY_HOTEL_CODE_R","NPS_Type")]
#View(newdf)


newdf <- newdf %>% select(NPS_Type,ENTRY_HOTEL_CODE_R) %>% count(NPS_Type,ENTRY_HOTEL_CODE_R) %>% spread(NPS_Type,n)
newdf <- newdf[order(newdf$Detractor,na.last = TRUE,decreasing = TRUE),]
#View(newdf)
ccc <- melt(newdf,id='ENTRY_HOTEL_CODE_R')
#View(ccc)

#Visualization for Hotel wise Detractor - Visualization 03
visualization_03 <- ggplot(ccc,aes(x=reorder(ENTRY_HOTEL_CODE_R,-value),y=value,fill=variable,color=variable,alpha=variable))+
  geom_bar(stat="identity",position ="identity") +
  scale_alpha_manual(values=c(.8, .2,0.5)) + scale_y_log10()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

table(HotelCalifornia$ENTRY_HOTEL_CODE_R=='OMGDS')
#Doing analysis for Hotel Code OMGDS as it has the maximum number of Detractor and Passive customers
HotelCaliforniaOMGDS <- subset(HotelCalifornia, HotelCalifornia$ENTRY_HOTEL_CODE_R=='OMGDS')
#View(HotelCaliforniaOMGDS)

replaceMean <- function(x)
{
  x[is.na(x)]<- round(mean(x,na.rm = TRUE))
  return (x);
}


HotelCaliforniaOMGDS$Overall_Sat_H <- replaceMean(HotelCaliforniaOMGDS$Overall_Sat_H)
HotelCaliforniaOMGDS$Guest_Room_H <- replaceMean(HotelCaliforniaOMGDS$Guest_Room_H)
HotelCaliforniaOMGDS$Tranquility_H <- replaceMean(HotelCaliforniaOMGDS$Tranquility_H)
HotelCaliforniaOMGDS$Customer_SVC_H <- replaceMean(HotelCaliforniaOMGDS$Customer_SVC_H)
HotelCaliforniaOMGDS$Staff_Cared_H <- replaceMean(HotelCaliforniaOMGDS$Staff_Cared_H)
HotelCaliforniaOMGDS$Internet_Sat_H <- replaceMean(HotelCaliforniaOMGDS$Internet_Sat_H)
HotelCaliforniaOMGDS$Check_In_H <- replaceMean(HotelCaliforniaOMGDS$Check_In_H)
HotelCaliforniaOMGDS$F.B_Overall_Experience_H <- replaceMean(HotelCaliforniaOMGDS$F.B_Overall_Experience_H)
#View(HotelCaliforniaOMGDS)

HotelCaliforniaOMGDSCopy <- HotelCaliforniaOMGDS

#First replacing Y -> 1 and N -> 0

HotelCaliforniaOMGDS$OFFER_FLG_R <- ifelse(HotelCaliforniaOMGDS$OFFER_FLG_R=='Y',1,0)
HotelCaliforniaOMGDS$All.Suites_PL <- ifelse(HotelCaliforniaOMGDS$All.Suites_PL=='Y',1,0)
HotelCaliforniaOMGDS$Bell.Staff_PL <- ifelse(HotelCaliforniaOMGDS$Bell.Staff_PL=='Y',1,0)
HotelCaliforniaOMGDS$Boutique_PL <- ifelse(HotelCaliforniaOMGDS$Boutique_PL=='Y',1,0)
HotelCaliforniaOMGDS$Business.Center_PL <- ifelse(HotelCaliforniaOMGDS$Business.Center_PL=='Y',1,0)
HotelCaliforniaOMGDS$Casino_PL <- ifelse(HotelCaliforniaOMGDS$Casino_PL=='Y',1,0)
HotelCaliforniaOMGDS$Conference_PL <- ifelse(HotelCaliforniaOMGDS$Conference_PL=='Y',1,0)
HotelCaliforniaOMGDS$Convention_PL <- ifelse(HotelCaliforniaOMGDS$Convention_PL=='Y',1,0)
HotelCaliforniaOMGDS$Dry.Cleaning_PL <- ifelse(HotelCaliforniaOMGDS$Dry.Cleaning_PL=='Y',1,0)
HotelCaliforniaOMGDS$Elevators_PL <- ifelse(HotelCaliforniaOMGDS$Elevators_PL=='Y',1,0)
HotelCaliforniaOMGDS$Fitness.Center_PL <- ifelse(HotelCaliforniaOMGDS$Fitness.Center_PL=='Y',1,0)
HotelCaliforniaOMGDS$Fitness.Trainer_PL <- ifelse(HotelCaliforniaOMGDS$Fitness.Trainer_PL=='Y',1,0)
HotelCaliforniaOMGDS$Golf_PL <- ifelse(HotelCaliforniaOMGDS$Golf_PL=='Y',1,0)
HotelCaliforniaOMGDS$Indoor.Corridors_PL <- ifelse(HotelCaliforniaOMGDS$Indoor.Corridors_PL=='Y',1,0)
HotelCaliforniaOMGDS$Laundry_PL <- ifelse(HotelCaliforniaOMGDS$Laundry_PL=='Y',1,0)
HotelCaliforniaOMGDS$Limo.Service_PL <- ifelse(HotelCaliforniaOMGDS$Limo.Service_PL=='Y',1,0)
HotelCaliforniaOMGDS$Mini.Bar_PL <- ifelse(HotelCaliforniaOMGDS$Mini.Bar_PL=='Y',1,0)
HotelCaliforniaOMGDS$Pool.Indoor_PL <- ifelse(HotelCaliforniaOMGDS$Pool.Indoor_PL=='Y',1,0)
HotelCaliforniaOMGDS$Pool.Outdoor_PL <- ifelse(HotelCaliforniaOMGDS$Pool.Outdoor_PL=='Y',1,0)
HotelCaliforniaOMGDS$Regency.Grand.Club_PL <- ifelse(HotelCaliforniaOMGDS$Regency.Grand.Club_PL=='Y',1,0)
HotelCaliforniaOMGDS$Resort_PL <- ifelse(HotelCaliforniaOMGDS$Resort_PL=='Y',1,0)
HotelCaliforniaOMGDS$Restaurant_PL <- ifelse(HotelCaliforniaOMGDS$Restaurant_PL=='Y',1,0)
HotelCaliforniaOMGDS$Self.Parking_PL <- ifelse(HotelCaliforniaOMGDS$Self.Parking_PL=='Y',1,0)
HotelCaliforniaOMGDS$Shuttle.Service_PL <- ifelse(HotelCaliforniaOMGDS$Shuttle.Service_PL=='Y',1,0)
HotelCaliforniaOMGDS$Ski_PL <- ifelse(HotelCaliforniaOMGDS$Ski_PL=='Y',1,0)
HotelCaliforniaOMGDS$Spa_PL <- ifelse(HotelCaliforniaOMGDS$Spa_PL=='Y',1,0)
HotelCaliforniaOMGDS$Spa.services.in.fitness.center_PL <- ifelse(HotelCaliforniaOMGDS$Spa.services.in.fitness.center_PL=='Y',1,0)
HotelCaliforniaOMGDS$Spa.online.booking_PL <- ifelse(HotelCaliforniaOMGDS$Spa.online.booking_PL=='Y',1,0)
HotelCaliforniaOMGDS$Spa.F.B.offering_PL <- ifelse(HotelCaliforniaOMGDS$Spa.F.B.offering_PL=='Y',1,0)
HotelCaliforniaOMGDS$Valet.Parking_PL <- ifelse(HotelCaliforniaOMGDS$Valet.Parking_PL=='Y',1,0)

#View(HotelCaliforniaOMGDS)


###########################################DESCRIPTIVE STATS########################################

#Use tapply to see relationships between room type code and likelihood to recommend.
RoomTypeMean <- tapply(HotelCaliforniaOMGDS$Likelihood_Recommend_H, HotelCaliforniaOMGDS$ROOM_TYPE_CODE_C, mean,na.rm=TRUE)
RoomTypeMin <- tapply(HotelCaliforniaOMGDS$Likelihood_Recommend_H, HotelCaliforniaOMGDS$ROOM_TYPE_CODE_C, min,na.rm=TRUE)
RoomTypeMax <- tapply(HotelCaliforniaOMGDS$Likelihood_Recommend_H, HotelCaliforniaOMGDS$ROOM_TYPE_CODE_C, max,na.rm=TRUE)

#View(unique(HotelCaliforniaOMGDS$ROOM_TYPE_CODE_C))

RTypeLike <- data.frame(RoomTypeMean, RoomTypeMin, RoomTypeMax,table(HotelCaliforniaOMGDS$ROOM_TYPE_CODE_C))
RTypeLike <- na.omit(RTypeLike)
RTypeLike <- RTypeLike[order(RTypeLike$RoomTypeMean),]
#View(RTypeLike)

RTypeLikeSubset <- sqldf("Select * from RTypeLike where Freq > 5 and RoomTypeMean < 8")
#View(RTypeLikeSubset)

#Visualization of room type and their feedback. Why? To improve the most used room for better feedback -Visualization 04
visualization_04 <- ggplot(RTypeLikeSubset, aes(x=reorder(Var1,-Freq), y=Freq,  col=RoomTypeMean, fill=RoomTypeMean))+
  geom_bar(stat='identity', position='dodge') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#View(HotelCaliforniaOMGDS)
#Removing unncessary columns
HotelCaliforniaOMGDS <- HotelCaliforniaOMGDS[,-c(2:3,5:14)]
#View(HotelCaliforniaOMGDS)
HotelCaliforniaOMGDS <- HotelCaliforniaOMGDS[,-c(6:9)]
#View(HotelCaliforniaOMGDS)
HotelCaliforniaOMGDS <- HotelCaliforniaOMGDS[,-c(8,9,12)]
#View(HotelCaliforniaOMGDS)
HotelCaliforniaOMGDS <- HotelCaliforniaOMGDS[,-c(23,24,26:29)]
#View(HotelCaliforniaOMGDS)

# Taking backup
backup <- HotelCaliforniaOMGDS

coorect <- HotelCaliforniaOMGDS

HotelCaliforniaOMGDS <- coorect


# backup and correct are extra variables having backup of HotelCaliforniaOMGDS

##  Linear Modelling

#View(HotelCaliforniaOMGDS)

#str(HotelCaliforniaOMGDS)

test<-HotelCaliforniaOMGDS
test1<-coorect

for ( i in 1: ncol(test))
{
  test[,i] <- as.numeric(test[,i])
  i =i+1
  test[is.na(test)]<-""
}

for ( i in 1: ncol(test1))
{
  test1[,i] <- as.numeric(test1[,i])
  i =i+1
  test1[is.na(test1)]<-""
}

#View(test)
#View(test1)

for(i in 1:ncol(test))
{
  test[,i] <- as.numeric(test[,i])
}

for(i in 1:ncol(test1))
{
  test1[,i] <- as.numeric(test1[,i])
}
#str(test1)

#str(test)
test$Condition_Hotel_H <- as.numeric(test$Condition_Hotel_H)
test$Restaurant_PL <- as.numeric(test$Restaurant_PL)


##  Linear Modelling 01 : Start (Likelihood_Recommend_H)

#lm_1 <- lm(Likelihood_Recommend_H ~ ROOM_TYPE_CODE_C + POV_CODE_C + ENTRY_HOTEL_CODE_R + NT_RATE_R + 
 #            MEMBER_STATUS_R + OFFER_FLG_R + Gender_H + Age_Range_H + Clublounge_Used_H +
  #           Spa_Used_H + GP_Tier_H+ NPS_Type+ Overall_Sat_H + Guest_Room_H + Tranquility_H  +
   #          Condition_Hotel_H + Customer_SVC_H + Staff_Cared_H + Internet_Sat_H + Check_In_H + 
    #         F.B_Overall_Experience_H + State_PL + All.Suites_PL  + Bell.Staff_PL + Boutique_PL +
     #        Business.Center_PL + Casino_PL + Conference_PL +
      #       Convention_PL + Dry.Cleaning_PL + Elevators_PL + Fitness.Center_PL + Fitness.Trainer_PL + Golf_PL
       #    + Indoor.Corridors_PL + Laundry_PL + Limo.Service_PL + Mini.Bar_PL+ Pool.Indoor_PL  + Pool.Outdoor_PL 
        #   + Regency.Grand.Club_PL + Resort_PL + Restaurant_PL + Self.Parking_PL + Shuttle.Service_PL + Ski_PL 
         #  + Spa_PL + Spa.services.in.fitness.center_PL + Spa.online.booking_PL + Spa.F.B.offering_PL + Valet.Parking_PL,  
          # data = test)

#summary(lm_1)

##LM_2 - Removed below columns - due to date columns, insignificant data types, no strong business relationship.

## ENTRY_HOTEL_CODE_R + STATE_R + COUNTRY_CODE_R
##  GP_Tier_H + Hotel.Name.Long_PL + City_PL + State_PL + US.Region_PL + Country_PL + Brand_PL + Relationship_PL + Ski_PL


#lm_2 <- lm(Likelihood_Recommend_H ~ POV_CODE_C + NT_RATE_R + 
 #            + MEMBER_STATUS_R + OFFER_FLG_R + Gender_H + Age_Range_H + Clublounge_Used_H
  #         + Spa_Used_H + NPS_Type + Overall_Sat_H + Guest_Room_H + Tranquility_H  + Condition_Hotel_H + 
   #          Customer_SVC_H + Staff_Cared_H + Internet_Sat_H + Check_In_H + F.B_Overall_Experience_H 
    #       + All.Suites_PL  + Bell.Staff_PL + Boutique_PL + Business.Center_PL + Casino_PL + Conference_PL +
     #        Convention_PL + Dry.Cleaning_PL + Elevators_PL + Fitness.Center_PL + Fitness.Trainer_PL + Golf_PL
      #     + Indoor.Corridors_PL + Laundry_PL + Limo.Service_PL + Mini.Bar_PL+ Pool.Indoor_PL  + Pool.Outdoor_PL 
       #    + Regency.Grand.Club_PL + Resort_PL + Restaurant_PL + Self.Parking_PL + Shuttle.Service_PL +
        #     + Spa_PL + Spa.services.in.fitness.center_PL + Spa.online.booking_PL + Spa.F.B.offering_PL + Valet.Parking_PL,  
         #  data = test)

#summary(lm_2)

##LM_3 - Removed below columns - due to insignificant p values of co-efficients

## Spa.F.B.offering_PL, Valet.Parking_PL, Spa.online.booking_PL , Spa.services.in.fitness.center_PL , + Spa_PL,
## Regency.Grand.Club_PL, Pool.Indoor_PL , Pool.Outdoor_PL,  Laundry_PL, Fitness.Center_PL , Fitness.Trainer_PL
## , Golf_PL , Business.Center_PL, All.Suites_PL, Internet_Sat_H + Check_In_H + F.B_Overall_Experience_H ,
##, + Clublounge_Used_H,+ Spa_Used_H

#str(test)

lm_3 <- lm(Likelihood_Recommend_H ~ POV_CODE_C + NT_RATE_R + 
             MEMBER_STATUS_R + OFFER_FLG_R + Gender_H + Age_Range_H + Guest_Room_H 
           + Tranquility_H  + Condition_Hotel_H 
           + Customer_SVC_H + Staff_Cared_H  
           + Bell.Staff_PL + Boutique_PL + Casino_PL + Conference_PL 
           + Convention_PL + Dry.Cleaning_PL + Elevators_PL  
           + Indoor.Corridors_PL + Limo.Service_PL + Mini.Bar_PL 
           + Resort_PL + Restaurant_PL + Self.Parking_PL + Shuttle.Service_PL,data = test)

#summary(lm_3)


## LM_4 - Removed below columns - due to insignificant p values of co-efficients

## POV_CODE_C + NT_RATE_R + MEMBER_STATUS_R + OFFER_FLG_R + Gender_H + Age_Range_H + Tranquility_H,
## Boutique_PL , Indoor.Corridors_PL , Limo.Service_PL, Mini.Bar_PL , Resort_PL,Bell.Staff_PL,
## Conference_PL,Dry.Cleaning_PL,+ Casino_PL, Restaurant_PL

lm_4 <-    lm (Likelihood_Recommend_H ~ POV_CODE_C + MEMBER_STATUS_R + Age_Range_H+ Guest_Room_H+ Tranquility_H
               +Condition_Hotel_H+ Customer_SVC_H + Staff_Cared_H+Restaurant_PL 
               + Guest_Room_H,data = test)

#summary(lm_4)

lm_5 <- lm (Likelihood_Recommend_H ~  Age_Range_H+ Guest_Room_H+ Tranquility_H
            +Condition_Hotel_H+ Customer_SVC_H + Staff_Cared_H+Restaurant_PL 
            + Guest_Room_H,data = test)
summary(lm_5)


#Visualization Linear modeling
temp <- test[,c("Likelihood_Recommend_H",'Age_Range_H',"Guest_Room_H",
                'Tranquility_H','Condition_Hotel_H',"Customer_SVC_H","Staff_Cared_H",
                'Guest_Room_H')]
#View(temp)
tempMelt <- melt(temp, id='Likelihood_Recommend_H')
#View(tempMelt)

#Heat Map - Visualization 05
visualization_05 <- ggplot(tempMelt, aes(x=Likelihood_Recommend_H,y=variable, fill=value)) + geom_tile() + scale_fill_gradient(low="yellow", high = "orange")

#Scatter Plot Visualization 06
visualization_06 <- ggplot(temp, aes(x=Tranquility_H, y=Likelihood_Recommend_H, size=Condition_Hotel_H, col=Customer_SVC_H,alpha=Guest_Room_H,
                                     group=Staff_Cared_H))+geom_jitter(width = 0.5, height = 1) + geom_abline()

## Linear Modelling 01 : End

## Linear Modelling 02 : Start (NPS Type)

lm1 <-    lm(NPS_Type  ~ POV_CODE_C + ENTRY_HOTEL_CODE_R  +  MEMBER_STATUS_R + OFFER_FLG_R + Gender_H + Age_Range_H + Clublounge_Used_H
             + Spa_Used_H + GP_Tier_H+ Guest_Room_H + Tranquility_H  + Condition_Hotel_H + 
               Customer_SVC_H + Staff_Cared_H + Internet_Sat_H + Check_In_H + F.B_Overall_Experience_H 
             + State_PL + All.Suites_PL  + Bell.Staff_PL + Boutique_PL + Business.Center_PL + Casino_PL + Conference_PL +
               Convention_PL + Dry.Cleaning_PL + Elevators_PL + Fitness.Center_PL + Fitness.Trainer_PL + Golf_PL
             + Indoor.Corridors_PL + Laundry_PL + Limo.Service_PL + Mini.Bar_PL+ Pool.Indoor_PL  + Pool.Outdoor_PL 
             + Regency.Grand.Club_PL + Resort_PL + Restaurant_PL + Self.Parking_PL + Shuttle.Service_PL + Ski_PL 
             + Spa_PL + Valet.Parking_PL,  
             data = test1)

#summary(lm1)

##LM2 - Removed below columns - due to date columns, insignificant data types, no strong business relationship.

##RESERVATION_STATUS_C+ CHECK_IN_DATE_C + CHECK_OUT_DATE_C + ENTRY_HOTEL_CODE_R + STATE_R + COUNTRY_CODE_R
##+ ETA_R + FLIGHTT_INFO_R + e_delivereddate_adj_I + e_status_I + GP_Tier_H + Hotel.Name.Long_PL + City_PL + State_PL + US.Region_PL + Country_PL + Brand_PL + Relationship_PL + Ski_PL


lm2 <-     lm(NPS_Type  ~ Guest_Room_H + Tranquility_H  + Fitness.Trainer_PL +
                Customer_SVC_H + Staff_Cared_H ,
              data = test1)

#summary(lm2)

#Visualization Linear modeling -- Visualization 07 - Scatter Plot
visualization_07 <- ggplot(test1, aes(x=Tranquility_H, y=NPS_Type, size=Guest_Room_H, col=Customer_SVC_H,alpha=Fitness.Trainer_PL,
                                      group=Staff_Cared_H))+geom_jitter(width = 0.5, height = 1) 


temp1 <- test[,c("NPS_Type","Guest_Room_H",'Tranquility_H'
                 ,'Customer_SVC_H','Staff_Cared_H')]
#View(temp1)
tempMelt1 <- melt(temp1, id='NPS_Type')
#View(tempMelt1)

#Heat Map - Visualization 08
visualization_08 <- ggplot(tempMelt1, aes(x=NPS_Type,y=variable, fill=value)) + geom_tile() + scale_fill_gradient(low="pink", high = "violet")




##  : Linear Modelling 02 : End

## , Linear Modelling, End


HotelCaliforniaOMGDS <- backup
#HotelCaliforniaOMGDSCopy <- backup

HotelCaliforniaOMGDS$Clublounge_Used_H <- ifelse(HotelCaliforniaOMGDS$Clublounge_Used_H=='Y',1,0)
#View(HotelCaliforniaOMGDS)

# Function to convert numerical (1-10) to NPS categories
trans <- function(x)
{
  x <- ifelse(x==c(1,2,3,4,5,6), "Low", ifelse(x==c(7,8),"Medium","High"))
  
  return (x)
}

HotelCaliforniaOMGDS$Overall_Sat_H <- trans(HotelCaliforniaOMGDS$Overall_Sat_H)
HotelCaliforniaOMGDS$Guest_Room_H <- trans(HotelCaliforniaOMGDS$Guest_Room_H)
HotelCaliforniaOMGDS$Tranquility_H <- trans(HotelCaliforniaOMGDS$Tranquility_H)
HotelCaliforniaOMGDS$Condition_Hotel_H <- trans(HotelCaliforniaOMGDS$Condition_Hotel_H)
HotelCaliforniaOMGDS$Customer_SVC_H <- trans(HotelCaliforniaOMGDS$Customer_SVC_H)
HotelCaliforniaOMGDS$Staff_Cared_H <- trans(HotelCaliforniaOMGDS$Staff_Cared_H)
HotelCaliforniaOMGDS$Internet_Sat_H <- trans(HotelCaliforniaOMGDS$Internet_Sat_H)
HotelCaliforniaOMGDS$Check_In_H <- trans(HotelCaliforniaOMGDS$Check_In_H)
HotelCaliforniaOMGDS$F.B_Overall_Experience_H <- trans(HotelCaliforniaOMGDS$F.B_Overall_Experience_H)

HotelCaliforniaOMGDSCopy <- HotelCaliforniaOMGDS

#View(HotelCaliforniaOMGDSCopy)

#Removing unncessary columns
HotelCaliforniaOMGDSCopy <- HotelCaliforniaOMGDSCopy[,-c(2:3,5:14)]
#View(HotelCaliforniaOMGDSCopy)
HotelCaliforniaOMGDSCopy <- HotelCaliforniaOMGDSCopy[,-c(6:9)]
#View(HotelCaliforniaOMGDSCopy)
HotelCaliforniaOMGDSCopy <- HotelCaliforniaOMGDSCopy[,-c(8,9,12)]
#View(HotelCaliforniaOMGDSCopy)
HotelCaliforniaOMGDSCopy <- HotelCaliforniaOMGDSCopy[,-c(5,23:29)]
#View(HotelCaliforniaOMGDSCopy)
HotelCaliforniaOMGDSCopy <- HotelCaliforniaOMGDSCopy[,-c(47,48,49)]
#View(HotelCaliforniaOMGDSCopy)
HotelCaliforniaOMGDSCopy <- HotelCaliforniaOMGDSCopy[,-c(1,4,6)]
#View(HotelCaliforniaOMGDSCopy)
#str(HotelCaliforniaOMGDSCopy)

# Converting all the variables in factors
for(i in 1:ncol(HotelCaliforniaOMGDSCopy))
{
  HotelCaliforniaOMGDSCopy[,i] <- as.factor(HotelCaliforniaOMGDSCopy[,i])
}

# Taking backup before doing na.omit to implement apriori

backup2 <- HotelCaliforniaOMGDSCopy


#omiting na's
#HotelCaliforniaOMGDSCopy <- na.omit(HotelCaliforniaOMGDSCopy)
#View(HotelCaliforniaOMGDSCopy)


#---------apriori rules--------#

#install.packages("arules")
#library(arules)
#install.packages("arulesViz")
#library(arulesViz)


#new_data <- as(trialdf1,"transactions")

########## Omkar ########
### Apriori ####

# Apriori overall

#aprioriDf <- HotelCaliforniaOMGDSCopy[,c('NPS_Type',"ROOM_TYPE_CODE_C","POV_CODE_C","MEMBER_STATUS_R","Gender_H",
 #                                        "Age_Range_H","Tranquility_H",
  #                                       'Guest_Room_H',"Internet_Sat_H","F.B_Overall_Experience_H", 
   #                                      'Condition_Hotel_H','Customer_SVC_H','Staff_Cared_H',
    #                                     "Casino_PL","Conference_PL","Convention_PL",
     #                                    "Fitness.Center_PL","Laundry_PL","Limo.Service_PL","Mini.Bar_PL",
      #                                   "Restaurant_PL","Spa_PL","Valet.Parking_PL")]

# Converting all the varibales into factors to run apriori function
#for(i in 1:ncol(aprioriDf))
#{
#  aprioriDf[,i] <- as.factor(aprioriDf[,i])
#}

#aprioriDf <- na.omit(aprioriDf)
#View(aprioriDf)

#ruleset <- apriori(aprioriDf, parameter = list(support=0.015,confidence=0.5, target='rules'),
 #                  appearance = list(default='lhs',rhs=('NPS_Type=Detractor')))

#inspect(ruleset)

#importantRules<-ruleset[quality(ruleset)$lift>4.85]
#importantRules
#inspect(importantRules)


#ruleset2 <- apriori(aprioriDf, parameter = list(support=0.5,confidence=0.5, target='rules'),
                   # appearance = list(default='lhs',rhs=('NPS_Type=Promoter')))

#inspect(ruleset2)

#importantRules2<-ruleset2[quality(ruleset2)$lift>1.2]
#importantRules2
#inspect(importantRules2)




#Visualization 09
#plot(ruleset, method="graph");

#plot(importantRules, method="graph", control=list(type="items"));

#visualization_09 <- plot(importantRules, method="grouped");

#aprioriDfTransaction <- as(aprioriDf, 'transactions')
#itemFrequencyPlot(aprioriDfTransaction, support=0.015,col='cyan')


########## Shiny #########


## Shiny App

# Define UI for application that draws a histogram
shinyApp(
  options(shiny.sanitize.errors = FALSE),
  ui <- fluidPage( sidebarLayout(
    sidebarPanel(
      h1("Hyatt Hotel Analysis"),
      actionButton(inputId = "Preprocessing", label = "Data Processing"),
      #actionButton(inputId = "Visualization1", label = "Visualization 1"),
      actionButton(inputId = "Visualization2", label = "State wise Detractor ratio"),
      actionButton(inputId = "Visualization3", label = "Hotel wise NPS"),
      actionButton(inputId = "Visualization4", label = "Room type and their feedback"),
      actionButton(inputId = "Visualization5", label = "Heat Map of Likelihood for LM"),
      actionButton(inputId = "Visualization6", label = "Scatter plot of Likelihood for LM"),
      actionButton(inputId = "Visualization7", label = "Scatter plot of NPS for LM"),
      actionButton(inputId = "Visualization8", label = "Heat Map of NPS for LM")
      #actionButton(inputId = "Visualization9", label = "Visualization 9")
    ),
    mainPanel(
      tableOutput("contents"),
      ## textOutput("contents"),
      textOutput("prepro"),
      tableOutput("contents2"),
      #textOutput("Visualization1content"),
      #plotOutput("Visualization1plot"),
      textOutput("Visualization2content"),
      plotOutput("Visualization2plot"),
      textOutput("Visualization3content"),
      plotOutput("Visualization3plot"),
      textOutput("Visualization4content"),
      plotOutput("Visualization4plot"),
      textOutput("Visualization5content"),
      plotOutput("Visualization5plot"),
      textOutput("Visualization6content"),
      plotOutput("Visualization6plot"),
      textOutput("Visualization7content"),
      plotOutput("Visualization7plot"),
      textOutput("Visualization8content"),
      plotOutput("Visualization8plot")
      #textOutput("Visualization9content"),
      #plotOutput("Visualization9plot")
      
    )
  )
  ),
  
  
  # Define server logic required to draw a histogram
  server <- function(input, output) {
    
    output$contents <- renderTable(head(Hyatt))
    observeEvent(input$Preprocessing,
                 {
                   output$prepro <- renderText("Preprocessed Data : ")
                   output$contents2 <- renderTable(head(HotelCaliforniaOMGDS))
                 }
                 
                 
    )
    
    #observeEvent(input$Visualization1,
                 #{
                  # output$Visualization1content <- renderText("Visualization 1")
                   #output$Visualization1plot <- renderPlot(visualization_01)
                 #})
    
    observeEvent(input$Visualization2,
                 {
                   output$Visualization2content <- renderText("State wise Detractor ratio")
                   output$Visualization2plot <- renderPlot(visualization_02)
                 })
    observeEvent(input$Visualization3,
                 {
                   output$Visualization3content <- renderText("Hotel wise NPS")
                   output$Visualization3plot <- renderPlot(visualization_03)
                 })
    observeEvent(input$Visualization4,
                 {
                   output$Visualization4content <- renderText("Room type and their feedback")
                   output$Visualization4plot <- renderPlot(visualization_04)
                 })
    observeEvent(input$Visualization5,
                 {
                   output$Visualization5content <- renderText("Heap map of Likelihood for LM")
                   output$Visualization5plot <- renderPlot(visualization_05)
                 })
    observeEvent(input$Visualization6,
                 {
                   output$Visualization6content <- renderText("Scatter plot of Likelihood for LM")
                   output$Visualization6plot <- renderPlot(visualization_06)
                 })
    observeEvent(input$Visualization7,
                 {
                   output$Visualization7content <- renderText("Scatter plot of NPS for LM")
                   output$Visualization7plot <- renderPlot(visualization_07)
                 })
    observeEvent(input$Visualization8,
                 {
                   output$Visualization8content <- renderText("Heat map of NPS for LM")
                   output$Visualization8plot <- renderPlot(visualization_08)
                 })
    #observeEvent(input$Visualization9,
     #            {
      #             output$Visualization9content <- renderText("Visualization 9")
       #            output$Visualization9plot <- renderPlot(visualization_09)
        #         })
    
  }
  
)

# Run the application 
shinyApp(ui = ui, server = server)