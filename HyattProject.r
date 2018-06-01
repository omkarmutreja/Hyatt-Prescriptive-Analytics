#Hotel Data Analysis
#IST 687 M002 Team 2
#Team members: Jeni Adenshia,Cordell Hudson,Omkar Mutreja,Romil Shroff 

#install.packages('tidyr')
#install.packages('dplyr')
#install.packages('magrittr')
#install.packages('data.table')
#install.packages('shiny')
#install.packages('rsconnect')
#install.packages('rworldmap')
library(tidyr)
library(dplyr)
library(magrittr)
library(data.table)
library(ggplot2)
library(sqldf)
library(shiny)
library(rsconnect)
library(rworldmap)


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

##### MAP ########

NPSCountry2$Country_PL<- gsub("([a-z])([A-Z])", "\\1 \\2",NPSCountry2$Country_PL)
Worldmap <- joinCountryData2Map(NPSCountry2, joinCode="NAME",
                                nameJoinColumn="Country_PL")
mapCountryData(Worldmap, nameColumnToPlot="NPSCountry",colourPalette = "topo",catMethod="fixedWidth")



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
NPSCountry <- data.frame(tapply(Hotel$NPS_Type,Hotel$Country_PL, NPS))
View(NPSCountry)
colnames(NPSCountry) <- c('NPS')
NPSCountry <- NPSCountry[order(NPSCountry$NPS ,decreasing=TRUE),]
View(NPSCountry)
NPSCountry <- NPSCountry[-56,]

#Computation to create bar plot with Country and NPS
NPSCountry2 <- data.frame(Hotel$Country_PL,NPSCountry)
View(NPSCountry2)
aaaa <- sqldf('Select distinct Country_PL from Hotel order by Country_PL')
colnames(NPSCountry2) <- c('Country','NPSValue')

#Visualization Bar plot Country to NPS - Visualization 01
visualization_01 <- ggplot(NPSCountry2, aes(x=reorder(Country,-NPSValue), y=NPSValue, fill=NPSValue,alpha=NPSValue)) +
  geom_bar(stat='identity', position='dodge') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

dd <- melt(NPSCountry2,id='Country')
View(dd)

# Here, we see that NPS of United Stated is very low.
# USA - 57.83222



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

##########################DESCRIPTIVE STATS##################################

#### Pie Chart USA #####

USA_Pro <- length(HotelUsa$NPS_Type[Hotel$NPS_Type =="Promoter"])
USA_DP <- length(HotelUsa$NPS_Type[Hotel$NPS_Type =="Detractor"])
USA_Pass <- length(HotelUsa$NPS_Type[Hotel$NPS_Type =="Passive"])


a <- c(USA_Pro, USA_DP,USA_Pass)
label <- c("Promoter","Detractor", "Passive")

pct <- round(a/sum(a)*100)
label <- paste(label,pct) #add percentages to labels
label<-paste(label,"%",sep="")

pie(a,labels=label,main = "USA" ,col = rainbow(length(a)))



#### Pie Chart California #####

Cali_Pro <- length(Hotel$NPS_Type[HotelCalifornia$NPS_Type =="Promoter"])
Cali_DP <- length(Hotel$NPS_Type[HotelCalifornia$NPS_Type =="Detractor"])
Cali_Pass <- length(Hotel$NPS_Type[HotelCalifornia$NPS_Type =="Passive"])


a <- c(Cali_Pro, Cali_DP,Cali_Pass)
label <- c("Promoter","Detractor", "Passive")

pct <- round(a/sum(a)*100)
label <- paste(label,pct) #add percentages to labels
label<-paste(label,"%",sep="")

pie(a,labels=label,main = "California" ,col = rainbow(length(a)))


#### Pie Chart OMGDS #####

OMGDS_Pro <- length(Hotel$NPS_Type[HotelCaliforniaOMGDS$NPS_Type =="Promoter"])
OMGDS_DP <- length(Hotel$NPS_Type[HotelCaliforniaOMGDS$NPS_Type =="Detractor"])
OMGDS_Pass <- length(Hotel$NPS_Type[HotelCaliforniaOMGDS$NPS_Type =="Passive"])


a <- c(OMGDS_Pro, OMGDS_DP,OMGDS_Pass)
label <- c("Promoter","Detractor", "Passive")

pct <- round(a/sum(a)*100)
label <- paste(label,pct) #add percentages to labels
label<-paste(label,"%",sep="")

pie(a,labels=label,main = "OMGDS" ,col = rainbow(length(a)))


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

#Staff cared
#Use tapply to see relationships between facilities and likelihood to recommend.
TypeMean <- tapply(HotelCaliforniaOMGDS$Likelihood_Recommend_H, HotelCaliforniaOMGDS$Staff_Cared_H, mean,na.rm=TRUE)
TypeMin <- tapply(HotelCaliforniaOMGDS$Likelihood_Recommend_H, HotelCaliforniaOMGDS$ROOM_TYPE_CODE_C, min,na.rm=TRUE)
TypeMax <- tapply(HotelCaliforniaOMGDS$Likelihood_Recommend_H, HotelCaliforniaOMGDS$ROOM_TYPE_CODE_C, max,na.rm=TRUE)


RTypeLike <- data.frame(TypeMean, TypeMin, TypeMax,table(HotelCaliforniaOMGDS$Staff_Cared_H))
RTypeLike <- na.omit(RTypeLike)
RTypeLike <- RTypeLike[order(RTypeLike$TypeMean),]
View(RTypeLike)

RTypeLikeSubset <- sqldf("Select * from RTypeLike where Freq > 5 and TypeMean < 8")
View(RTypeLikeSubset)

#Visualization of facilities and their feedback. Why? To improve the most used room for better feedback -Visualization 04
ggplot(RTypeLikeSubset, aes(x=reorder(Var1,-Freq), y=Freq,  col=TypeMean, fill=TypeMean))+
  geom_bar(stat='identity', position='dodge') + labs(x="Staff cared", y="Likelihood to recommend")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Purpose of visit
#Use tapply to see relationships between facilities and likelihood to recommend.
TypeMean <- tapply(HotelCaliforniaOMGDS$Likelihood_Recommend_H, HotelCaliforniaOMGDS$POV_CODE_C, mean,na.rm=TRUE)
TypeMin <- tapply(HotelCaliforniaOMGDS$Likelihood_Recommend_H, HotelCaliforniaOMGDS$POV_CODE_C, min,na.rm=TRUE)
TypeMax <- tapply(HotelCaliforniaOMGDS$Likelihood_Recommend_H, HotelCaliforniaOMGDS$POV_CODE_C, max,na.rm=TRUE)


RTypeLike <- data.frame(TypeMean, TypeMin, TypeMax,table(HotelCaliforniaOMGDS$POV_CODE_C))
RTypeLike <- na.omit(RTypeLike)
RTypeLike <- RTypeLike[order(RTypeLike$TypeMean),]
View(RTypeLike)

RTypeLikeSubset <- sqldf("Select * from RTypeLike where Freq > 5 and TypeMean < 8.5")
View(RTypeLikeSubset)

#Visualization of facilities and their feedback. Why? To improve the most used room for better feedback -Visualization 04
ggplot(RTypeLikeSubset, aes(x=reorder(Var1,-Freq), y=Freq,  col=TypeMean, fill=TypeMean))+
  geom_bar(stat='identity', position='dodge') + labs(x="Purpose of visit", y="Likelihood to recommend")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Internet
#Use tapply to see relationships between facilities and likelihood to recommend.
TypeMean <- tapply(HotelCaliforniaOMGDS$Likelihood_Recommend_H, HotelCaliforniaOMGDS$Internet_Sat_H, mean,na.rm=TRUE)
TypeMin <- tapply(HotelCaliforniaOMGDS$Likelihood_Recommend_H, HotelCaliforniaOMGDS$Internet_Sat_H, min,na.rm=TRUE)
TypeMax <- tapply(HotelCaliforniaOMGDS$Likelihood_Recommend_H, HotelCaliforniaOMGDS$Internet_Sat_H, max,na.rm=TRUE)


RTypeLike <- data.frame(TypeMean, TypeMin, TypeMax,table(HotelCaliforniaOMGDS$Internet_Sat_H))
RTypeLike <- na.omit(RTypeLike)
RTypeLike <- RTypeLike[order(RTypeLike$TypeMean),]
View(RTypeLike)

RTypeLikeSubset <- sqldf("Select * from RTypeLike where Freq > 5 and TypeMean < 8.5")
View(RTypeLikeSubset)

#Visualization of facilities and their feedback. Why? To improve the most used room for better feedback -Visualization 04
ggplot(RTypeLikeSubset, aes(x=reorder(Var1,-Freq), y=Freq,  col=TypeMean, fill=TypeMean))+
  geom_bar(stat='identity', position='dodge') + labs(x="Internet", y="Likelihood to recommend")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Condition of the Hotel
#Use tapply to see relationships between facilities and likelihood to recommend.
TypeMean <- tapply(HotelCaliforniaOMGDS$Likelihood_Recommend_H, HotelCaliforniaOMGDS$Condition_Hotel_H, mean,na.rm=TRUE)
TypeMin <- tapply(HotelCaliforniaOMGDS$Likelihood_Recommend_H, HotelCaliforniaOMGDS$Condition_Hotel_H, min,na.rm=TRUE)
TypeMax <- tapply(HotelCaliforniaOMGDS$Likelihood_Recommend_H, HotelCaliforniaOMGDS$Condition_Hotel_H, max,na.rm=TRUE)


RTypeLike <- data.frame(TypeMean, TypeMin, TypeMax,table(HotelCaliforniaOMGDS$Condition_Hotel_H))
RTypeLike <- na.omit(RTypeLike)
RTypeLike <- RTypeLike[order(RTypeLike$TypeMean),]
View(RTypeLike)

RTypeLikeSubset <- sqldf("Select * from RTypeLike where Freq > 5 and TypeMean < 8.5")
View(RTypeLikeSubset)

#Visualization of facilities and their feedback. Why? To improve the most used room for better feedback -Visualization 04
ggplot(RTypeLikeSubset, aes(x=reorder(Var1,-Freq), y=Freq,  col=TypeMean, fill=TypeMean))+
  geom_bar(stat='identity', position='dodge') + labs(x="Hotel Condition", y="Likelihood to recommend")+
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



############################## Linear Modeling ################################################

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
# MEMBER_STATUS_R + OFFER_FLG_R + Gender_H + Age_Range_H + Clublounge_Used_H +
#Spa_Used_H + GP_Tier_H+ NPS_Type+ Overall_Sat_H + Guest_Room_H + Tranquility_H  +
#Condition_Hotel_H + Customer_SVC_H + Staff_Cared_H + Internet_Sat_H + Check_In_H + 
#F.B_Overall_Experience_H + State_PL + All.Suites_PL  + Bell.Staff_PL + Boutique_PL +
# Business.Center_PL + Casino_PL + Conference_PL +
#Convention_PL + Dry.Cleaning_PL + Elevators_PL + Fitness.Center_PL + Fitness.Trainer_PL + Golf_PL
#+ Indoor.Corridors_PL + Laundry_PL + Limo.Service_PL + Mini.Bar_PL+ Pool.Indoor_PL  + Pool.Outdoor_PL 
#+ Regency.Grand.Club_PL + Resort_PL + Restaurant_PL + Self.Parking_PL + Shuttle.Service_PL + Ski_PL 
#+ Spa_PL + Spa.services.in.fitness.center_PL + Spa.online.booking_PL + Spa.F.B.offering_PL + Valet.Parking_PL,  
#data = test)

#summary(lm_1)

##LM_2 - Removed below columns - due to date columns, insignificant data types, no strong business relationship.

## ENTRY_HOTEL_CODE_R + STATE_R + COUNTRY_CODE_R
##  GP_Tier_H + Hotel.Name.Long_PL + City_PL + State_PL + US.Region_PL + Country_PL + Brand_PL + Relationship_PL + Ski_PL


#lm_2 <- lm(Likelihood_Recommend_H ~ POV_CODE_C + NT_RATE_R + 
# + MEMBER_STATUS_R + OFFER_FLG_R + Gender_H + Age_Range_H + Clublounge_Used_H
#+ Spa_Used_H + NPS_Type + Overall_Sat_H + Guest_Room_H + Tranquility_H  + Condition_Hotel_H + 
#   Customer_SVC_H + Staff_Cared_H + Internet_Sat_H + Check_In_H + F.B_Overall_Experience_H 
# + All.Suites_PL  + Bell.Staff_PL + Boutique_PL + Business.Center_PL + Casino_PL + Conference_PL +
#   Convention_PL + Dry.Cleaning_PL + Elevators_PL + Fitness.Center_PL + Fitness.Trainer_PL + Golf_PL
# + Indoor.Corridors_PL + Laundry_PL + Limo.Service_PL + Mini.Bar_PL+ Pool.Indoor_PL  + Pool.Outdoor_PL 
# + Regency.Grand.Club_PL + Resort_PL + Restaurant_PL + Self.Parking_PL + Shuttle.Service_PL +
# + Spa_PL + Spa.services.in.fitness.center_PL + Spa.online.booking_PL + Spa.F.B.offering_PL + Valet.Parking_PL,  
# data = test)

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
            ,data = test)
summary(lm_5)


#Visualization Linear modeling
temp <- test[,c("Likelihood_Recommend_H",'Age_Range_H',"Guest_Room_H",
                'Tranquility_H','Condition_Hotel_H',"Customer_SVC_H","Staff_Cared_H")]
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
#visualization_08



##  : Linear Modelling 02 : End

## , Linear Modelling, End

#### Apriori #####

# Converting all the variables in factors
for(i in 1:ncol(HotelCaliforniaOMGDSCopy))
{
  HotelCaliforniaOMGDSCopy[,i] <- as.factor(HotelCaliforniaOMGDSCopy[,i])
}

str(HotelCaliforniaOMGDSCopy)

HotelCaliforniaOMGDSCopy <- HotelCaliforniaOMGDSCopy[,-c(10,11,18)]

ksvmbackup <- HotelCaliforniaOMGDSCopy

# Taking backup before doing na.omit to implement apriori

backup2 <- HotelCaliforniaOMGDSCopy


#omiting na's
#HotelCaliforniaOMGDSCopy <- na.omit(HotelCaliforniaOMGDSCopy)
#View(HotelCaliforniaOMGDSCopy)


#---------apriori rules--------#

install.packages("arules")
library(arules)
install.packages("arulesViz")
library(arulesViz)


#new_data <- as(trialdf1,"transactions")

### Apriori ####

# Apriori overall
View(aprioriDf)
aprioriDf <- HotelCaliforniaOMGDSCopy

# Converting all the varibales into factors to run apriori function
for(i in 1:ncol(aprioriDf))
{
  aprioriDf[,i] <- as.factor(aprioriDf[,i])
}

aprioriDf <- na.omit(aprioriDf)
View(aprioriDf)

ruleset <- apriori(aprioriDf, parameter = list(support=0.01,confidence=0.3, target='rules'),
                   appearance = list(default='lhs',rhs=('NPS_Type=Detractor')))

inspect(ruleset)

importantRules<-ruleset[quality(ruleset)$lift>5.1]
importantRules
inspect(importantRules)


ruleset2 <- apriori(aprioriDf, parameter = list(support=0.5,confidence=0.5, target='rules'),
                    appearance = list(default='lhs',rhs=('NPS_Type=Promoter')))

inspect(ruleset2)

importantRules2<-ruleset2[quality(ruleset2)$lift>1.15]
importantRules2
inspect(importantRules2)

#Visualization 09
#plot(ruleset, method="graph");

plot(importantRules, method="graph", control=list(type="items"));

aprioriDfTransaction <- as(aprioriDf, 'transactions')
itemFrequencyPlot(aprioriDfTransaction, support=0.015,col='cyan')

##### KSVM  ######
View(HotelCaliforniaOMGDSCopy)
test <- HotelCaliforniaOMGDSCopy
View(test)
str(test)
#View(backup2)

test$Convention_PL <- ifelse(test$Convention_PL==1,'Y','N')
test$Golf_PL <- ifelse(test$Golf_PL==1,'Y','N')
test$Laundry_PL <- ifelse(test$Laundry_PL==1,'Y','N')
test$Limo.Service_PL <- ifelse(test$Limo.Service_PL==1,'Y','N')
test$Mini.Bar_PL <- ifelse(test$Mini.Bar_PL==1,'Y','N')
test$Restaurant_PL <- ifelse(test$Restaurant_PL==1,'Y','N')
test$Spa_PL <- ifelse(test$Spa_PL==1,'Y','N')
test$Valet.Parking_PL <- ifelse(test$Valet.Parking_PL==1,'Y','N')

View(test)



#Removing irrelevant columns
#str(test)
#test <- test[,-c(1,4,6,10,11,18,40,41,44)]
#test <- test[,-c(2,4,5,6,13,14,16,21:23,25,29:31)]

#backup3Ksvm <- test 
View(Ksvm)

test <- na.omit(test)
View(test)
str(test)

str(Ksvm)

ksvmNew <- Ksvm[,c(78,36,40,37,54,73,64,34,35,59,77)]
View(ksvmNew)
ksvmNew <- na.omit(ksvmNew)

ksvmNew$NPS_Type <- ifelse(ksvmNew$NPS_Type=='Promoter',1,ifelse(ksvmNew$NPS_Type=='Detractor',3,2))
View(ksvmNew)

ksvmNew$Conference_PL <- ifelse(ksvmNew$Conference_PL=='Y',1,0)
ksvmNew$Spa_PL <- ifelse(ksvmNew$Spa_PL=='Y',1,0)
ksvmNew$Mini.Bar_PL <- ifelse(ksvmNew$Mini.Bar_PL=='Y',1,0)
ksvmNew$Fitness.Trainer_PL <- ifelse(ksvmNew$Fitness.Trainer_PL=='Y',1,0)

testTemp <- ksvmNew
str(testTemp)
View(testTemp)

for(i in 1:ncol(testTemp))
{
  testTemp[,i] <- as.factor(testTemp[,i])
}



#Train and Test
randIndex <- sample(1:dim(testTemp)[1])
randIndex

# Training Data set
train_cutpoint2_3 <- floor((2*dim(testTemp)[1])/3)
train_cutpoint2_3

trainData <- testTemp[randIndex[1:train_cutpoint2_3],]
dim(trainData)

# Testing Data set
test_cutpoint <- dim(testTemp)[1]-(train_cutpoint2_3+1)
test_cutpoint

testData <- testTemp[randIndex[train_cutpoint2_3+1:test_cutpoint],]
dim(testData)


View(trainData)


#Build a model using the ksvm function.

ksvmOutput <- ksvm(NPS_Type ~ Condition_Hotel_H+Check_In_H+Customer_SVC_H+Guest_Room_H+Tranquility_H+Valet.Parking_PL
                   , data=trainData, kernel="rbfdot", kpar= "automatic", C=10, cross=10, prob.model=TRUE)

ksvmOutput

# Testing on test data
ksvmOutputTest <- ksvm(NPS_Type ~ Condition_Hotel_H+Check_In_H+Customer_SVC_H+Guest_Room_H+Tranquility_H+Valet.Parking_PL 
                       , data=testData, kernel="rbfdot", kpar= "automatic", C=10, cross=10, prob.model=TRUE)
ksvmOutputTest


#Test the model on the testing dataset.

svmPred <- predict(ksvmOutputTest, testData)
View(svmPred)
head(svmPred)
str(svmPred)
View(testData)


#Make data frame with the two results to compare.
comp <- data.frame(testData[,1], svmPred)
colnames(comp) <- c("test", "pred")
colnames(comp)
table(comp)



#### KSVM end ####

#################################################################################################



