# IST 687 Final Project
# Student name: Prathik Prabhakara Reddy

dev.off()
cat("\014")
rm(list=ls())


library(tidyverse)
library(jsonlite)
library(dplyr)
library(ggplot2)

#Reading the data
airlinedata<-"C:/Users/prath/Desktop/Syracuse/Syllabus and Material/IST 687(DS)/Project/fall2019-survey-M07.json"
airlinedf<-jsonlite::fromJSON(airlinedata)
View(airlinedf)
str(airlinedf)
summary(airlinedf)


#PART1: Data munging/Cleaning the data ##############################################################################################################################################

colnames(airlinedf) <- airlinedf %>% colnames() %>% str_replace_all("\\.","_")
names(airlinedf)[names(airlinedf) == "Likelihood_to_recommend"] <- 'Satisfaction'
airlinedf$Satisfaction <- as.numeric(airlinedf$Satisfaction)
#Replacing NAs with blank values for free text
airlinedf$freeText[is.na(airlinedf$freeText)] <- ""


#Dividing Customers based on Satisfaction 
hist(airlinedf$Satisfaction)
airlinedf$Satisfaction_Group <- replicate(dim(airlinedf)[1],"Average")
airlinedf$Satisfaction_Group[airlinedf$Satisfaction>8] <- "High"
airlinedf$Satisfaction_Group[airlinedf$Satisfaction<7]<-"Low"
airlinedf$Satisfaction_Group <- as.factor(airlinedf$Satisfaction_Group)

#Dividing Customers based on Age Group
airlinedf$Age_Group <- airlinedf$Age
airlinedf$Age_Group[which(airlinedf$Age<=15)] <- "0-15"
airlinedf$Age_Group[which(airlinedf$Age>15 & airlinedf$Age<=25)] <- "16-25"
airlinedf$Age_Group[which(airlinedf$Age>25 & airlinedf$Age<=35)] <- "26-35"
airlinedf$Age_Group[which(airlinedf$Age>35 & airlinedf$Age<=45)] <- "36-45"
airlinedf$Age_Group[which(airlinedf$Age>45 & airlinedf$Age<=55)] <- "46-55"
airlinedf$Age_Group[which(airlinedf$Age>55 & airlinedf$Age<=65)] <- "56-65"
airlinedf$Age_Group[which(airlinedf$Age>65 & airlinedf$Age<=75)] <- "66-75"
airlinedf$Age_Group[which(airlinedf$Age>75)] <- "75<"

#We use sapply() to check the number of missing values in each columns.
sapply(airlinedf, function(x) sum(is.na(x)))
           #Departure.Delay.in.Minutes : 225
           #Arrival.Delay.in.Minutes   : 259
           #Flight.time.in.minutes     : 259
           #freeText                   : 10000

#To know the number of flights cancelled
table(airlinedf$Flight_cancelled)
           #Cancelled    : 232
           #Not Cancelled: 10050
#Split the dataset based on flight cancellation status
airlinedf_Cancelled <- filter(airlinedf, Flight_cancelled=="Yes")
airlinedf_NotCancelled <- filter(airlinedf,Flight_cancelled=="No")

#Summary statistics of the attributes
summary(airlinedf_NotCancelled)
summary(airlinedf_Cancelled)

#For Not_Cancelled Flights
#Replacing NAs of Departure delay in Minutes with Median value as 50 percent of the data has 0.00 values and median is a good measure of central tendency
airlinedf_NotCancelled$Departure_Delay_in_Minutes[is.na(airlinedf_NotCancelled$Departure_Delay_in_Minutes)] <- median(airlinedf_NotCancelled$Departure_Delay_in_Minutes, na.rm = TRUE)
#Replacing NAs for Arrival delay in Minutes with Median Value as 50 percent of the data has 0.00 values
airlinedf_NotCancelled$Arrival_Delay_in_Minutes[is.na(airlinedf_NotCancelled$Arrival_Delay_in_Minutes)] <- median(airlinedf_NotCancelled$Arrival_Delay_in_Minutes, na.rm=TRUE)
#Replacing NAs for Flight time in Minutes by interpolation method After sorting the data according to Flight Distance
airlinedf_NotCancelled <- airlinedf_NotCancelled %>% arrange(Flight_Distance)
library(imputeTS)
airlinedf_NotCancelled$Flight_time_in_minutes <- na_interpolation(airlinedf_NotCancelled$Flight_time_in_minutes)
#Checking again for missing values
sapply(airlinedf_NotCancelled, function(x) sum(is.na(x)))


#Date Visualization
#Understsnding the attributes relation
library(ggplot2)
#Satisfaction distribution
airlinedf %>% ggplot(aes(x=Satisfaction)) + geom_histogram(color="blue",fill="black", binwidth = 0.3)+ scale_x_continuous(breaks=1:10,labels = 1:10) + ggtitle(" Customer Satisfaction Distribution")


#Age Distribution. Age vs Satisfaction
airlinedf %>% ggplot(aes(x=Age)) + geom_histogram(color="blue",fill="black") + ggtitle(" Customer Age Distribution")
ggplot(airlinedf,aes(x=Age,y=Satisfaction))+geom_count()+
  stat_summary(aes(y =airlinedf$Satisfaction ,group=1), fun.y=mean, colour="blue", geom="line",group=1) ##?

ggplot(airlinedf,aes(x=Age,y=Satisfaction))+geom_count()+
  stat_summary(aes(y =airlinedf$Satisfaction ,group=1), fun.y=mean, colour="blue", geom="line",group=1)

ggplot(airlinedf,aes(x=Age,y=Satisfaction))+geom_count()+
  stat_summary(aes(y =airlinedf$Satisfaction ,group=1), fun.y=mean, colour="red", geom="line",group=1)

ggplot(airlinedf,aes(x=Gender,y=Satisfaction))+geom_count()+
  stat_summary(aes(y =airlinedf$Satisfaction ,group=1), fun.y=mean, colour="blue", geom="point",group=1)

ggplot(airlinedf,aes(x=Type_of_Travel,y=Satisfaction))+geom_count()+
  stat_summary(aes(y =airlinedf$Satisfaction ,group=1), fun.y=mean, colour="blue", geom="line",group=1)


ggplot(airlinedf,aes(x=Class,y=Satisfaction))+geom_count()+
  stat_summary(aes(y =airlinedf$Satisfaction ,group=1), fun.y=mean, colour="blue", geom="line",group=1)

# Satisfaction Vs Airline Status

ggplot(airlinedf) + 
  geom_bar(aes(Airline_Status,Satisfaction,width=0.5), 
           position = "dodge", stat = "summary", fun.y = "mean")


# Satisfaction Vs Price Sensitivity at AIRPORT

ggplot(airlinedf,aes(x=Price_Sensitivity,y=Satisfaction)) + 
  geom_bar(aes(Price_Sensitivity,Satisfaction), 
           position = "dodge", stat = "summary", fun.y = "mean")+ scale_y_continuous(breaks=seq(0,10,by=0.5))+
  stat_summary(aes(y =airlinedf$Satisfaction ,group=1), fun.y=mean, colour="blue", geom="line",group=1)

##Satisfaction Vs Gender
ggplot(airlinedf,aes(x=Gender,y=Satisfaction,width=0.5)) + 
  geom_bar(aes(Gender,Satisfaction), 
           position = "dodge", stat = "summary", fun.y = "mean")+ scale_y_continuous(breaks=seq(0,10,by=0.5))+
  stat_summary(aes(y =airlinedf$Satisfaction ,group=1), fun.y=mean, colour="blue", geom="line",group=1)
##Satisfaction Vs Partner Names
ggplot(airlinedf,aes(x=Partner_Name,y=Satisfaction)) + 
  geom_bar(aes(Partner_Name,Satisfaction), 
           position = "dodge", stat = "summary", fun.y = "mean")+ scale_y_continuous(breaks=seq(0,10,by=0.5))+
  stat_summary(aes(y =airlinedf$Satisfaction ,group=1), fun.y=mean, colour="blue", geom="line",group=1)
#Linear Regression On Southeast:

class(s)
airlinedf$Arrival_Delay_5min<-as.numeric(airlinedf$Arrival_Delay_in_Minutes>=5)
View(airlinedf)
lm1<-lm(Satisfaction~Gender+Flights_Per_Year+Loyalty+Type_of_Travel+Shopping_Amount_at_Airport+Eating_and_Drinking_at_Airport+Class+Day_of_Month+Partner_Name+Scheduled_Departure_Hour+Type_of_Travel+Age+Airline_Status+Price_Sensitivity+Flights_Per_Year+Flight_cancelled+
          +Loyalty+Shopping_Amount_at_Airport+Flight_Distance+olong+olat+dlat+dlong+Arrival_Delay_5min+
          +Eating_and_Drinking_at_Airport+Flight_time_in_minutes+Flight_cancelled+Departure_Delay_in_Minutes,data=airlinedf)
View(summary((lm1)))
glimpse(lm1)
summary(lm1)
colnames(airlinedf)
lm2<-lm(Satisfaction~Gender+Flights_Per_Year+Loyalty+Type_of_Travel+Shopping_Amount_at_Airport+Eating_and_Drinking_at_Airport+Class+Partner_Name+Scheduled_Departure_Hour+Type_of_Travel+Age+Airline_Status+Price_Sensitivity+Flight_cancelled+
          +Loyalty+Shopping_Amount_at_Airport+Flight_Distance
        +Eating_and_Drinking_at_Airport+Flight_time_in_minutes+Flight_cancelled+Arrival_Delay_5min+Departure_Delay_in_Minutes,data=airlinedf)
summary(lm2)
library("arules")
library("arulesViz")
airlinedf$Price_Sensitivity<-discretize(airlinedf$Price_Sensitivity,breaks = ,method="fixed")

airlinedfX <- as(airlinedf,"transactions")
inspect(airlinedfX)


