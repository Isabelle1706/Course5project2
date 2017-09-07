# reproducible research project 2

# data source: https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2

rm(list=ls())

library(ROracle)
library(DBI)

# load data into R

if (!file.exists("coursera")){dir.create("coursera")}
fileurl="https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileurl,destfile="~/R/coursera/stormdata.csv.bz2")
datedownloaded=date()

storm=read.csv("~/R/coursera/stormdata.csv.bz2", header=T, sep=",")

# get to know the data

str(storm)

table(toupper(storm$EVTYPE))
table(toupper(storm$STATE))

class(storm$FATALITIES)
summary(storm$FATALITIES)

class(storm$INJURIES)
summary(storm$INJURIES)

class(storm$PROPDMG)
class(storm$PROPDMGEXP)

table(storm$PROPDMGEXP)

test=subset(storm, PROPDMGEXP %in% c("-", "?", "+", "0", "1", "2", "3", "4", "5", "6", "7", "8", "h", "H"))

table(storm$PROPDMG==storm$CROPDMG)

class(storm$CROPDMG)

summary(storm$CROPDMG)

table(storm$CROPDMGEXP)

test1=subset(storm, CROPDMGEXP %in% c("?", "0", "2"))

# clea-up the data

library(dplyr)

# change storm type all in upper letters

storm$EVTYPE=toupper(storm$EVTYPE)

table(storm$EVTYPE)



storm=storm %>% 
        mutate (DEATH_INJURIES=FATALITIES+INJURIES) %>%
        mutate(PROPDMG1=ifelse(PROPDMGEXP %in% c("K", "k"), PROPDMG*1000,
                               ifelse(PROPDMGEXP %in% c("m", "M"), PROPDMG*1000000,
                                      ifelse(PROPDMGEXP %in% c("B","b"), PROPDMG*1000000000, PROPDMG)))) %>%
        mutate(PROPDMG_K=PROPDMG1/1000) %>%
        mutate(CROPDMG1=ifelse(CROPDMGEXP %in% c("K", "k"), CROPDMG*1000,
                               ifelse(CROPDMGEXP %in% c("m", "M"), CROPDMG*1000000,
                                      ifelse(CROPDMGEXP %in% c("B","b"), CROPDMG*1000000000, CROPDMG)))) %>%
        mutate(CROPDMG_K=CROPDMG1/1000) %>%
        mutate(TOTALDMG_K=PROPDMG_K+CROPDMG_K)



# Which types of events are most harmful with respect to pupulation health

# Top 10 most harmful events for fatalities

T_death=storm %>%
        group_by(EVTYPE) %>%
        summarise(Total_death=sum(FATALITIES, na.rm=T)) %>%
        arrange(desc(Total_death))

Top10_death=head(T_death, n=10)

print(Top10_death)

# Top 10 most harmful events for injuries

T_injury=storm %>%
        group_by(EVTYPE) %>%
        summarise(Total_injury=sum(INJURIES, na.rm=T)) %>%
        arrange(desc(Total_injury))

Top10_injury=head(T_injury, n=10)

print(Top10_injury)

# Top 10 most harmful events for death and injuries

T_death_injury=storm %>%
        group_by(EVTYPE) %>%
        summarise(Total_death_injury=sum(DEATH_INJURIES, na.rm=T)) %>%
        arrange(desc(Total_death_injury))

Top10_death_injury=head(T_death_injury, n=10)

print(Top10_death_injury)



library(ggplot2)

# create a bar plot to show the top 20 most harmful weather events for fatalities

Top20_death=head(T_death, n=20)

              
      g=ggplot(Top20_death, aes(x=EVTYPE, y=Total_death))
      g+ geom_bar(position="dodge", stat="identity", col="blue") +
              coord_flip() +
              xlab("Weather Event") +
              ylab("Total Number of Fatalities ") +
              ggtitle("The Top 20 Most Harmful Weather Events for Fatalites ")
      
      
      
      
# Which types of events have the greatest economic consequeces
      
# Top 10 most harmful  events for property damage in thousand dollars
      
      T_propdmg=storm %>%
              group_by(EVTYPE) %>%
              summarise(Total_propdmg=sum(PROPDMG_K, na.rm=T)) %>%
              arrange(desc(Total_propdmg))
      
      Top10_propdmg=head(T_propdmg, n=10)
      
      print(Top10_propdmg)
      
# Top 10 most harmful  events for crop damage in thousand dollars
      
      T_cropdmg=storm %>%
              group_by(EVTYPE) %>%
              summarise(Total_cropdmg=sum(CROPDMG_K, na.rm=T)) %>%
              arrange(desc(Total_cropdmg))
      
      Top10_cropdmg=head(T_cropdmg, n=10)
      
      print(Top10_cropdmg)
      
      
 # Top 10 most harmful  events for property and crop damage in thousand dollars
      
      T_totaldmg=storm %>%
              group_by(EVTYPE) %>%
              summarise(Totaldmg=sum(TOTALDMG_K, na.rm=T)) %>%
              arrange(desc(Totaldmg))
      
      Top10_totaldmg=head(T_totaldmg, n=10)
      
      print(Top10_totaldmg)
      
      
# create a bar plot to show the top 20 most harmful weather events for property and crop damage
   
T_totaldmg=T_totaldmg %>%
        mutate(Totaldmg_in_M=Totaldmg/1000)

      
Top20_totaldmg=head(T_totaldmg, n=20)      
      
      g=ggplot(Top20_totaldmg, aes(x=EVTYPE, y=Totaldmg_in_M))
      g+ geom_bar(position="dodge", stat="identity", col="green") +
              coord_flip() +
              xlab("Weather Event") +
              ylab("Total Damage in Million Dollars ") +
              ggtitle("The Top 20 Most Harmful Weather Events for Property and Crop Damage")
      
      
      
