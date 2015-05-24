if(!file.exists("data"))
{
    dir.create("data")    
}

#Please make sure to use the setInternet2 function to enable the use of Internet Explorer for Internet access, notice that this issue happens with 'knitr' and download.file() in Windows systems.

setInternet2(use = TRUE)

if(!file.exists("data/StormData.csv.bz2"))
{
    fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"   
    download.file(fileUrl, destfile = "data/StormData.csv.bz2")
}

#By default read.csv will treat strings as factors, causing some issues later, it was used the option stringAsFactor set by FALSE to prevent that.
options(stringsAsFactors = FALSE)

stormData <- read.csv("./data/StormData.csv.bz2")

str(stormData)

#subsetting the columns
stormData <- stormData[,c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]

#creating a new summarized column
stormData$HEALTHDMG <- stormData$FATALITIES + stormData$INJURIES

#Assess the magnitudes of property and crop damages
unique(stormData$PROPDMGEXP)
unique(stormData$CROPDMGEXP)

##changing the character values to numbers to make the math

stormData$PROPDMGEXP[stormData$PROPDMGEXP=="K"] <- 3
stormData$PROPDMGEXP[stormData$PROPDMGEXP=="M"] <- 6
stormData$PROPDMGEXP[stormData$PROPDMGEXP==""] <- 0
stormData$PROPDMGEXP[stormData$PROPDMGEXP=="B"] <- 9
stormData$PROPDMGEXP[stormData$PROPDMGEXP=="m"] <- 6
stormData$PROPDMGEXP[stormData$PROPDMGEXP=="+"] <- 0
stormData$PROPDMGEXP[stormData$PROPDMGEXP=="?"] <- 0
stormData$PROPDMGEXP[stormData$PROPDMGEXP=="h"] <- 2
stormData$PROPDMGEXP[stormData$PROPDMGEXP=="H"] <- 2
stormData$PROPDMGEXP[stormData$PROPDMGEXP=="-"] <- 0

##also taking into account the NAs values.
stormData$PROPDMGEXP[is.na(stormData$PROPDMGEXP)] <- 0

#transforming in a numeric column.
stormData$PROPDMGEXP <- as.numeric(stormData$PROPDMGEXP)

stormData$CROPDMGEXP[stormData$CROPDMGEXP==""] <- 0
stormData$CROPDMGEXP[stormData$CROPDMGEXP=="M"] <- 6
stormData$CROPDMGEXP[stormData$CROPDMGEXP=="K"] <- 3
stormData$CROPDMGEXP[stormData$CROPDMGEXP=="m"] <- 6
stormData$CROPDMGEXP[stormData$CROPDMGEXP=="B"] <- 9
stormData$CROPDMGEXP[stormData$CROPDMGEXP=="?"] <- 0
stormData$CROPDMGEXP[stormData$CROPDMGEXP=="k"] <- 3

##taking care of the NAs values.
stormData$CROPDMGEXP[is.na(stormData$CROPDMGEXP)] <- 0

#transforming in a numeric column.
stormData$CROPDMGEXP <- as.numeric(stormData$CROPDMGEXP)

stormData$ECONOMICDMG <- (stormData$PROPDMG * 10^(stormData$PROPDMGEXP)) + (stormData$CROPDMG * 10^(stormData$CROPDMGEXP)) 

str(stormData)
unique(stormData$PROPDMGEXP)
unique(stormData$CROPDMGEXP)

#Loading the 'dplyr' library
library(dplyr)

healthDamage <- summarize(group_by(stormData, EVTYPE), TOTAL=sum(HEALTHDMG))
economicDamage <- summarize(group_by(stormData, EVTYPE), TOTAL=sum(ECONOMICDMG))

healthDamage <- healthDamage[order(healthDamage$TOTAL, decreasing = TRUE), ]
healthDamage <- healthDamage[1:10, ]

economicDamage <- economicDamage[order(economicDamage$TOTAL, decreasing = TRUE), ]
economicDamage <- economicDamage[1:10, ]

#preview the results (the total measure of healthDamage counts people and economic is in dollars).
head(healthDamage)
head(economicDamage)

library(ggplot2)

healthPlot <- ggplot(healthDamage[1:5, ], aes(x=EVTYPE, y=TOTAL, fill=EVTYPE)) +
    geom_bar(stat="identity") + xlab("Event Type") + ylab("People Harmed") +
    theme(legend.position="none") + ggtitle ("People harmed by weather event type")

healthPlot

economicPlot <- ggplot(economicDamage[1:5, ], aes(x=EVTYPE, y=TOTAL, fill=EVTYPE)) +
    geom_bar(stat="identity") + xlab("Event Type") + ylab("Economic damage in USD") +
    theme(legend.position="none") + ggtitle ("Economic damage by weather event type")

economicPlot


