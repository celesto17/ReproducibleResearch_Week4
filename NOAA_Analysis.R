# setwd("F:/01. Data Science/05. Reproducible Research/04. Week 4/05. Course Project")
# https://rpubs.com/noeliacarrillo/284385

if (!file.exists("./downloadedDataset"))
{
 dir.create("./downloadedDataset")
}
if (!file.exists ("./downloadedDataset/repdata%2Fdata%2FStormData.csv.bz2")) # This step is to avoid downloading data every time one runs this script
{
 datasetURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
 download.file (datasetURL, destfile="./downloadedDataset/repdata%2Fdata%2FStormData.csv.bz2")
}

# Keep only relevant data variables required for calculating damage caused by disasters
stormDataSet <- read.csv("./downloadedDataset/repdata%2Fdata%2FStormData.csv.bz2", header = TRUE, sep = ",", quote = "\"")
stormDataRelevant <- stormDataSet [ , c(8, 23:28)]

library (dplyr)
# Calculate Injuries and Fatalities caused by disaster type
totalInjuries <- aggregate (INJURIES ~ EVTYPE, stormDataRelevant, sum)
totalInjuries <- arrange (totalInjuries, desc(INJURIES))
totalInjuries <- totalInjuries [1:20, ]

totalFatalities <- aggregate (FATALITIES ~ EVTYPE, stormDataRelevant, sum)
totalFatalities <- arrange (totalFatalities, desc(FATALITIES))
totalFatalities <- totalFatalities [1:20, ]

# Merge Injuries and Fatalities data
totalDamage <- merge (totalFatalities, totalInjuries, by.x = "EVTYPE", by.y = "EVTYPE")
totalDamage <- arrange (totalDamage, desc (FATALITIES + INJURIES))
namesEvents <- totalDamage$EVTYPE
# Plot graph showing Injusris and Fatalities damaged by disasters
barplot (t(totalDamage [,-1]), names.arg = namesEvents, ylim = c (0, 95000), beside = T, cex.names = 0.8, las = 2, col = c("red", "blue"), main ="Maximum Damage by Disaster 1")
legend ("topright", c ("Fatalities", "Injuries"), fill = c("red","blue"), bty = "n")

stormDataSet$PROPDAMAGE = 0
stormDataSet[stormDataSet$PROPDMGEXP == "H", ]$PROPDAMAGE = stormDataSet[stormDataSet$PROPDMGEXP == "H", ]$PROPDMG * 10^2
stormDataSet[stormDataSet$PROPDMGEXP == "K", ]$PROPDAMAGE = stormDataSet[stormDataSet$PROPDMGEXP == "K", ]$PROPDMG * 10^3
stormDataSet[stormDataSet$PROPDMGEXP == "M", ]$PROPDAMAGE = stormDataSet[stormDataSet$PROPDMGEXP == "M", ]$PROPDMG * 10^6
stormDataSet[stormDataSet$PROPDMGEXP == "B", ]$PROPDAMAGE = stormDataSet[stormDataSet$PROPDMGEXP == "B", ]$PROPDMG * 10^9

stormDataSet$CROPDAMAGE = 0
stormDataSet[stormDataSet$CROPDMGEXP == "H", ]$CROPDAMAGE = stormDataSet[stormDataSet$CROPDMGEXP == "H", ]$CROPDMG * 10^2
stormDataSet[stormDataSet$CROPDMGEXP == "K", ]$CROPDAMAGE = stormDataSet[stormDataSet$CROPDMGEXP == "K", ]$CROPDMG * 10^3
stormDataSet[stormDataSet$CROPDMGEXP == "M", ]$CROPDAMAGE = stormDataSet[stormDataSet$CROPDMGEXP == "M", ]$CROPDMG * 10^6
stormDataSet[stormDataSet$CROPDMGEXP == "B", ]$CROPDAMAGE = stormDataSet[stormDataSet$CROPDMGEXP == "B", ]$CROPDMG * 10^9

economic_damage <- aggregate (PROPDAMAGE + CROPDAMAGE ~ EVTYPE, stormDataSet, sum)
names (economic_damage) = c ("EVENT_TYPE", "TOTAL_DAMAGE")
economic_damage <- arrange (economic_damage, desc(TOTAL_DAMAGE))
economic_damage <- economic_damage[1:20, ]
economic_damage$TOTAL_DAMAGE <- economic_damage$TOTAL_DAMAGE/10^9
economic_damage$EVENT_TYPE <- factor (economic_damage$EVENT_TYPE, levels = economic_damage$EVENT_TYPE)

with (economic_damage, barplot(TOTAL_DAMAGE, names.arg = EVENT_TYPE, beside = T, cex.names = 0.8, las=2, col = "red", main = "Total Property and Crop Damage by Top 20 Event Types", ylab = "Total Damage in USD (10^9)"))