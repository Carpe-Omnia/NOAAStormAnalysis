dt <- function() {
library(ggplot2)
library(dplyr)
  
##checking to see if storm data exists and downloading it if it does not    
stormDataURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
stormFileName <- "./myStormData.csv"
if(!file.exists(stormFileName)){
  download.file(stormDataURL, stormFileName)
}
##Checks to see if the file has already been read (saves a bunch of time on subsequent runs)
if(!exists("myStormData")){myStormData <- read.csv(stormFileName)}

##messing with the date times
##adding a year column to make plotting easier
myStormData$YEAR<- format(as.POSIXct(myStormData$BGN_DATE, format = "%m/%d/%Y %H:%M:%S", tz = "" ), "%Y")
myStormData$YEAR <- as.numeric(myStormData$YEAR)

##damage to human life
##getting average fatalities for event type
myStorms <- aggregate(x=myStormData$FATALITIES, by = list(myStormData$EVTYPE), FUN=mean)
myStorms <- myStorms[order(myStorms$x, decreasing=TRUE),]
colnames(myStorms) <- c("stormType","AvgFatalities")
#print(head(myStorms))

myStorms_summary <- myStormData %>%
  group_by(EVTYPE) %>%
  summarize(Total_Sum = sum(FATALITIES), .groups = "drop") %>%
  arrange(desc(Total_Sum)) %>%  # Sort by total sum in descending order
  slice(1:5) %>% # Keep only the top 5 categories
  left_join(myStormData, by = "EVTYPE") %>% # Join back with original data to get Year and Value
  group_by(YEAR, EVTYPE) %>%
  summarize(Sum_Value = sum(Total_Sum), .groups = "drop") %>%
  arrange(YEAR, desc(Sum_Value)) # Sort by year and then within year by sum value

fatalPlot <- ggplot(myStorms_summary, aes(x = YEAR, y = Sum_Value, fill = EVTYPE)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ EVTYPE, ncol = 1, scales="free_y")
  labs(title = "Sum of Fatalities by Year (Top 5 EVTYPEs)",
      x = "Year",
      y = "Sum of Fatalities",
      fill = "Event Type") +
    theme_bw() +
    xlim(1990, 2025) + 
    scale_x_continuous(breaks = unique(myStorms_summary$YEAR))
print(fatalPlot)

##economic Damage
myStorms <- aggregate(x=myStormData$PROPDMG, by = list(myStormData$EVTYPE), FUN=mean)
myStorms <- myStorms[order(myStorms$x, decreasing=TRUE),]
colnames(myStorms) <- c("stormType","AvgPropertyDamage")
#print(head(myStorms))
myCostlyStorms <- myStorms$stormType[1:5]

myStorms_summary <- myStormData %>%
  group_by(EVTYPE) %>%
  summarize(Total_Sum = sum(PROPDMG), .groups = "drop") %>%
  arrange(desc(Total_Sum)) %>%  # Sort by total sum in descending order
  slice(1:5) %>% # Keep only the top 5 categories
  left_join(myStormData, by = "EVTYPE") %>% # Join back with original data to get Year and Value
  group_by(YEAR, EVTYPE) %>%
  summarize(Sum_Value = sum(Total_Sum), .groups = "drop") %>%
  arrange(YEAR, desc(Sum_Value)) # Sort by year and then within year by sum value

propertyPlot <- ggplot(myStorms_summary, aes(x = YEAR, y = Sum_Value, fill = EVTYPE)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ EVTYPE, ncol = 1, scales="free_y")
labs(title = "Sum of Property Damage by Year (Top 5 EVTYPEs)",
     x = "Year",
     y = "Sum of Fatalities",
     fill = "Event Type") +
  theme_bw() +
  scale_x_continuous(breaks = unique(myStorms_summary$YEAR))
print(propertyPlot)

}