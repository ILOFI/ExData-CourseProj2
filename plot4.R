## Source ggplot2 and dplyr package.
library(ggplot2)
library(dplyr)

## Read the dataset.
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Across the United States, how have emissions 
## from coal combustion-related sources changed from 1999¨C2008?

## Get the data
index.of.coal.comb <- grep("Fuel Comb.*Coal", SCC$EI.Sector)
CC <- NEI[NEI$SCC %in% SCC[index.of.coal.comb, ]$SCC, ]
total <- tapply(CC$Emissions, CC$year, sum)
total <- data.frame(year = names(total), sum = total)

## Making plots.

png("plot4.png")

ggplot(total, aes(x = year, y = sum)) +
    geom_bar(stat = "identity") +
    xlab("Year") +
    ylab("Total PM2.5 Emission") +
    ggtitle("Total Emissions of PM2.5 from Coal Combustion-Related Sources")

dev.off()
