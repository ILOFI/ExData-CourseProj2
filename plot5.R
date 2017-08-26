## Source ggplot2 and dplyr package.
library(ggplot2)
library(dplyr)

## Read the dataset.
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## How have emissions from motor vehicle sources changed 
## from 1999¨C2008 in Baltimore City?

## Get the data
BC <- subset(NEI, fips == "24510" & type == "ON-ROAD")
total <- tapply(BC$Emissions, BC$year, sum)
total <- data.frame(year = names(total), sum = total)

## Making plots.

png("plot5.png")

ggplot(total, aes(x = year, y = sum)) +
    geom_bar(stat = "identity") +
    xlab("Year") +
    ylab("Total PM2.5 Emission") +
    ggtitle("PM2.5 Emissions from Moter Vehicles in the Baltimore City by Year")

dev.off()
