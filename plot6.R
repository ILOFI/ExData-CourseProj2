## Source ggplot2 and dplyr package.
library(ggplot2)
library(dplyr)

## Read the dataset.
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Compare emissions from motor vehicle sources in Baltimore City
## with emissions from motor vehicle sources in Los Angeles County,
## California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?

## Get the data
## Baltimore City
BC <- subset(NEI, fips == "24510" & type == "ON-ROAD")
BCtotal <- tapply(BC$Emissions, BC$year, sum)

## Los Angeles
LA <- subset(NEI, fips == "06037" & type == "ON-ROAD")
LAtotal <- tapply(LA$Emissions, LA$year, sum)

total <-
    data.frame(
        year = rep(names(BCtotal), 2),
        sum = c(BCtotal, LAtotal),
        County = c(rep("Baltimore", 4), rep("Los Angeles", 4))
    )

## Making plots.

png("plot6.png")

ggplot(total, aes(x = year, y = sum, fill = County)) +
    geom_bar(stat = "identity") +
    facet_grid(County ~ ., scales = "free") +
    xlab("Year") +
    ylab("Total PM2.5 Emission") +
    ggtitle("PM2.5 Emissions from Moter Vehicles in Baltimore and Los Angeles \nby Year")

dev.off()
