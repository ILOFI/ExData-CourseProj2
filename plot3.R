## Source ggplot2 and dplyr package.
library(ggplot2)
library(dplyr)

## Read the dataset.
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) 
## variable, which of these four sources have seen decreases in emissions 
## from 1999¨C2008 for Baltimore City? Which have seen increases in emissions from 1999¨C2008? 
## Use the ggplot2 plotting system to make a plot answer this question.

## Get the total PM2.5 emission from all sources in the Baltimore City
## for each of the years 1999, 2002, 2005, and 2008 and for each type.
BC <- subset(NEI, fips == "24510")
total <-
    BC %>% group_by(type, year) %>% summarize(sum = sum(Emissions))

## Making plots.

png("plot3.png", 680)

ggplot(total, aes(x = factor(year), y = sum, fill = type)) + 
    geom_bar(stat = "identity") + 
    facet_grid(.~type) + 
    xlab("Year") + 
    ylab("Total PM2.5 Emission") + 
    ggtitle("Total Emissions from PM2.5 for Each Type in the Baltimore City")

dev.off()
