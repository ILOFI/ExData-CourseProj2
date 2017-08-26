## Read the dataset.
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
## Using the base plotting system, make a plot showing the total PM2.5 emission 
## from all sources for each of the years 1999, 2002, 2005, and 2008.

## Get the total PM2.5 emission from all sources
## for each of the years 1999, 2002, 2005, and 2008.
total <- tapply(NEI$Emissions, NEI$year, sum)

## Making plots.

png("plot1.png")

barplot(
    total,
    names.arg = names(total),
    xlab = "Year",
    ylab = "Total PM2.5 Emission",
    main = "Total Emissions from PM2.5 in US"
)

dev.off()
