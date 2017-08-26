## Read the dataset.
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Have total emissions from PM2.5 decreased in the Baltimore City, 
## Maryland (fips == "24510") from 1999 to 2008? 
## Use the base plotting system to make a plot answering this question.

## Get the total PM2.5 emission from all sources in the Baltimore City
## for each of the years 1999, 2002, 2005, and 2008.
BC <- subset(NEI, fips == "24510")
total <- tapply(BC$Emissions, BC$year, sum)

## Making plots.

png("plot2.png")

barplot(
    total,
    names.arg = names(total),
    xlab = "Year",
    ylab = "Total PM2.5 Emission",
    main = "Total Emissions from PM2.5 in the Baltimore City"
)

dev.off()
