## Dalao's Work

## plot 4.
library(stats)
main_data <- readRDS("summarySCC_PM25.rds")
class_codes <- readRDS("Source_Classification_Code.rds")

## it appears that the main classification is in the field "EI.Sector"
## coal combustion sources contain the word "Coal" 
class_codes$EI.Sector <- as.character(class_codes$EI.Sector)
codes_subset <- class_codes[grep("Coal", class_codes$EI.Sector), ]
## subset the main data file to the rows with codes containing coal comb sources
merged_data <- merge(codes_subset, main_data, by="SCC", all.x = TRUE)

## as we required to illustrate changes "across the US", in addition to the aggregated
## total for the US we will plot several counties from different parts of the country.

## prepare aggregated files for the US and counties
plotting_US <- merged_data[, c(18, 20)]
plotting_US <- aggregate(.~ year, data = plotting_US, FUN = "sum")
plotting_US$Emissions <- plotting_US$Emissions / 1000
plotting_county <- merged_data[, c(16, 18, 20)]
plotting_county <- aggregate(.~ year + fips, data = plotting_county, FUN = "sum")

## for a sample we selected 3 states from the western, central and eastern parts of the country.
## These are Washington, Kansas and New York. Within these states we selected counties
## that had maximum PM2.5 volumes in 1999 and also had values for all the years in the question.
## the selected counties are Lewis, Washington(53041), Pottawatomie, Kansas(20149) and 
## Chautauqua, New York (36013)

Washington <- plotting_county[grep("^53", plotting_county$fips), ]
Washington <- Washington[order(Washington$year, -Washington$Emissions), ]
head(Washington)

Kansas <- plotting_county[grep("^20", plotting_county$fips), ]
Kansas <- Kansas[order(Kansas$year, -Kansas$Emissions), ]
head(Kansas)

NYork <- plotting_county[grep("^36", plotting_county$fips), ]
NYork <- NYork[order(NYork$year, -NYork$Emissions), ]
head(NYork)

## make files for plotting each of the selected counties
LW <- plotting_county[plotting_county$fips == "53041", ]
PK <- plotting_county[plotting_county$fips == "20149", ]
CN <- plotting_county[plotting_county$fips == "36013", ]
## check that values for all 4 years are present
LW
PK
CN

## restate values to '000 tons
LW$Emissions <- LW$Emissions / 1000
PK$Emissions <- PK$Emissions / 1000
CN$Emissions <- CN$Emissions / 1000

## make a 2*2 plot
par(mar = c(2, 4, 2, 0.5))
par(mfrow = c(2, 2))

bp4_1 <- barplot(plotting_US$Emissions, plotting_US$year, 
                 col = "green", names.arg = c(1999, 2002, 2005, 2008),
                 main = "PM2.5 from coal comb in the US",
                 ylab = "PM2.5 emission ('000 tons)" )
text(x = bp4_1, y = plotting_US$Emissions,
     label = sprintf("%0.0f", round(plotting_US$Emissions, digits = 0)), pos = 1)

bp4_2 <- barplot(LW$Emissions, LW$year, 
                 col = "steelblue", names.arg = c(1999, 2002, 2005, 2008),
                 main = "PM2.5 from coal comb in Lewis, WA",
                 cex.main = 0.95,
                 ylab = "PM2.5 emission ('000 tons)" )
text(x = bp4_2, y = LW$Emissions,
     label = sprintf("%0.2f", round(LW$Emissions, digits = 2)), pos = 1, col = "white")

bp4_3 <- barplot(PK$Emissions, PK$year, 
                 col = "darkgrey", names.arg = c(1999, 2002, 2005, 2008),
                 main = "PM2.5 from coal comb in Pottawatomie, KS",
                 cex.main = 0.95,
                 ylab = "PM2.5 emission ('000 tons)" )
text(x = bp4_3, y = PK$Emissions,
     label = sprintf("%0.2f", round(PK$Emissions, digits = 2)), pos = 1, col = "white")

bp4_4 <- barplot(CN$Emissions, CN$year, 
                 col = "orange", names.arg = c(1999, 2002, 2005, 2008),
                 main = "PM2.5 from coal comb in Chautauqua, NY",
                 cex.main = 0.95,
                 ylab = "PM2.5 emission ('000 tons)" )
text(x = bp4_4, y = CN$Emissions,
     label = sprintf("%0.2f", round(CN$Emissions, digits = 2)), pos = 1)

dev.copy(png, file = "plot4.png", width = 600, height = 600)
dev.off()

## plot 5.
library(stats)
main_data <- readRDS("summarySCC_PM25.rds")
class_codes <- readRDS("Source_Classification_Code.rds")

## it appears that the main classification is in the field "EI.Sector".
## motor vehicles are included in the "Mobile - On-Road" and "Mobile - Non-Road" categories
## we didn't perform any additional arbitrary selection of individual items from "Short.Name" field

class_codes$EI.Sector <- as.character(class_codes$EI.Sector)
codes_subset2 <- class_codes[grep("Mobile - On-Road|Mobile - Non-Road", class_codes$EI.Sector), ]
## subset the main data file to the rows with codes containing coal comb sources
merged_data2 <- merge(codes_subset2, main_data, by="SCC", all.x = TRUE)

## setup data for Baltimore 
Balt5 <- merged_data2[, c(16, 18, 20)]
Balt5 <- aggregate(.~ year + fips, data = Balt5, FUN = "sum")
Balt5 <- Balt5[Balt5$fips == "24510", ]

## add absolute and % change 
a <- diff(Balt5$Emissions)
b <- (a / Balt5$Emissions[-length(Balt5$Emissions)])*100
a <- c(NA, a)
b <- c(NA, b)
ab <- cbind(a, b)
colnames(ab) <- c("abs_change", "percent_change")

Balt5 <- cbind(Balt5, ab)

par(mar = c(2, 6, 2, 1))
par(mfrow = c(1, 2))

bp5_1 <- barplot(Balt5$Emissions, Balt5$year, 
                 col = "steelblue", names.arg = c(1999, 2002, 2005, 2008),
                 main = "PM2.5 emission from motor vehicles, Baltimore, MD",
                 cex.main = 0.95,
                 ylab = "PM2.5 emission (tons)" )
text(x = bp5_1, y = Balt5$Emissions,
     label = sprintf("%0.2f", round(Balt5$Emissions, digits = 2)), pos = 1, col = "white")

bp5_2 <- barplot(Balt5$percent_change, Balt5$year, 
                 col = "darkgrey", names.arg = c("", 2002, 2005, 2008),
                 main = "PM2.5 emission from motor vehicles, Baltimore, MD",
                 cex.main = 0.95,
                 ylab = "% change from previous year" )
text(x = bp5_2, y = Balt5$percent_change,
     label = sprintf("%0.2f", round(Balt5$percent_change, digits = 2)), pos = 3, col = "white")
dev.copy(png, file = "plot5.png", width = 720, height = 480)
dev.off()

## plot 6.
library(stats)
main_data <- readRDS("summarySCC_PM25.rds")
class_codes <- readRDS("Source_Classification_Code.rds")

## it appears that the main classification is in the field "EI.Sector".
## motor vehicles are included in the "Mobile - On-Road" and "Mobile - Non-Road" categories
## we didn't perform any additional arbitrary selection of individual items from "Short.Name" field

class_codes$EI.Sector <- as.character(class_codes$EI.Sector)
codes_subset2 <- class_codes[grep("Mobile - On-Road|Mobile - Non-Road", class_codes$EI.Sector), ]
## subset the main data file to the rows with codes containing coal comb sources
merged_data2 <- merge(codes_subset2, main_data, by="SCC", all.x = TRUE)

## setup data for Baltimore and LA
County_data <- merged_data2[, c(16, 18, 20)]
County_data <- aggregate(.~ year + fips, data = County_data, FUN = "sum")
Balt6 <- County_data[County_data$fips == "24510", ]
LAC <- County_data[County_data$fips == "06037", ]


## add cumulative % change from 1999 
library(dplyr)
Balt6 <- mutate(Balt6, base_year = rep(Balt6[1,3], length(Balt6$Emissions)))
a <- diff(Balt6$Emissions)
b <- cumsum(a)
a <- c(NA, a)
b <- c(NA, b)
cum_perc_change <- b / Balt6$base_year * 100
Balt6 <- cbind(Balt6, cum_perc_change)

LAC <- mutate(LAC, base_year = rep(LAC[1,3], length(LAC$Emissions)))
a1 <- diff(LAC$Emissions)
b1 <- cumsum(a1)
a1 <- c(NA, a1)
b1 <- c(NA, b1)
cum_perc_changeLA <- b1 / LAC$base_year * 100
LAC <- cbind(LAC, cum_perc_changeLA)

## make the plot
par(mar = c(2, 6, 2, 1))
par(mfrow = c(2, 2))

bp6_1 <- barplot(Balt6$Emissions, Balt6$year, 
                 col = "steelblue", names.arg = c(1999, 2002, 2005, 2008),
                 main = "PM2.5 emission from motor vehicles, Baltimore, MD",
                 cex.main = 0.95,
                 ylab = "PM2.5 emission (tons)" )
text(x = bp6_1, y = Balt6$Emissions,
     label = sprintf("%0.0f", round(Balt6$Emissions, digits = 0)), pos = 1, col = "white")

bp6_2 <- barplot(Balt6$cum_perc_change, Balt6$year, 
                 col = "darkgrey", names.arg = c("", 2002, 2005, 2008),
                 main = "PM2.5 emission from motor vehicles, Baltimore, MD",
                 cex.main = 0.95,
                 ylab = "cumulative change from 1999, %" )
text(x = bp6_2, y = Balt6$cum_perc_change,
     label = sprintf("%0.2f", round(Balt6$cum_perc_change, digits = 2)), pos = 3, col = "white")

bp6_3 <- barplot(LAC$Emissions, LAC$year, 
                 col = "lightgreen", names.arg = c(1999, 2002, 2005, 2008),
                 main = "PM2.5 emission from motor vehicles, LA, CA",
                 cex.main = 0.95,
                 ylab = "PM2.5 emission (tons)" )
text(x = bp6_3, y = LAC$Emissions,
     label = sprintf("%0.0f", round(LAC$Emissions, digits = 0)), pos = 1, col = "black")

bp6_4 <- barplot(LAC$cum_perc_changeLA, LAC$year, 
                 col = "darkgrey", names.arg = c("", 2002, 2005, 2008),
                 main = "PM2.5 emission from motor vehicles, LA, CA",
                 cex.main = 0.95,
                 ylab = "cumulative change from 1999, %" )
text(x = bp6_4, y = LAC$cum_perc_changeLA,
     label = sprintf("%0.2f", round(LAC$cum_perc_changeLA, digits = 2)), pos = 1, col = "white")


dev.copy(png, file = "plot6.png", width = 720, height = 480)
dev.off()