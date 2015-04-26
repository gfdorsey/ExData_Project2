## Plot 6:
## Compare emissions from motor vehicle sources in Baltimore City with emissions
## from motor vehicle sources in Los Angeles County, California
## (fips == "06037"). Which city has seen greater changes over time in motor
## vehicle emissions?

library(dplyr)
NEI <- readRDS("dataset/summarySCC_PM25.rds")

NEI.24510.vehicle <- filter(NEI, fips == "24510" & type == "ON-ROAD")
NEI.06037.vehicle <- filter(NEI, fips == "06037" & type == "ON-ROAD")

# need to create sums by year to calculate total emissions
tbl <- tapply(NEI.24510.vehicle$Emissions, NEI.24510.vehicle$year, sum)
years <- as.numeric(attributes(tbl)$dimnames[[1]])
sums.24510 <- as.vector(tbl)
tbl <- tapply(NEI.06037.vehicle$Emissions, NEI.06037.vehicle$year, sum)
sums.06037 <- as.vector(tbl)

tbl <- tapply(log(NEI.24510.vehicle$Emissions), NEI.24510.vehicle$year, median)
med.24510 <- as.vector(tbl)
tbl <- tapply(log(NEI.06037.vehicle$Emissions), NEI.06037.vehicle$year, median)
med.06037 <- as.vector(tbl)

# predict regression line and confidence intervals
years.range <- seq(min(years), max(years), by=9.0/100.0)

model.24510 <- lm(sums.24510 ~ years)
r2.24510 <- summary(model.24510)$r.squared
sums.24510.pred <- predict(model.24510, newdata = data.frame(years=years.range), 
                     interval = 'confidence')

model.06037 <- lm(sums.06037 ~ years)
r2.06037 <- summary(model.06037)$r.squared
sums.06037.pred <- predict(model.06037, newdata = data.frame(years=years.range), 
                           interval = 'confidence')

# create plot
baltimoreColor = rgb(0.8, 0.4, 0.0, 0.5)
losAngelesColor = rgb(0.1, 0.6, 1.0, 0.5)
png(filename = "plot6.png", width = 960, height = 960)
par(mfrow = c(2,2), mar = c(4,4,4,2), cex = 0.8)

## Baltimore
plot(years, sums, main="Total PM2.5 Emissions from Motor Vehicle Sources\nfor Years 1999-2008, Baltimore City",
     xlab = "Year", ylab = "PM2.5 Emissions (tons)", type = "n",
     xaxp = c(1999, 2008, 3),
     ylim=c(min(sums.24510.pred[,2],sums.06037.pred[,2]),
            max(sums.06037.pred[,3],sums.06037.pred[,3])))

# draw confidence interval and regression line
polygon(c(rev(years.range), years.range), c(rev(sums.24510.pred[ ,3]),
                sums.24510.pred[ ,2]), col = "grey90", border = NA)
lines(sums.24510.pred[,1] ~ years.range, lwd = 2, lty="dashed", col="darkorange3")
lines(sums.24510.pred[,2] ~ years.range, lwd = 2, lty = "dotted", col = "darkorange3")
lines(sums.24510.pred[,3] ~ years.range, lwd = 2, lty = "dotted", col = "darkorange3")

# plot points last so they are visible over the confidence interval
points(years, sums.24510, pch = 16, col = "darkorange3")

# show r squared value for reference
legend("topright", bty="n", legend=paste("r^2 =", format(r2.24510, digits=4)))

## LA
plot(years, sums, main="Total PM2.5 Emissions from Motor Vehicle Sources\nfor Years 1999-2008, Los Angeles County",
     xlab = "Year", ylab = "PM2.5 Emissions (tons)", type = "n",
     xaxp = c(1999, 2008, 3),
     ylim=c(min(sums.24510.pred[,2],sums.06037.pred[,2]),
            max(sums.06037.pred[,3],sums.06037.pred[,3])))

# draw confidence interval and regression line
polygon(c(rev(years.range), years.range), c(rev(sums.06037.pred[ ,3]),
        sums.06037.pred[ ,2]), col = "grey90", border = NA)
lines(sums.06037.pred[,1] ~ years.range, lwd = 2, lty="dashed", col="dodgerblue")
lines(sums.06037.pred[,2] ~ years.range, lwd = 2, lty = "dotted", col = "dodgerblue")
lines(sums.06037.pred[,3] ~ years.range, lwd = 2, lty = "dotted", col = "dodgerblue")

# plot points last so they are visible over the confidence interval
points(years, sums.06037, pch = 16, col = "dodgerblue")

# show r squared value for reference
legend("topright", bty="n", legend=paste("r^2 =", format(r2.06037, digits=4)))


## Baltimore individual sources
plot(jitter(NEI.24510.vehicle$year, factor=0.5), log(NEI.24510.vehicle$Emissions),
     main="PM2.5 Emissions Related to Vehicle Emissions by Individual Source\nfor Years 1999-2008, Baltimore City",
     xlab = "Year", ylab = "log PM2.5 Emmissions (tons)",
     xaxp = c(1999, 2008, 3), pch=20, col=baltimoreColor,
     ylim = c(log(min(NEI.24510.vehicle$Emissions, NEI.06037.vehicle$Emissions)),
              log(max(NEI.24510.vehicle$Emissions, NEI.06037.vehicle$Emissions))))
abline(lm(med.24510 ~ years))

## LA individual sources
plot(jitter(NEI.06037.vehicle$year, factor=0.5), log(NEI.06037.vehicle$Emissions),
     main="PM2.5 Emissions Related to Vehicle Emissions by Individual Source\nfor Years 1999-2008, Los Angeles County",
     xlab = "Year", ylab = "log PM2.5 Emmissions (tons)",
     xaxp = c(1999, 2008, 3), pch=20, col=losAngelesColor,
     ylim = c(log(min(NEI.24510.vehicle$Emissions, NEI.06037.vehicle$Emissions)),
              log(max(NEI.24510.vehicle$Emissions, NEI.06037.vehicle$Emissions))))
abline(lm(med.06037 ~ years))

dev.off()

