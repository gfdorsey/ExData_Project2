## Plot 2:
## Have total emissions from PM2.5 decreased in the Baltimore City,
## Maryland (fips == "24510") from 1999 to 2008? Use the **base** plotting
## system to make a plot answering this question.

## confidence intervals based on code from
## http://stackoverflow.com/questions/14069629/plotting-confidence-intervals

library(dplyr)
NEI <- readRDS("dataset/summarySCC_PM25.rds")
NEI.24510 <- filter(NEI, fips == "24510")

# need to create sums by year to calculate total emissions
tbl <- tapply(NEI.24510$Emissions, NEI.24510$year, sum)
years <- as.numeric(attributes(tbl)$dimnames[[1]])
sums <- as.vector(tbl)

# predict regression line and confidence intervals
model <- lm(sums ~ years)
r2 <- summary(model)$r.squared
years.range <- seq(min(years), max(years), by=9.0/100.0)
sums.pred <- predict(model, newdata = data.frame(years=years.range), 
                 interval = 'confidence')

# create plot
png(filename = "plot2.png", width = 480, height = 480)
par(mfrow = c(1,1), mar = c(4,4,4,2), cex = 0.8)
plot(years, sums, main="Total PM2.5 Emissions by Year, 1999-2008, Baltimore City",
     xlab = "Year", ylab = "PM2.5 Emissions (tons)", type = "n",
     xaxp = c(1999, 2008, 3),
     ylim=c(min(sums.pred[,2]), max(sums.pred[,3])))

# draw confidence interval and regression line
polygon(c(rev(years.range), years.range), c(rev(sums.pred[ ,3]),
     sums.pred[ ,2]), col = "grey90", border = NA)
lines(sums.pred[,1] ~ years.range, lwd = 2, lty="dashed", col="darkorange3")
lines(sums.pred[,2] ~ years.range, lwd = 2, lty = 'dotted', col = 'darkorange3')
lines(sums.pred[,3] ~ years.range, lwd = 2, lty = 'dotted', col = 'darkorange3')

# plot points last so they are visible over the confidence interval
points(years, sums, pch = 16, col = "darkorange3")

# show r squared value for reference
legend("topright", bty="n", legend=paste("r^2 =", format(r2, digits=4)))
dev.off()





