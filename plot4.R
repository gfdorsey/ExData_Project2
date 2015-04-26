## Plot 4:
## Across the United States, how have emissions from coal combustion-related
## sources changed from 1999–2008?

## TA Note: This might work for grepping SCC$EI.Sector -
## "^fuel comb -(.*)- coal$" (ignore.case=T).

NEI <- readRDS("dataset/summarySCC_PM25.rds")
SCC <- readRDS("dataset/Source_Classification_Code.rds")

coal.sources <- grepl("^fuel comb.*coal$", SCC$EI.Sector, ignore.case=TRUE)
coal.sources <- unique(SCC$SCC[coal.sources])
NEI.coal <- NEI[NEI$SCC %in% coal.sources,]

tbl <- tapply(NEI.coal$Emissions, NEI.coal$year, sum)
years <- as.numeric(attributes(tbl)$dimnames[[1]])
sums <- as.vector(tbl)

# predict regression line and confidence intervals
model <- lm(sums ~ years)
r2 <- summary(model)$r.squared
years.range <- seq(min(years), max(years), by=9.0/100.0)
sums.pred <- predict(model, newdata = data.frame(years=years.range), 
                     interval = 'confidence')

# create plot
coalColor <- rgb(0.15, 0.15, 0.3, 0.4)
png(filename = "plot4.png", width = 480, height = 960)
par(mfrow = c(2,1), mar = c(4,4,4,2), cex = 0.8)
plot(years, sums, 
     main="Total PM2.5 Emissions Related to Coal Combustion\nfor Years 1999-2008, United States",
     xlab = "Year", ylab = "PM2.5 Emmissions (tons)",
     type="n", xaxp = c(1999, 2008, 3),
     ylim=c(min(sums.pred[,2]), max(sums.pred[,3])))

# draw confidence intervals and regression line
polygon(c(rev(years.range), years.range), c(rev(sums.pred[ ,3]),
                                            sums.pred[ ,2]), col = 'grey90', border = NA)
lines(sums.pred[,1] ~ years.range, lwd = 2, lty="dashed", col="slategray")
lines(sums.pred[,2] ~ years.range, lwd = 2, lty = "dotted", col = "slategray")
lines(sums.pred[,3] ~ years.range, lwd = 2, lty = "dotted", col = "slategray")

# plot points last so they are visible over the confidence interval
points(years, sums, pch = 16, col = "slategray")

# show r squared value for reference
legend("topright", bty="n", legend=paste("r^2 =", format(r2, digits=4)))

# in addition to total emissions show individual sources
plot(NEI.coal$year, NEI.coal$Emissions,
     main="PM2.5 Emissions Related to Coal Combustion by Source\nfor Years 1999-2008, United States",
     xlab = "Year", ylab = "PM2.5 Emmissions (tons)",
     xaxp = c(1999, 2008, 3), pch=20, col=coalColor)

dev.off()
