## Plot3:
## Of the four types of sources indicated by the type (point, nonpoint,
## onroad, nonroad) variable, which of these four sources have seen
## decreases in emissions from 1999–2008 for Baltimore City? Which have
## seen increases in emissions from 1999–2008? Use the **ggplot2**
## plotting system to make a plot answer this question.

## from previous problem, use (fips == "24510")

library(dplyr)
library(ggplot2)
NEI <- readRDS("dataset/summarySCC_PM25.rds")
NEI.24510 <- filter(NEI, fips == "24510")
NEI.24510.bytype <- split(NEI.24510, NEI.24510$type)

# although total emissions aren't asked for specifically in this case,
# the sum from all sources still seems like the best measure in this case
dat <- data.frame(year=numeric(), emissions=double(), type=character())
for (t in names(NEI.24510.bytype)) {
    tbl <- tapply(NEI.24510.bytype[[t]]$Emissions, NEI.24510.bytype[[t]]$year, sum)
    dat <- rbind(dat, data.frame(year=as.numeric(names(tbl)), emissions=as.double(tbl), type=t))
}

# create plot using the ggplot2 system
g <- ggplot(dat, aes(year, emissions)) + geom_point(aes(color=type)) +
    facet_grid(.~type) + geom_smooth(aes(color=type), method="lm") +
    ggtitle("Total PM2.5 Emissions by Source for 1999-2008, United States") +
    labs(x = "Year", y= "PM2.5 Emissions (tons)")
print(g)
ggsave("plot3.png")
