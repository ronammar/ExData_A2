# A2 script
# Instructions:
# "Explore the National Emissions Inventory database and see what it say about
# fine particulate matter pollution in the United states over the 10-year period
# 1999–2008. You may use any R package you want to support your analysis."
#
# I have chosen to use ggplot2 for all plotting in A2.

library(dplyr)
library(ggplot2)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Of the four types of sources indicated by the type (point, nonpoint, onroad,
# nonroad) variable, which of these four sources have seen decreases in
# emissions from 1999–2008 for Baltimore City? Which have seen increases in
# emissions from 1999–2008?
totalEmissions <- NEI %>%
  dplyr::filter(fips == "24510") %>%
  group_by(year, type) %>%
  summarise(total=sum(Emissions))

ggplot(totalEmissions, aes(x=year, y=total, col=type, shape=type)) +
  geom_point(size=5) +
  geom_smooth(se=FALSE, method="lm") +
  labs(title="Total emissions in Baltimore City from 1999 to 2008, categorized by source",
       x="Year",
       y= "Total emissions (tons of fine particulate matter)") +
  theme_bw()

ggsave("../plot3.png", device="png", width=9, height=6, units="in")