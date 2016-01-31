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

# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (𝚏𝚒𝚙𝚜 == "𝟸𝟺𝟻𝟷𝟶") from 1999 to 2008?
totalEmissions <- NEI %>%
  dplyr::filter(fips == "24510") %>%
  group_by(year) %>%
  summarise(total=sum(Emissions))

ggplot(totalEmissions, aes(x=year, y=total)) +
  geom_point(aes(col=as.factor(year)), size=5) +
  guides(color=FALSE) +
  geom_text(aes(label=year), nudge_x=0.5) + 
  geom_smooth(method="lm") +
  labs(title="Total emissions in Baltimore City from 1999 to 2008",
       x="Year",
       y= "Total emissions (tons of fine particulate matter)") +
  theme_bw()

ggsave("../plot2.png", device="png", width=9, height=6, units="in")