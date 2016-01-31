# A2 script
# Instructions:
# "Explore the National Emissions Inventory database and see what it say about
# fine particulate matter pollution in the United states over the 10-year period
# 1999–2008. You may use any R package you want to support your analysis."
#
# I have chosen to use ggplot2 for all plotting in A2.

library(dplyr)
library(ggplot2)
library(stringr)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Compare emissions from motor vehicle sources in Baltimore City with emissions 
# from motor vehicle sources in Los Angeles County, California (fips == "06037").
# Which city has seen greater changes over time in motor vehicle emissions?
totalEmissions <- NEI %>%
  left_join(SCC, by="SCC") %>%
  dplyr::filter(fips %in% c("06037", "24510") &
                str_detect(EI.Sector, ignore.case("vehicle"))) %>%
  group_by(year, fips) %>%
  summarise(total=sum(Emissions))

cities <- data.frame(fips=c("06037", "24510"),
                     city=c("Los Angeles", "Baltimore City"))
totalEmissions <- left_join(totalEmissions, cities, by="fips")

# Build the linear models and extract the slopes for annotation of the graph.
baltimore <- filter(totalEmissions, fips == "24510")
losangeles <- filter(totalEmissions, fips == "06037")
baltimoreCoef <- coef(lm(baltimore$total ~ baltimore$year))["baltimore$year"]
losangelesCoef <- coef(lm(losangeles$total ~ losangeles$year))["losangeles$year"]

# With a little geom_text syntax help from the following source:
# https://trinkerrstuff.wordpress.com/2012/09/01/add-text-annotations-to-ggplot2-faceted-plot/
labelData <- data.frame(x=rep(2002, 2),
                        y=c(3000, 1000),
                        city=c("Los Angeles", "Baltimore City"),
                        facetLabel=paste("slope",
                                         format(c(losangelesCoef, baltimoreCoef),
                                                digits=4,
                                                trim=TRUE)))

ggplot(totalEmissions, aes(x=year, y=total)) +
  geom_point(size=5) +
  geom_smooth(method="lm") +
  facet_grid(~ city) +
  geom_text(data=labelData, aes(x, y, label=facetLabel, group=NULL)) +
  labs(title="Total motor vehicle emissions from 1999 to 2008",
       x="Year",
       y= "Total emissions (tons of fine particulate matter)") +
  theme_bw()

ggsave("../plot6.png", device="png", width=9, height=6, units="in")