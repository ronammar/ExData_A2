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

# Across the United States, how have emissions from coal combustion-related 
# sources changed from 1999–2008?
totalEmissions <- NEI %>%
  left_join(SCC, by="SCC") %>%
  dplyr::filter(str_detect(EI.Sector, ignore.case("coal"))) %>%
  group_by(year) %>%
  summarise(total=sum(Emissions))

ggplot(totalEmissions, aes(x=year, y=total)) +
  geom_point(size=5) +
  geom_smooth(method="lm") +
  labs(title="Total coal emissions from 1999 to 2008",
       x="Year",
       y= "Total emissions (tons of fine particulate matter)") +
  theme_bw()

ggsave("../plot4.png", device="png", width=9, height=6, units="in")