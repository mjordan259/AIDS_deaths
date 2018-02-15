# The Gapminder website contains over 500 data sets with information about
# the world's population. Your task is to continue the investigation you did at the
# end of Problem Set 3 or you can start fresh and choose a different
# data set from Gapminder.

# If you’re feeling adventurous or want to try some data munging see if you can
# find a data set or scrape one from the web.

# In your investigation, examine pairs of variable and create 2-5 plots that make
# use of the techniques from Lesson 4.

library(dplyr)
library(tidyr)
library(ggplot2)

master <- read.csv('Indicator_Annual number of AIDS deaths.csv', check.names = F)

tidymaster <- gather(master, "year", "n", '1990':'2011', na.rm=TRUE)


# this compares just 1990 to 2000 for countries above the mean in 1990

master2 <- subset(master, select = c('Country', '1990', '2000'))
master2 <- na.omit(master2)

summary(master2)

master2<-subset(master2, master2$`1990` > 2150)

ggplot(data=master2, aes(x=Country, y=master2$`1990`)) + geom_bar(stat='identity')

library(reshape2)
master2long <- melt(master2, id.var = "Country")

ggplot(master2long, aes(x = Country, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "dodge") 

# and this just adds on 2010. Countries with NA for any year surveyed have been omitted.

master3 <- subset(master, select = c('Country', '1990', '2000', '2010'))
master3 <- na.omit(master3)

master3<-subset(master3, master3$`1990` > 2150)

master3long <- melt(master3, id.var = "Country")

ggplot(master3long, aes(x = Country, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "dodge") 

summary(master$`1990`)
summary(master$`1991`)

# this looks at the changes in AIDS deaths between Uganda (the max rate in 1990) and Mozambique (the mean rate in 1990)

comparison <- subset(master, master$Country == 'Uganda' | master$Country == 'Mozambique')

comparisonlong <- melt(comparison, id.var = "Country")
ggplot(aes(x = variable, y = value, color=Country), data=comparisonlong) + geom_point(stat = 'identity')

# this is the same as above, except it looks at the country with the max rate in 2011

maxcomparison <- subset(master, master$Country == 'Uganda' | master$Country == 'South Africa')

maxcomparisonlong <- melt(maxcomparison, id.var = "Country")
ggplot(aes(x = variable, y = value, color=Country), data=maxcomparisonlong) + geom_point(stat = 'identity')


fiveyear <- subset(master, select = c('Country', '1990', '1995', '2000', '2005', '2010'))

fiveyear <- gather(fiveyear, "year", "n", '1990':'2010', na.rm=TRUE)

#here's a broadly useless scatterplot looking at all countries every five years
ggplot(aes(x=year, y=n), data=fiveyear) + 
  geom_jitter(aes(color=Country))

summary(fiveyear)

#here's the same as above, but only looking at countries above the mean

ggplot(aes(x=year, y=n), data=subset(fiveyear, fiveyear$n > 8320)) + 
  geom_jitter(aes(color=Country))

# this adds columns splitting countries into Quartiles

pf$year_joined.bucket <- cut(pf$year_joined, c(2004, 2009, 2011, 2012, 2014))

fiveyear$quartile <- cut(fiveyear$n, c(60, 350, 3100, 370000))

fiveyear$quartile <- as.character(fiveyear$quartile)
fiveyear$quartile[fiveyear$quartile == "(350,3.1e+03]"] <- "Quartile 2"
fiveyear$quartile[fiveyear$quartile == "(60,350]"] <- "Quartile 1"
fiveyear$quartile[fiveyear$quartile == "(3.1e+03,3.7e+05]"] <- "Quartile 4"
fiveyear$quartile[fiveyear$quartile == "(350,3.1e+03"] <- "Quartile 3"

# and this is the five-year scatterplot for all countries, faceted into quartiles

ggplot(aes(x=year, y=n), data=fiveyear) + 
  geom_jitter(aes(color=Country)) +
  facet_wrap( ~ quartile)