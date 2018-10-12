# Name: Nimna Rodrigo
# Date: September 12, 2018

# Run the below only if the library is not already installed.
# install.packages(dslabs)

library(dslabs)
library(dplyr)
library(tidyverse)
data(us_contagious_diseases)

# Question 1.

# Creates an object called dat that stores only Measles data collected from all states excluding Alaska and Hawaii. 

dat <- filter(us_contagious_diseases, disease == "Measles", state != "Alaska", state != "Hawaii")

# Rate equation (per 100,000 people rate). 

dat <- mutate(dat, rate = ((count*100000)/population)* (weeks_reporting/52))

# Question 2.

# Plotting the Measles disease rates per year for California. 

dat_cal <- filter(dat, disease == "Measles", state == "California")

# Adding a vertical line to the plot to show the year 1965 and its relation to the data. 

ggplot(data = dat_cal, mapping = aes(x = year, y = rate)) + geom_line() + geom_vline(xintercept = 1965)

# Question 3.

# Creates an object to show U.S. contagious diseases data for the state of California. 

dat_caliFocus <- filter(us_contagious_diseases, state == "California")

# Narrows data down to the 1950’s, 1960’s, and 1970’s. 

dat_caliFocus$yearBlock[dat_caliFocus$year == 1950] <- "1950’s"
dat_caliFocus$yearBlock[dat_caliFocus$year == 1960] <- "1960’s"
dat_caliFocus$yearBlock[dat_caliFocus$year == 1970] <- "1970’s"

# Plots a bar graph to display data (without square root counts). 

ggplot(data = dat_caliFocus ) + geom_bar(mapping = aes(x = state, y = count, fill = yearBlock), position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=-0.01))

# Plots a bar graph to display data (with square root counts). 

ggplot(data = dat_caliFocus ) + geom_bar(mapping = aes(x = state, y = sqrt(count), fill = yearBlock), position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=-0.01))

# Question 4.

# Creates an object to show U.S. contagious diseases data for all states. 

dat_allStates <- filter(us_contagious_diseases)

# Narrows data down to the 1950’s, 1960’s, and 1970’s. 

dat_allStates$yearBlock[dat_allStates$year == 1950] <- "1950’s"
dat_allStates$yearBlock[dat_allStates$year == 1960] <- "1960’s"
dat_allStates$yearBlock[dat_allStates$year == 1970] <- "1970’s"

# Plots a bar graph to display data. 

ggplot(data = allStates) + geom_bar(mapping = aes(x = state, y = count, fill = yearBlock), position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=-0.01))

# Question 5.

# Rate equation (per 100,000 people rate). 

dat_allStates <- mutate(dat_allStates, rate = ((count*100000)/population)* (weeks_reporting/52))

# Plotting a graph to display data. 

ggplot(data = dat_allStates) + geom_bar(mapping = aes(x = state, y = count, fill = yearBlock), position = "dodge", stat = "identity") + geom_tile(mapping = aes(x = state, y = count, color = rate)) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=-0.01))

# Question 6.

# I got this data from the website: https://www.cdc.gov/ncbddd/autism/data.html. 

autismData <- read.csv("~/Desktop/classDocs/labs/04_lab/04-i-lab-cs301-fall-2018-nimnar/autismData.png")

# From this data, I can conclude that autism rates have increased, but this does not seem to coincide with the introduction of vaccines. 