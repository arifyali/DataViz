setwd("~/Documents/DataViz/")
U.S._Chronic_Disease_Indicators__CDI_ <- read.csv("data/U.S._Chronic_Disease_Indicators__CDI_.csv")
Readmissions_and_Deaths_._Hospital <- read.csv("data/Readmissions_and_Deaths_-_Hospital.csv")
Medicare_Hospital_Spending_by_Claim <- read.csv("data/Medicare_Hospital_Spending_by_Claim.csv")
Age.adjusted_death_rates_1900_2013 <- read.csv("data/NCHS_-_Age-adjusted_death_rates_and_life-expectancy_at_birth___All_Races__Both_Sexes___United_States__1900-2013.csv",  stringsAsFactors=FALSE)
Patient_survey__HCAHPS__._Hospital <- read.csv("data/Patient_survey__HCAHPS__-_Hospital.csv")


medicare_spend = read.csv("data/medicare_spend per enrollee per year/1991.csv", skip = 3, stringsAsFactors = F)[1:52,]
names(medicare_spend) = c("Location", "1991")
medicare_spend[,2] = as.numeric(gsub("\\$", "", medicare_spend[,2]))
for(i in 1992:2009){
  path = paste("data/medicare_spend per enrollee per year/", i, ".csv", sep ="")
  new_data = read.csv(path, skip = 3, stringsAsFactors = F)[1:52,]
  names(new_data) = c("Location", i)
  new_data[,2] = as.numeric(gsub("\\$", "", new_data[,2]))
  medicare_spend = merge(medicare_spend, new_data, by.x = "Location", by.y = "Location")
}
medicare_spend =(t(medicare_spend))
colnames(medicare_spend) = as.vector(medicare_spend[1,])
medicare_spend = as.data.frame(medicare_spend)

medicare_spend = medicare_spend[-1,]
year = as.numeric(row.names(medicare_spend))
medicare_spend = cbind(medicare_spend, year)


library("sqldf")
library("ggplot2")
library("maps")
library("rbokeh")
library("rgdal")
library("maptools")
library("broom")
library("dplyr")
library("ggplot2")
library("ggmap")
library("ggplot2")
library("qcc")
library("maps")
library("rbokeh")


