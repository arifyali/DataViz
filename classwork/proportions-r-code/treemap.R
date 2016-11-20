# install.packages("treemap") 
# install.packages("RColorBrewer") ## installing a package only has to be done once


library(treemap)
library(RColorBrewer) ## loading a package must be done every time you want to use the functions that come with that package

campfin <- read.csv("campfin.csv") 

head(campfin)

campfin$Money.Spent.100k <- campfin$Money.Spent/100000


unique(campfin$Organization.Type) ## see all unique organization types

table(campfin$Organization.Type)

## Histogram of spending
hist(campfin$Money.Spent.100k)

## Histogram of spending - removing the outlier
hist(campfin$Money.Spent.100k[which(campfin$Money.Spent.100k < 100)])

## We are going to use a loop to make several graphs (small multiples).

## Single for loop syntax:
for(i in 1:20){print(i)}
## for(iterator in list){code that does something to iterator}
## the iterator, in this case i, goes through the code within the {} as each value in the list. In this case, as 1, then, 2, then 3


## Set up the graphing space to display three grames in one row and three columns:
par(mfrow=c(1,3))

## Three histograms of spending - by organization type
for(i in unique(campfin$Organization.Type)){
	hist(campfin$Money.Spent.100k[campfin$Organization.Type == i],
		xlab = "Campaign Contributions", main = i)
}
## In this case our iterator goes through the list: 
## unique(campfin$Organization.Type)
## So first i is 'Law Firms', then 'Finance', then 'Labor Unions'




## The code below replicates the same effect as the loop:
par(mfrow=c(1,3))
hist(campfin$Money.Spent.100k[campfin$Organization.Type == "Law Firms"],
		xlab = "Campaign Contributions", main = "Law Firms")

hist(campfin$Money.Spent.100k[campfin$Organization.Type == "Finance"],
		xlab = "Campaign Contributions", main = "Finance")

hist(campfin$Money.Spent.100k[campfin$Organization.Type == "Labor Unions"],xlab = "Campaign Contributions", main = "Labor Unions")


par(mfrow=c(1,1)) ## reset to default

## 
treemap(campfin
	,index= c("Organization")
	,vSize= "Money.Spent.100k")

treemap(campfin
	,index= c("Organization.Type","Organization")
	,vSize= "Money.Spent.100k")

treemap(campfin
	,index= c("Organization.Type","Organization")
	,vSize= "Money.Spent.100k"
	,type= "value"
	,vColor= "Percent.For.Democrats")

## Colors: http://www.r-bloggers.com/r-using-rcolorbrewer-to-colour-your-figures-in-r/

## Works best with color variable scaled around 0
campfin$Percent.For.Democrats_ZeroScale <- campfin$Percent.For.Democrats - 0.5

treemap(campfin
	,index= c("Organization.Type","Organization")
	,vSize= "Money.Spent.100k"
	,type= "value"
	,vColor= "Percent.For.Democrats_ZeroScale")


## RColorBrewer - it's great.
## See colors:
display.brewer.all(colorblindFriendly=TRUE)
display.brewer.pal(7,"BrBG")
display.brewer.pal(10,"RdBu")

## Use colors:
brewer.pal(10, "RdBu")

treemap(campfin
	,index= c("Organization.Type","Organization")
	,vSize= "Money.Spent.100k"
	,type= "value"
	,vColor= "Percent.For.Democrats_ZeroScale"
	,palette= brewer.pal(10,"RdBu")
  ,range= c(-0.5, 0.5)
  ,title="Largest Campaign Finance Contributors"
  ,fontsize.labels=8)

## Create color ramp:
blue_ramp <- colorRampPalette(brewer.pal(9,"Blues"))(100)


