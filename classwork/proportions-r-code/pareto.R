install.packages("qcc")
library(qcc)


## Source: http://www.bls.gov/news.release/pdf/empsit.pdf
## The pareto.chart function naturally uses a named vector
jobs <- c(46000,39000,38000,35000,33000,26000,22000,6000)
sectors <- c("Retail Trade","Construction","Health Care","Finance","Food and Drink Services","Prof&Tech Services","Manufacturing","Other")

names(jobs) <- sectors
jobs # Named vector.

## Pie Chart:
pie(jobs, labels = names(jobs), col='#EEDDCC', border="#FFFFFF")
pie(jobs, labels = names(jobs), col='#EEDDCC', border="#FFFFFF", clockwise=TRUE)


## Donut Chart:
pie(jobs, labels = names(jobs) , col='#EEDDCC', border="#FFFFFF")
symbols(0, 0, circles = c(0.5), add=TRUE, bg="white")



pareto.chart(jobs)

## If you wanted to do this from a dataset, use this pseudo-code:
# list1 <- df$value_column
# names(list1) <- df$name_column


jobs_fake <- c(86000,79000,18000,15000,13000,6000,2000,600)
names(jobs_fake) <- sectors

pareto.chart(jobs_fake)


## Change some simple options.
pareto.chart(jobs_fake, cex.names = 0.75, col = "black")
pareto.chart(jobs_fake, cex.names = 0.75, col = "black", xlab = "Sector", ylab = "Jobs Gained")

?pareto.chart

## Charts in R allow for layering:
abline(h=(sum(jobs_fake)*0.75))
abline(h=(sum(jobs_fake)*0.75),col="red",lwd=4)


## Scale Y Axis for easier reading
jobs_1000 <- jobs/1000
pareto.chart(jobs_1000, cex.names = 0.75, col = "black", xlab = "Sector", ylab = "Jobs Gained (Thousands)")


