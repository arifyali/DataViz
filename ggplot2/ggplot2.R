## Introduction to Data Visualization in R

########################
## ggplot2: gg stands for 'Grammar of Graphics' 
## This very popular package that attemps to create a consistent and meaningful syntax (or grammar) that translates data into visualizations.

#### More Resources:
## Extensive Tutorial from Harvard: http://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html
## ggplot2 cheatsheet: https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf
## ggplot2 Documentation: http://docs.ggplot2.org/current/
## Concepts of ggplot2: http://opr.princeton.edu/workshops/201401/ggplot2Jan2014DawnKoffman.pdf
## More on 'Grammar of Graphics': http://byrneslab.net/classes/biol607/readings/wickham_layered-grammar.pdf

## Chapters from 'R for Data Science':
## Data Visualization Chapter: http://r4ds.had.co.nz/data-visualisation.html
## Graphs for Communication Chapter: http://r4ds.had.co.nz/graphics-for-communication.html

########################


########################
### Setup

## Make sure R is updated to the most recent version.
## And that these packages are installed:
install.packages("ggplot2")
install.packages("grid")
install.packages("RColorBrewer")

library(ggplot2)
library(grid)
library(RColorBrewer)

########################
#### Introduction to ggplot2

enr <- read.csv("~/Documents/Georgetown/Data-Visualization-PPOL-646-Fall-2016/ggplot2/enr.csv")

## DC Public Students Enrollment Data

dim(enr) # See numbers of rows and columns
head(enr, n=10) # Look at first ten rows
unique(enr$grade) # See all grades
table(enr$ell_indicator) # Frequency Table
fivenum(enr$read_scale_score) # See five number summary (min, 25th, median, 75th, max)


## Histograms

# ggplot is meant to work with dataframes, and will most often start like this:
ggplot(data=enr)

# Create an aesthetic mapping with aes, but don't graph anything: 
ggplot(data=enr, aes(x=read_scale_score))

# Now add a geometry, aka a visual encoding of the data, in this case: 'geom_histogram'
ggplot(data=enr, aes(x=read_scale_score)) + 
  geom_histogram()

# You could also write this like this:
ggplot(data=enr) + 
  geom_histogram(aes(x=read_scale_score))

# What is important is that the mapping of data to geometries happens within aes()
# You can changes options that are not data-driven within geom_histogram()
ggplot(data=enr, aes(x=read_scale_score)) + 
  geom_histogram(binwidth=0.2, fill="blue")


# ggplot works in layers:
ggplot(data=enr, aes(x=read_scale_score)) + 
  geom_histogram(binwidth=0.2, fill="blue") +
  geom_vline(aes(xintercept=median(read_scale_score)))


## Remember that data-driven changes happen within aes()
# And non data driven changes (see linetipe and size below) do not:
ggplot(data=enr, aes(x=read_scale_score)) + 
  geom_histogram(binwidth=0.2) +
  geom_vline(aes(xintercept=median(enr$read_scale_score)), linetype="dashed", size=1)



## Now with density plots:
ggplot(enr, aes(x=read_scale_score)) + 
  geom_density()

ggplot(enr, aes(x=read_scale_score)) + 
  geom_density(fill="red")

ggplot(enr, aes(x=read_scale_score)) + 
  geom_density(aes(fill=factor(at_risk_3)))

ggplot(enr, aes(x=read_scale_score)) + 
  geom_density(aes(fill=factor(at_risk_3)), size=3, alpha=0.7)

ggplot(enr, aes(x=read_scale_score)) + 
  geom_density(aes(fill=factor(at_risk_3)), size=1, alpha=0.4) + 
  scale_fill_manual(values = c("red","blue","green"))

ggplot(enr, aes(x=read_scale_score)) + 
  geom_density(aes(fill=factor(at_risk_3)), size=1, alpha=0.7) + 
  scale_fill_manual(values = c("red","blue","green")) + 
  ggtitle("Normalized Reading Scores by At Risk Status") + 
  xlab("Normalized Reading Scores") + 
  ylab("Density")

ggplot(enr, aes(x=factor(at_risk_3), y=read_scale_score)) + 
  geom_violin(aes(fill=factor(at_risk_3))) + 
  scale_fill_manual(values = c("red","blue","green"))


## Sample the dataset:
num_rows <- nrow(enr)
enr_lim <- enr[sample(num_rows, num_rows/25),]

# Scatterplot:
ggplot(data=enr_lim, aes(x=read_scale_score, y=math_scale_score)) + 
  geom_point()

# Note how quickly you can switch between similar geometries:
ggplot(data=enr_lim, aes(x=read_scale_score, y=math_scale_score)) + 
  geom_density_2d()

# install.packages("hexbin")
ggplot(data=enr_lim, aes(x=read_scale_score, y=math_scale_score)) + 
  geom_hex()

## Back to the Scatterplot - change transparency with 'alpha':
ggplot(data=enr_lim, aes(x=read_scale_score, y=math_scale_score)) + 
  geom_point(alpha = 0.3)


## Back to the Scatterplot - change transparencywith 'alpha':
ggplot(data=enr_lim, aes(x=read_scale_score, y=math_scale_score)) + 
  geom_point(alpha = 0.3) + 
  ggtitle("DC Student Test Scores")

## Use color to show different groups, based on the data:
ggplot(data=enr_lim, aes(x=read_scale_score, y=math_scale_score)) + 
  geom_point(aes(color= factor(atrisk)), alpha = 0.3) +
  ggtitle("DC Student Test Scores")

## Add another geom layet of a local average:
ggplot(data=enr_lim, aes(x=read_scale_score, y=math_scale_score)) + 
  geom_point(aes(color= factor(atrisk)), alpha = 0.3) +
  geom_smooth() +
  ggtitle("DC Student Test Scores")

  
## Note the inheretence of aesthetics. Above, geom_smooth inherents the x and y aesthetics, but not color. Below, geom_smooth gets all three:
ggplot(data=enr_lim, aes(x=read_scale_score, y=math_scale_score, color= factor(atrisk))) + 
    geom_point(alpha = 0.3) +
    geom_smooth() + 
    ggtitle("DC Student Test Scores")
  
  




## Creating an ordered categorical variable with factor()
enr$grade <- factor(enr$grade, levels = c("PK3","PK4","KG","2","3","4","5","6","7","8","9","10","11","12","AO","UN"))

# Bar charts:
ggplot(data=enr, aes(factor(grade))) + geom_bar()

# Stacked bar chart - use 'fill' for color:
ggplot(data=enr, aes(factor(grade), fill=ell_indicator)) + geom_bar()
ggplot(data=enr, aes(factor(grade), fill=ell_indicator)) + geom_bar() + coord_flip()

# Paired Bar Chart (set position="dodge")
ggplot(data=enr, aes(factor(grade), fill=ell_indicator)) + geom_bar(position="dodge")








