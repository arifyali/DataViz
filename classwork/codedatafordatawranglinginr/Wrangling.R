## Data Wrangling with R
## 10/31/2016
## Alex C. Engler

## Data:
# National Survey on Drug Use and Health (2012)
# Source: http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/34933?q=&paging.rows=25&sortBy=10

########  More Resources ######## 
## R for Data Science Chapters 9-16 on Data Wrangling: http://r4ds.had.co.nz/
## Data Wrangling Cheat Sheet: https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
########  ########  ########  ######## 


drugs <- read.csv(file.choose(), header=TRUE) ## load the data
## drugs <- read.csv("NSDUH.csv")

dim(drugs) ## check data dimensions (rows, columns)
head(drugs) ## look at the first six rows of data
summary(drugs) ## gives five number summary for continuous variables

table(drugs$HEALTH, useNA="always") ## frequency table of the HEALTH column
table(drugs$AGE2, useNA="always") ## frequency table of the AGE2 column

hist(drugs$AGE2) # quick histogram


## Subsetting data:
example <- subset(drugs, CIGEVER == "No")

drugs <- subset(drugs, AGE2 %in% c(1,2,3,4,5,6,7,8,9,10)) ## subset out older age categories

# drugs <- subset(drugs, AGE2 < 11)
# drugs <- subset(drugs, AGE2 <= 10)
# drugs <- subset(drugs, AGE2 %in% 1:10)




drugs$AGE2 <- drugs$AGE2 + 11 ## change age
table(drugs$AGE2) ## check to see if that worked
hist(drugs$AGE2) ## histogram of the above table


table(drugs$MJEVER) 
prop.table(table(drugs$MJEVER)) ## Proportional Frequency Table


## Categorical variables must be factors to work. Factors are how you define the possible set of categorical variables. All values not in the set will be coerced into NA, or missing (which is fine).
drugs$MJEVER <- factor(drugs$MJEVER, levels = c("Yes","No"))
drugs$CIGEVER <- factor(drugs$CIGEVER, levels = c("Yes","No"))
drugs$SNFEVER <- factor(drugs$SNFEVER, levels = c("Yes","No"))
drugs$CIGAREVR <- factor(drugs$CIGAREVR, levels = c("Yes","No"))
drugs$ALCEVER <- factor(drugs$ALCEVER, levels = c("Yes","No"))
drugs$IRSEX <- factor(drugs$IRSEX, levels = c("Male","Female"))





## The Tidyverse - a series of packages invented by Hadley Wickham that make R a lot easier to use. These include dplyr, ggplot2, tidyr, lubridate, stringr, broom etc.

## dplyr 
# Convenient easy functions for subseting, reordering, adding columns, simple aggregations, simple randoms sampling
# Introduction here: https://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html
# install.packages("dplyr") 
library(dplyr)

glimpse(drugs)

# Simple functions for data manipulation:
new_data <- filter(drugs, CIGEVER == "Yes")
new_data <- group_by(new_data, IRSEX)
new_data <- summarize(new_data, 
                      avg_education = mean(IREDUC2),
                      median_health = median(HEALTH, na.rm=TRUE))
new_data <- arrange(new_data, -avg_education)


# Can chain those operations together with the %>% operator:

new_data2 <- drugs %>%
	filter(CIGEVER == "Yes") %>%
	group_by(IRSEX) %>%
	summarize(avg_education = mean(IREDUC2), 
	          median_health = median(HEALTH, na.rm=TRUE)) %>%
	arrange(-avg_education)


## These are exactly the same:
new_data == new_data2






## These chains can be extended to ggplot2 - a powerful graphics package.
# install.packages("ggplot2")
library(ggplot2)

drugs %>%
	filter(CIGEVER == "Yes") %>%
	group_by(HEALTH) %>%
	summarize(avg_education = mean(IREDUC2)) %>%
	arrange(-avg_education)  %>%
	ggplot(aes(x=HEALTH, y=avg_education)) + 
		geom_bar(stat="identity", fill="#00AEEF") + 
		theme(panel.background = element_rect(fill = "#9D9FA2")) + ggtitle("Average Education Level by Health Outcomes")


drugs %>%
  group_by(AGE2, CIGEVER) %>%
  summarize(avg_education = mean(IREDUC2)) %>%
  arrange(-avg_education)  %>%
  ggplot(aes(x=avg_education, y=AGE2, color=CIGEVER)) + 
  geom_point(size=5) + 
  theme(panel.background = element_rect(fill = "#9D9FA2")) + ggtitle("Average Education Level by Age and Cigarette Usage")



## Reshaping data with tidyr:


library(tidyr)
data(airquality) ## load a default R dataset

head(airquality)


## Wide to Long:
airquality_long <- airquality %>%
  gather(measure, value, c(Ozone,Solar.R,Wind,Temp))
head(airquality_long, n=100)                 

table(airquality_long$measure)


## Long to wide:
airquality_wide <- airquality_long %>%
  spread(measure, value)

head(airquality_wide)


## Working with Strings:
a <- "test me"
b <- 8 + 9

## test to see if an object is a character string:
is.character(a)
is.character(b)

class(a)
class(b)

## You can coerce numerics into characters:
b <- as.character(b)
is.character(b)
class(b)

## Change case:
toupper("letters")
tolower("LETTERS")

## Simple substitution:
txt <- c("arm","foot","lefroo", "bafoobar")
grep("foo", txt) ## find 'foo'
gsub("foo", "bar", txt) ## replace 'foo' with 'bar'
## More on pattern replacement here: http://www.inside-r.org/r-doc/base/sub






## stringr
# Modern and consistent processing of character strings.
# Introduction here: http://journal.r-project.org/archive/2010-2/RJournal_2010-2_Wickham.pdf
# install.packages("stringr")
library(stringr)

strings <- c("   219 733 8965", 
	"329-293-8753    ", 
	"banana", 
	"387 287 6718", 
	"apple", 
	"233.398.9187 ", 
    "842 566 4692", 
    "    Work: 579-499-7527    ", 
    "$1000", 
    "Home: 543.355.3679")

str_trim(strings) ## Remove whitespace from start and end of string
str_trim("   String with trailing and leading white space \t", side="right")
str_trim("   String with trailing and leading white space \t", side="left")

str_detect(strings, "5") ## Is there a '5' in this string?

str_detect(strings, "a") ## Is there an 'a' in this string?
str_locate(strings, "a") ## Where is the first 'a' in this string?
str_locate_all(strings, "a") ## Where are all the 'a's in this string?

## Makes use of regular expressions:
# Regular expressions in R reference: http://www.regular-expressions.info/rlanguage.html
str_detect(strings, "\\s") ## Is there any whitespace in this string?
str_detect(strings, "\\$")  ## Is there a '$' in this string?

phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"
str_detect(strings, phone) ## Is this string a phone number?
str_locate(strings, phone) ## Where is the phone number in this string?






## lubridate
# Working quickly and easily with dates.
# Introduction here: http://www.r-statistics.com/2012/03/do-more-with-dates-and-times-in-r-with-lubridate-1-1-0/
# install.packages("lubridate")
library(lubridate)

## Use 'y' 'm' and 'd' to specify years, months, days and standardize different data formats:
ymd("20110604")
mdy("06-04-2011")
dmy("04/06/2011")

## Add 'h', 'm', 's' for hours, minutes, and seconds:
ymd_hms("2011-06-04 12:00:00")

## Easily calculate differences across timezones:
leave <- ymd_hms("2011-08-09 6:15:00", tz = "Europe/London")
arrive <- ymd_hms("2011-08-10 14:40:00", tz = "Pacific/Auckland")
arrive - leave

## Check weekday:
wday(arrive)
wday(arrive, label=TRUE)

## Intervals:
int1 <- interval(ymd(20150720, tz = "America/New_York"), mdy("08/06/2015", tz = "America/New_York"))
int2 <- interval(ymd(20150801, tz = "America/New_York"), mdy("08/12/2015", tz = "America/New_York"))
int_overlaps(int1, int2) ## Do these intervals overlap?

int1/ddays(1) ## How many days is interval 1?
int2/dminutes(1) ## How many minutes is interval 2?

## Check for leap years:
leap_year(2011) 
leap_year(2012)
