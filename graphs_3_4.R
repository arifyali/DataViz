setwd("~/Documents/DataViz/")
source("data_sources.R")

par(mfrow =c(1,1))

Age.adjusted_death_rates_1900_2013 = Age.adjusted_death_rates_1900_2013[!is.na(Age.adjusted_death_rates_1900_2013$Average.Life.Expectancy), ]
par(mfrow = c(3,1))

for(i in unique(Age.adjusted_death_rates_1900_2013$Sex)){
  plot(Average.Life.Expectancy~Year, 
       data = Age.adjusted_death_rates_1900_2013[Age.adjusted_death_rates_1900_2013$Race== "All Races" & 
                                                   Age.adjusted_death_rates_1900_2013$Sex == i, ], 
       type = "l",lwd= 2, col = "black", main = i,
       ylim = c(min(Age.adjusted_death_rates_1900_2013$Average.Life.Expectancy)-10, max(Age.adjusted_death_rates_1900_2013$Average.Life.Expectancy)+10))
  
  points(Average.Life.Expectancy~Year, data = Age.adjusted_death_rates_1900_2013[Age.adjusted_death_rates_1900_2013$Race== "Black" & Age.adjusted_death_rates_1900_2013$Sex == i, ], type = "l",lwd= 2, col = "#FF0000FF")
  
  points(Average.Life.Expectancy~Year, data = Age.adjusted_death_rates_1900_2013[Age.adjusted_death_rates_1900_2013$Race== "White" & Age.adjusted_death_rates_1900_2013$Sex == i, ], type = "l",lwd= 2, col = "blue")
  
}
legend(x = "bottomright", legend = c("All Races", "Black", "White"), lwd = 2, col = c("Black", "#FF0000FF", "Blue"))

### Plot 4
kff_medicare_data <- read.delim("data/kff_medicare_data.csv")

medicare_rep_total = as.numeric(as.character(kff_medicare_data$Total))
for(i in 2:ncol(kff_medicare_data)){
  kff_medicare_data[,i] = as.numeric(as.character(kff_medicare_data[,i]))/as.numeric(as.character(kff_medicare_data[,ncol(kff_medicare_data)]))
}
kff_medicare_data$Total = medicare_rep_total

kff_medicare_data[is.na(kff_medicare_data)] <- 0
# Kaiser Family Foundation estimates based on the Census Bureau's March 2016 Current Population Survey (CPS: Annual Social and Economic Supplements
Medicare_Spending <- read.delim("data/Total Medicare Spending by State.csv")
# Centers for Medicare &amp; Medicaid Services (2011). _Health Expenditures by State of Residence._Retrieved (December 2011) at [http://www.cms.gov/NationalHealthExpendData/downloads/resident-state-estimates.zip.](http://www.cms.gov/NationalHealthExpendData/downloads/resident-state-estimates.zip)	

kff_medicare_data = sqldf("SELECT * FROM kff_medicare_data
                          JOIN Medicare_Spending
                          ON kff_medicare_data.Location = 
                          Medicare_Spending.Location")

kff_medicare_data = kff_medicare_data[-1,]
library(ggplot2)
names(kff_medicare_data) = c("State", "Percentage_of_White_Receipients", "Percentage_of_Black_Receipients",
                             "Hispanic", "Other", "total", "State", "Medicare_Spend_per_Residence")
kff_medicare_data$colors = "black"
kff_medicare_data$colors[kff_medicare_data$Percentage_of_White_Receipients>0.8 & kff_medicare_data$Percentage_of_Black_Receipients<0.1] = "#FF000099"



medicare <- ggplot() + 
  geom_point(data = kff_medicare_data[kff_medicare_data$colors == "black", ], aes(x = Percentage_of_White_Receipients, y = Percentage_of_Black_Receipients, size = Medicare_Spend_per_Residence,  label=State)) +
  geom_point(data = kff_medicare_data[kff_medicare_data$colors == "#FF000099", ], aes(x = Percentage_of_White_Receipients, y = Percentage_of_Black_Receipients, size = Medicare_Spend_per_Residence,  label=State), color = "#8B0000") +
  ggtitle("States with Insignificant Proportions of Black Recipients of Medicare relative to Whiten Recipients tend to spend less per Resident")+
  labs(x = "Percentage of White Receipients", y = "Percentage of Black Receipients", subtitle = "TEST") + 
  #axis.title.x="Percentage of White Receipients",
  #axis.title.y="Percentage of Black Receipients",
  theme(
        axis.ticks.x=element_blank(),
        
        axis.ticks.y=element_blank(),
        plot.subtitle=element_text(size=.5, hjust=0.5, face="italic", color="black"))
ggplotly(medicare)


ggplot(data = kff_medicare_data_no_outliers, aes(x = White, y = Black, size = Total.Medicare.Spending.by.Residence)) + geom_point(color = "#FF000099")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

