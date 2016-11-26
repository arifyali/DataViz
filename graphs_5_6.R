setwd("~/Documents/DataViz/")
source("data_sources.R")

### Plot 5
Healthcare_Expenditures_by_State <- read.delim("data/Health Care Expenditures by State of Residence.csv")
library(qcc)
states = as.character(Healthcare_Expenditures_by_State[,1])
Healthcare_Expenditures_by_State = Healthcare_Expenditures_by_State$Total.Health.Spending
names(Healthcare_Expenditures_by_State) = states
pareto.chart(Healthcare_Expenditures_by_State[-1], cex.xlab = .5,srt=45)

######### Plot 6
bad_teens = U.S._Chronic_Disease_Indicators__CDI_[U.S._Chronic_Disease_Indicators__CDI_$QuestionID == "ALC1_1", c("DataValueAlt","Question", "LocationAbbr")]
names(bad_teens) = c("Percent_teen_drink", "Question", "State")

really_bad_teens = U.S._Chronic_Disease_Indicators__CDI_[U.S._Chronic_Disease_Indicators__CDI_$QuestionID == "ALC2_1", c("DataValueAlt","Question", "LocationAbbr")]
names(really_bad_teens) = c("Percent_teen_bing_drink", "Question", "State")

teen_drinking = sqldf("SELECT * FROM bad_teens
                      JOIN really_bad_teens
                      ON bad_teens.State = really_bad_teens.State")
lm_teen_drinking = lm(Percent_teen_bing_drink~Percent_teen_drink, data = teen_drinking)

teen_drinking = na.omit(teen_drinking)
cor(teen_drinking$Percent_teen_drink, teen_drinking$Percent_teen_bing_drink)
ggplot(data = teen_drinking, aes(x = Percent_teen_drink, y = Percent_teen_bing_drink)) + 
  geom_point(col = "grey20") + 
  geom_abline(slope = lm_teen_drinking$coefficients[2], intercept = lm_teen_drinking$coefficients[1], col = "#FF000099")
