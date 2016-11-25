setwd("~/Documents/DataViz/")
source("data_sources.R")
### Plot 11
par(mfrow = c(1,1))
tobacco <- U.S._Chronic_Disease_Indicators__CDI_[which(U.S._Chronic_Disease_Indicators__CDI_$Topic == "Tobacco"), ]

youth_tobacco_smoke = tobacco[which(tobacco$Question == "Current cigarette smoking among youth"),c("LocationDesc","DataValue")]
names(youth_tobacco_smoke) = c("State", "Percent_Youth_Smoke")

youth_tobacco_smokeless = tobacco[which(tobacco$Question == "Current smokeless tobacco use among youth"),c("LocationDesc","DataValue")]
names(youth_tobacco_smokeless) = c("State", "Percent_Youth_tobacco_(smokeless)")

License_requirement = tobacco[which(tobacco$Question == "States with strong polices that require retail licenses to sell tobacco products"),c("LocationDesc","DataValue")]
names(License_requirement) = c("State", "License_requirement")

youth_tobacco = sqldf("SELECT * FROM youth_tobacco_smoke
                      JOIN youth_tobacco_smokeless
                      ON youth_tobacco_smoke.State = youth_tobacco_smokeless.State
                      JOIN License_requirement
                      ON youth_tobacco_smokeless.State = License_requirement.State")
youth_tobacco$Percent_Youth_Smoke = as.numeric(as.character(youth_tobacco$Percent_Youth_Smoke))
youth_tobacco$`Percent_Youth_tobacco_(smokeless)` = as.numeric(as.character(youth_tobacco$`Percent_Youth_tobacco_(smokeless)`))

youth_tobacco_license = youth_tobacco[which(youth_tobacco$License_requirement == "Yes"),]

ggplot(data = youth_tobacco, aes(x = Percent_Youth_Smoke, y = `Percent_Youth_tobacco_(smokeless)`)) + 
  geom_point(col = "grey") +
  geom_point(data = youth_tobacco_license, aes(x = Percent_Youth_Smoke, y = `Percent_Youth_tobacco_(smokeless)`), col = "#FF000099") +
  ggtitle("Requiring Licenses to Sell Cigerattes or other Tobacco Products does not affect Youth Tobacco Use") +
  xlab("Percent of Youth that are smoking") + ylab("Percent of Youth that are using smokeless tobacco products")

### Plot 12
medicare_spend = read.csv("medicare_spend per enrollee per year/1991.csv", skip = 3, stringsAsFactors = F)[1:52,]
names(medicare_spend) = c("Location", "1991")
medicare_spend[,2] = as.numeric(gsub("\\$", "", medicare_spend[,2]))
for(i in 1992:2009){
  path = paste("medicare_spend per enrollee per year/", i, ".csv", sep ="")
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


spend_medicare = medicare_spend[, c("Alabama", "year")]
spend_medicare$State = "Alabama"
names(spend_medicare) = c("medicare_expenditure", "year", "state")
for(i in names(medicare_spend)[-c(1, 53)]){
  state = medicare_spend[, c(i, "year")]
  state$State = i
  names(state) = c("medicare_expenditure", "year", "state")
  spend_medicare = rbind(spend_medicare, state)
}

figure(title = "Medicare Spending per recipent from 1992-2009", 
       ylab = "Medicare Expenditure") %>%
  ly_lines(year,medicare_expenditure, group = state,data = spend_medicare)

spend_medicare = medicare_spend[, c("Alabama", "year")]
spend_medicare$State = "Alabama"
names(spend_medicare) = c("medicare_expenditure", "year", "state")
for(i in names(medicare_spend)[-c(1, 53)]){
  state = medicare_spend[, c(i, "year")]
  state$State = i
  names(state) = c("medicare_expenditure", "year", "state")
  spend_medicare = rbind(spend_medicare, state)
}

figure(title = "Medicare Spending per recipent from 1992-2009", 
       ylab = "Medicare Expenditure") %>%
  ly_lines(year,medicare_expenditure, group = state,data = spend_medicare)

medicare_spend_percentage = as.data.frame(t(medicare_spend))[-53,]
ms = medicare_spend_percentage
ms = as.data.frame(sapply(ms, function(x) as.numeric(as.character(x))))
row.names(ms) = row.names(medicare_spend_percentage)
for(i in ncol(medicare_spend_percentage):2){
  medicare_spend_percentage[,i] = (ms[,i]-ms[,(i-1)])/ms[,(i-1)]
}
medicare_spend_percentage = as.data.frame(t(medicare_spend_percentage))[-1,]

medicare_percentage = as.data.frame(as.numeric(as.character(medicare_spend_percentage[, c("Alabama")])))
names(medicare_percentage) = "medicare_expenditure"
medicare_percentage$year = row.names(medicare_spend_percentage)
medicare_percentage$State = "Alabama"

for(i in names(medicare_spend_percentage)[-1]){
  state = as.data.frame(as.numeric(as.character(medicare_spend_percentage[, c(i)])))
  names(state) = "medicare_expenditure"
  state$year = row.names(medicare_spend_percentage)
  state$State = i
  medicare_percentage = rbind(medicare_percentage, state)
}
us_medicare_percentage = medicare_percentage[medicare_percentage$State == "United States",]
ggplot(medicare_percentage, aes(year, medicare_expenditure, group=State)) + geom_line() + geom_line(data = us_medicare_percentage, aes(year, medicare_expenditure, color ="red"))
