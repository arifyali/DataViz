setwd("~/Documents/DataViz/")
source("graphs_9_10.R")
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
spend_medicare = medicare_spend[, c("Alabama", "year")]
spend_medicare$State = "Alabama"
names(spend_medicare) = c("medicare_expenditure", "year", "state")
for(i in names(medicare_spend)[-c(1, 53)]){
  state = medicare_spend[, c(i, "year")]
  state$State = i
  names(state) = c("medicare_expenditure", "year", "state")
  spend_medicare = rbind(spend_medicare, state)
}

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

years = as.character(unique(medicare_percentage$year))

fifth_percentitle = rep(0, length(years))
ninety_fifth_percentile = rep(0, length(years))
max_percent =rep(0, length(years))
min_percent =rep(0, length(years))
years_df = data.frame(years, min_percent, fifth_percentitle, ninety_fifth_percentile, max_percent)
for(i in 1:length(years)){
  percentage_year = medicare_percentage[which(medicare_percentage$year == years[i]),"medicare_expenditure"]
  years_df[i, 2:5] = quantile(percentage_year, probs = c(0,.05,.95,1))
}


us_medicare_percentage = medicare_percentage[medicare_percentage$State == "United States",]
time_series_medicare = #ggplot(medicare_percentage, aes(year, medicare_expenditure, group=State)) + 
  ggplot(years_df,aes(years, fifth_percentitle, group = 1))+
  geom_line() + 
  geom_line(aes(years, ninety_fifth_percentile))+
  geom_ribbon(aes(ymin=fifth_percentitle,ymax=ninety_fifth_percentile))+
  geom_line(aes(years, min_percent))+
  geom_line(aes(years, max_percent))+
  geom_line(data = us_medicare_percentage, aes(year, medicare_expenditure, color ="#8B0000"), size = 1.25)+
  coord_cartesian(ylim = c(-0.1, 0.2))
library(plotly)
ggplotly(time_series_medicare)
### Plot 13
### Heatmap
names(Cancer) = c("state", "total_cancer_deaths")
Cancer_deaths_by_type_by_state$Question = gsub(", mortality", "",Cancer_deaths_by_type_by_state$Question)
Cancer_deaths_by_type_by_state$Question = gsub("cancer of the ", "",Cancer_deaths_by_type_by_state$Question)

for(i in unique(Cancer_deaths_by_type_by_state$Question)){
  cancer = Cancer_deaths_by_type_by_state[which(Cancer_deaths_by_type_by_state$Question == i), c("LocationDesc", "DataValue")]
  cancer$LocationDesc = tolower(cancer$LocationDesc)
  names(cancer) = c("state", i)
  Cancer = merge(Cancer, cancer, by = "state")
  }
states = Cancer$state
row.names(Cancer) = states
Cancer = Cancer[, -(which(names(Cancer) == "state"))]
Cancer <- Cancer[order(Cancer$total_cancer_deaths, decreasing=TRUE),]
# Ordering
Cancer = Cancer[, -1]



for(i in 1:ncol(Cancer)){
  Cancer[,i] = as.numeric(as.character(Cancer[,i]))
}

library(Hmisc)

cancer_matrix = data.matrix(Cancer)#[,-c(1,3,5,8)]
row.names(cancer_matrix)<- capitalize(row.names(cancer_matrix))
colfunc <- colorRampPalette(c("#ffffff", "#8B0000"))

bball_heatmap <- heatmap(cancer_matrix, Rowv=NA, Colv=NA, col = (colfunc(256)), scale="column", cexRow = 1.25)
