setwd("~/Documents/DataViz/")
U.S._Chronic_Disease_Indicators__CDI_ <- read.csv("U.S._Chronic_Disease_Indicators__CDI_.csv")
Readmissions_and_Deaths_._Hospital <- read.csv("Readmissions_and_Deaths_-_Hospital.csv")
library("sqldf")
library("rbokeh")

#Plot 10
READM_30_HOSP_WIDE = Readmissions_and_Deaths_._Hospital[Readmissions_and_Deaths_._Hospital$Measure.ID == "READM_30_HOSP_WIDE", c("Provider.ID", "Score","Denominator", "Compared.to.National", "Hospital.Name","City","State")]
names(READM_30_HOSP_WIDE) = c("Provider_ID", "Readmission_Score","Denominator", "Compared.to.National","Hospital.Name","City","State")
READM_30_HOSP_WIDE$Readmission_Score = as.numeric(as.character(READM_30_HOSP_WIDE$Readmission_Score))
READM_30_HOSP_WIDE = READM_30_HOSP_WIDE[!is.na(READM_30_HOSP_WIDE$Readmission_Score),]

h <- figure(width = 600, height = 400) %>%
  ly_hist(Readmission_Score, data = READM_30_HOSP_WIDE, hover = Compared.to.National, breaks = 100, freq = FALSE, color = "grey") %>%
  # the fille color needs to be "#FF000099"
  ly_density(Readmission_Score, data = READM_30_HOSP_WIDE)

h

READM_30_HOSP_WIDE = Readmissions_and_Deaths_._Hospital[which(Readmissions_and_Deaths_._Hospital$Measure.ID == "READM_30_HOSP_WIDE"), c("Provider.ID", "Score","Denominator", "Compared.to.National", "Hospital.Name","City","State")]
names(READM_30_HOSP_WIDE) = c("Provider_ID", "Readmission_Score","Denominator", "Compared.to.National","Hospital.Name","City","State")
READM_30_HOSP_WIDE$Readmission_Score = as.numeric(as.character(READM_30_HOSP_WIDE$Readmission_Score))
READM_30_HOSP_WIDE = READM_30_HOSP_WIDE[!is.na(READM_30_HOSP_WIDE$Readmission_Score),]

#Plot 11
tobacco = U.S._Chronic_Disease_Indicators__CDI_[U.S._Chronic_Disease_Indicators__CDI_$Topic == "Tobacco", ]

youth_tobacco_smoke = tobacco[tobacco$Question == "Current cigarette smoking among youth",c("LocationDesc","DataValue")]
names(youth_tobacco_smoke) = c("State", "Percent_Youth_Smoke")

youth_tobacco_smokeless = tobacco[tobacco$Question == "Current smokeless tobacco use among youth",c("LocationDesc","DataValue")]
names(youth_tobacco_smokeless) = c("State", "Percent_Youth_tobacco_(smokeless)")

License_requirement = tobacco[tobacco$Question == "States with strong polices that require retail licenses to sell tobacco products",c("LocationDesc","DataValue")]
names(License_requirement) = c("State", "License_requirement")

youth_tobacco = sqldf("SELECT * FROM youth_tobacco_smoke
                      JOIN youth_tobacco_smokeless
                      ON youth_tobacco_smoke.State = youth_tobacco_smokeless.State
                      JOIN License_requirement
                      ON youth_tobacco_smokeless.State = License_requirement.State")
youth_tobacco$Percent_Youth_Smoke = as.numeric(as.character(youth_tobacco$Percent_Youth_Smoke))
youth_tobacco$`Percent_Youth_tobacco_(smokeless)` = as.numeric(as.character(youth_tobacco$`Percent_Youth_tobacco_(smokeless)`))
youth_tobacco$License_requirement = as.character(youth_tobacco$License_requirement)
youth_tobacco$License_requirement[youth_tobacco$License_requirement == "Yes"] = "The State requires a license"
youth_tobacco$License_requirement[youth_tobacco$License_requirement == "No"] = " The State don't requires a license"

j <- figure(width = 1200, height = 800,
            title = "Requiring Licenses to Sell Cigarettes. or other Tobacco Products does not affect Youth Tobacco Use",
            xlab = "Percent of Youth that are smoking", ylab = "Percent of Youth that are using smokeless tobacco products"
) %>%
  ly_points(Percent_Youth_Smoke, `Percent_Youth_tobacco_(smokeless)`, data = youth_tobacco,
            hover = "In @State, the percentage of youth that smoke is @Percent_Youth_Smoke%. @License_requirement to sell cigarettes.")
j

###plot 12

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
