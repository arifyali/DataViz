setwd("~/Documents/DataViz/")
U.S._Chronic_Disease_Indicators__CDI_ <- read.csv("U.S._Chronic_Disease_Indicators__CDI_.csv")
Readmissions_and_Deaths_._Hospital <- read.csv("Readmissions_and_Deaths_-_Hospital.csv")
Medicare_Hospital_Spending_by_Claim <- read.csv("Medicare_Hospital_Spending_by_Claim.csv")

library("sqldf")
library("ggplot2")
library("maps")
library("rbokeh")
library(rgdal)
library(maptools)
library(broom)
library(dplyr)
library(ggplot2)
library(ggmap)
### First Plot
Readmissions = Readmissions_and_Deaths_._Hospital[grepl("READM", Readmissions_and_Deaths_._Hospital$Measure.ID), ]
names(Readmissions) = gsub("\\.","_", names(Readmissions))
Readmissions$normalized_scores = as.numeric(as.character(Readmissions$Score))*as.numeric(as.character(Readmissions$Denominator))
Readmissions = Readmissions[!is.na(Readmissions$normalized_scores),]

Readmissions_by_state = sqldf("SELECT AVG(Score) AS average_score, State, Measure_Name AS readmission_type FROM Readmissions
                              GROUP BY State, Measure_Name;")
Overall_readmission = sqldf("SELECT AVG(Score) AS average_score, State FROM Readmissions
                              GROUP BY State;")
Overall_readmission$readmission_type = "Overall Average"
Readmissions_by_state = rbind(Readmissions_by_state, Overall_readmission)

Readmissions_by_state = Readmissions_by_state[order(Readmissions_by_state$average_score), ]

Readmissions_by_state = Readmissions_by_state[order(Readmissions_by_state$average_score), ]
Readmissions_by_state$readmission_type = gsub("Rate of readmission for ", "",Readmissions_by_state$readmission_type)
Readmissions_by_state$readmission_type = gsub("Rate of readmission after ", "",Readmissions_by_state$readmission_type)
Readmissions_by_state$readmission_type = gsub(" \\(.*", "",Readmissions_by_state$readmission_type)

Readmissions_by_state$readmission_type = gsub(" Readmission Rate", "",Readmissions_by_state$readmission_type)
Readmission_types = unique(Readmissions_by_state$readmission_type)[-c(4,5)]
par(mfrow =c(2,4),oma=c(0,0,2,0))
for(i in Readmission_types){
  Readmissions_type = merge(Readmissions_by_state[Readmissions_by_state$readmission_type == i,], Readmissions_by_state[Readmissions_by_state$readmission_type == "discharge from hospital",], by.x = "State", by.y = "State")
  Readmissions_type = Readmissions_type[order(Readmissions_type$average_score.y, decreasing = T)[1:10],]
  print(i)
  barplot(t(as.matrix(Readmissions_type[,c("average_score.y")])), names.arg=Readmissions_type$State,col = c("grey80"), border=NA, space=0.25, xlab="State", main = toupper(i),ylab = "Rate of Readmin. (%)", ylim = c(0, 25), horiz=F)
}
Readmissions_type = merge(Readmissions_by_state[Readmissions_by_state$readmission_type == "Overall Average",], Readmissions_by_state[Readmissions_by_state$readmission_type == "discharge from hospital",], by.x = "State", by.y = "State")
Readmissions_type = Readmissions_type[order(Readmissions_type$average_score.y, decreasing = T)[1:10],]

barplot(t(as.matrix(Readmissions_type[,c("average_score.x")])), names.arg=Readmissions_type$State,col = c("#FF0000FF"), border=NA, space=0.25, xlab="State",main ="Overall Average",ylab = "Rate of Readmin. (%)", ylim = c(0, 25), horiz=F)
title("Average Hospital Readmission Rate by Medical Reason for the Top Ten States by Overall Readmission", outer = T)


### Plot 2
par(mfrow =c(1,1))
Patient_survey__HCAHPS__._Hospital <- read.csv("Patient_survey__HCAHPS__-_Hospital.csv")
View(Patient_survey__HCAHPS__._Hospital)
Patient_survey__HCAHPS__._Hospital$Patient.Survey.Star.Rating = as.numeric(as.character(Patient_survey__HCAHPS__._Hospital$Patient.Survey.Star.Rating))
clean = Patient_survey__HCAHPS__._Hospital[Patient_survey__HCAHPS__._Hospital$HCAHPS.Answer.Description == "Cleanliness - star rating",]
names(clean) = gsub('\\.', '_', names(clean))
clean_avg_state = sqldf("SELECT AVG(Patient_Survey_Star_Rating) AS avg_clean_rating ,State FROM clean GROUP BY State")

Readmissions$normalized_scores = as.numeric(as.character(Readmissions$Score))*as.numeric(as.character(Readmissions$Denominator))
Readmissions = Readmissions[!is.na(Readmissions$normalized_scores),]

Readmissions_normalized_by_state = sqldf("SELECT SUM(normalized_scores)/SUM(Denominator) AS average_score, Sum(Denominator) AS total_readmission, State FROM Readmissions
                                         GROUP BY State;")
pop_max = max(Readmissions_normalized_by_state$total_readmission)
Readmissions_normalized_by_state$total_readmission = Readmissions_normalized_by_state$total_readmission/1e6
merged_states = sqldf("SELECT * FROM clean_avg_state
                      JOIN Readmissions_normalized_by_state
                      ON Readmissions_normalized_by_state.State = clean_avg_state.State")
merged_states = na.omit(merged_states)
merged_states = merged_states[order(merged_states$avg_clean_rating, decreasing = T)[1:20],]

symbols(x = merged_states$avg_clean_rating, 
        y = merged_states$average_score, 
        circles = sqrt(merged_states$total_readmission)*10,
        bg = "#FF000099", xlab = "A State’s Average Clean Rating by Hosiptal",
        ylab = "A State’s Average Readmission Rate",
        main = "States with larger Total Hospital Readmissions with higher Readmission Rates tend to be less Clean", xlim = c(2.8,4))

text(x = c(merged_states$avg_clean_rating), y = merged_states$average_score, labels = merged_states$State)


### Plot 3
par(mfrow =c(1,1))

Age.adjusted_death_rates_1900_2013 <- read.csv("NCHS_-_Age-adjusted_death_rates_and_life-expectancy_at_birth___All_Races__Both_Sexes___United_States__1900-2013.csv",  stringsAsFactors=FALSE)
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
kff_medicare_data <- read.delim("kff_medicare_data.csv")

medicare_rep_total = as.numeric(as.character(kff_medicare_data$Total))
for(i in 2:ncol(kff_medicare_data)){
  kff_medicare_data[,i] = as.numeric(as.character(kff_medicare_data[,i]))/as.numeric(as.character(kff_medicare_data[,ncol(kff_medicare_data)]))
}
kff_medicare_data$Total = medicare_rep_total

kff_medicare_data[is.na(kff_medicare_data)] <- 0
# Kaiser Family Foundation estimates based on the Census Bureau's March 2016 Current Population Survey (CPS: Annual Social and Economic Supplements
Medicare_Spending <- read.delim("Total Medicare Spending by State.csv")
# Centers for Medicare &amp; Medicaid Services (2011). _Health Expenditures by State of Residence._Retrieved (December 2011) at [http://www.cms.gov/NationalHealthExpendData/downloads/resident-state-estimates.zip.](http://www.cms.gov/NationalHealthExpendData/downloads/resident-state-estimates.zip)	

kff_medicare_data = sqldf("SELECT * FROM kff_medicare_data
                          JOIN Medicare_Spending
                          ON kff_medicare_data.Location = 
                          Medicare_Spending.Location")

kff_medicare_data = kff_medicare_data[-1,]
library(ggplot2)
ggplot(data = kff_medicare_data, aes(x = White, y = Black, size = Total.Medicare.Spending.by.Residence)) + geom_point() #+ geom_text(aes(label=Location),hjust=0, vjust=0)
kff_medicare_data_no_outliers = kff_medicare_data[-which(kff_medicare_data$Location %in% c("District of Columbia", "Hawaii")),]
ggplot(data = kff_medicare_data_no_outliers, aes(x = White, y = Black, size = Total.Medicare.Spending.by.Residence, color = "#FF000022")) + geom_point() + geom_text(aes(label=Location),hjust=0, vjust=0)


### Plot 5
Healthcare_Expenditures_by_State <- read.delim("Health Care Expenditures by State of Residence.csv")
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

######### Plot 7
Readmissions_and_Deaths_._Hospital$geo_code = as.character(Readmissions_and_Deaths_._Hospital$Location)
Readmissions_and_Deaths_._Hospital$geo_code = gsub(".*\\n","",Readmissions_and_Deaths_._Hospital$geo_code)
Readmissions_and_Deaths_._Hospital$geo_code = gsub("\\(|\\)| ","",Readmissions_and_Deaths_._Hospital$geo_code)
Readmissions_and_Deaths_._Hospital = Readmissions_and_Deaths_._Hospital[Readmissions_and_Deaths_._Hospital$geo_code != "", ]
latlong= as.data.frame(strsplit(Readmissions_and_Deaths_._Hospital$geo_code,split = ","))
row.names(latlong) = c("lat", "long")
latlong = t(latlong)
Readmissions_and_Deaths_._Hospital = cbind(Readmissions_and_Deaths_._Hospital, latlong)

Readmissions_and_Deaths_._Hospital$lat = as.numeric(as.character(Readmissions_and_Deaths_._Hospital$lat))
Readmissions_and_Deaths_._Hospital$long = as.numeric(as.character(Readmissions_and_Deaths_._Hospital$long))

READM_30_HOSP_WIDE = Readmissions_and_Deaths_._Hospital[Readmissions_and_Deaths_._Hospital$Measure.ID == "READM_30_HOSP_WIDE", ]

#state.abb = state.abb[state.abb != "AK"]
#state.abb = state.abb[state.abb != "HI"]

READM_30_HOSP_WIDE = READM_30_HOSP_WIDE[as.character(READM_30_HOSP_WIDE$State) %in% state.abb,]
READM_30_HOSP_WIDE$Score = as.numeric(as.character(READM_30_HOSP_WIDE$Score))
READM_30_HOSP_WIDE = READM_30_HOSP_WIDE[!is.na(READM_30_HOSP_WIDE$Score),]

READM_30_HOSP_WIDE$Score[READM_30_HOSP_WIDE$Score >= 17.5] = 17.5
library(maps)
#load us map data
#all_states <- map_data("state")
states <- readOGR(dsn = "ne_50m_admin_1_states_provinces_lakes",
                  layer = "ne_50m_admin_1_states_provinces_lakes")

# Let's transform them into tidy data that ggplot can use
states.points <- tidy(states, region = "adm1_code")

# And join in the original variables from the shapefile
states.df <- left_join(states.points, states@data, by = c("id" = "adm1_code"))

# Using those variables, we'll filter so we just have US states
states.df <- filter(states.df, iso_a2 == "US")



#load us map data
#plot all states with ggplot
# p <- ggplot()
# p <- p + geom_polygon( data=all_states, aes(x=long, y=lat, group = group),colour="white", fill="grey" )
# 
# p + geom_point(data=READM_30_HOSP_WIDE, aes(x=long, y=lat,colour = Score)) +
#   scale_colour_gradient(high = "#FF000020") +
#   ggtitle("Location of All Hospitals that have Medicare/Medicaid Readmission Scores (in the Continous United States") +
#   theme_bw() +
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())
usa <- ggplot() +
  geom_polygon(data = states.df, aes(x = long, y = lat, group = group), colour="white", fill="grey") +
  scale_fill_gradient2(limits = c(-0.5, 0.5)) +
  geom_point(data=READM_30_HOSP_WIDE, aes(x=long, y=lat,colour = Score)) + scale_colour_gradient(high = "#FF000020") + 
  coord_map("albers", lat0 = 29.5, lat1 = 45.5,
             xlim = c(-124.85, -66.88), ylim = c(24.4, 49.38),
             orientation = c(90, 0, -98.35)) +
             theme_bw() +
             theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())
usa

alaska = states.df[states.df$name == "Alaska", ]
ak <- ggplot() +
  geom_polygon(data = states.df, aes(x = long, y = lat, group = group), colour="white", fill="grey") +
  scale_fill_gradient2(limits = c(-0.5, 0.5)) +
  geom_point(data=READM_30_HOSP_WIDE, aes(x=long, y=lat,colour = Score)) + scale_colour_gradient(high = "#FF000020") + 
  coord_map("albers", lat0 = 29.5, lat1 = 45.5,
            xlim = c(min(alaska$long)-10, max(alaska$long)+10), 
            ylim = c(min(alaska$lat)-1, max(alaska$lat)+1),
            orientation = c(90, 0, -98.35)) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())
ak

alaska = states.df[states.df$name == "Alaska", ]
ak <- ggplot() +
  geom_polygon(data = states.df, aes(x = long, y = lat, group = group), colour="white", fill="grey") +
  scale_fill_gradient2(limits = c(-0.5, 0.5)) +
  geom_point(data=READM_30_HOSP_WIDE, aes(x=long, y=lat,colour = Score)) + scale_colour_gradient(high = "#FF000020") + 
  coord_map("albers", lat0 = 29.5, lat1 = 45.5,
            xlim = c(min(alaska$long), max(alaska$long)), 
            ylim = c(min(alaska$lat), max(alaska$lat)),
            orientation = c(90, 0, -98.35)) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())
ak


hawaii = states.df[states.df$name == "Hawaii", ]
hi <- ggplot() +
  geom_polygon(data = states.df, aes(x = long, y = lat, group = group), colour="white", fill="grey") +
  scale_fill_gradient2(limits = c(-0.5, 0.5)) +
  geom_point(data=READM_30_HOSP_WIDE, aes(x=long, y=lat,colour = Score)) + scale_colour_gradient(high = "#FF000020") + 
  coord_map("albers", lat0 = 29.5, lat1 = 45.5,
            xlim = c(-160, -153), 
            ylim = c(16, 24),
            orientation = c(90, 0, -98.35)) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())
hi



# p + geom_point(data=READM_30_HOSP_WIDE, aes(x=long, y=lat,colour = Score)) + 
#   scale_colour_gradient(high = "#FF000020") + 
#   theme_bw() +
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())
# ### Plot 8
# Correlation Plot of Readmission vs Mortality Scores (use hosiptal adjusted average) by hospital
READM_30_HOSP_WIDE = Readmissions_and_Deaths_._Hospital[Readmissions_and_Deaths_._Hospital$Measure.ID == "READM_30_HOSP_WIDE", c("Provider.ID", "Score")]
names(READM_30_HOSP_WIDE) = c("Provider_ID", "Readmission_Score")
READM_30_HOSP_WIDE$Readmission_Score = as.numeric(as.character(READM_30_HOSP_WIDE$Readmission_Score))
READM_30_HOSP_WIDE = READM_30_HOSP_WIDE[!is.na(READM_30_HOSP_WIDE$Readmission_Score),]

MORT = Readmissions_and_Deaths_._Hospital[grepl("MORT", Readmissions_and_Deaths_._Hospital$Measure.ID), ]
MORT$Score = as.numeric(as.character(MORT$Score))
MORT = MORT[!is.na(MORT$Score),]
names(MORT) = gsub("\\.", "_", names(MORT))
MORT_count_hospital = sqldf("SELECT SUM(Denominator) AS Sum, Provider_ID 
                            FROM MORT
                            GROUP BY Provider_ID")

MORT = sqldf("SELECT SUM(Score*Denominator)/Sum AS Mortality_Score, MORT.State AS State, MORT.Provider_ID AS Provider_ID
              FROM MORT
              JOIN MORT_count_hospital
              ON MORT.Provider_ID = MORT_count_hospital.Provider_ID
              GROUP BY MORT.Provider_ID")

MORT_vs_Readm = sqldf("SELECT * FROM
                      MORT
                      JOIN READM_30_HOSP_WIDE
                      ON MORT.Provider_ID = READM_30_HOSP_WIDE.Provider_ID")

cor(MORT_vs_Readm$Mortality_Score, MORT_vs_Readm$Readmission_Score)
UT_MORT_vs_Readm = MORT_vs_Readm[MORT_vs_Readm$State == "UT",]
cor(UT_MORT_vs_Readm$Mortality_Score, UT_MORT_vs_Readm$Readmission_Score)

non_UT_MORT_vs_Readm = MORT_vs_Readm[MORT_vs_Readm$State != "UT",]

ggplot(data = non_UT_MORT_vs_Readm, aes(x = Mortality_Score, y = Readmission_Score)) + 
  geom_point(col = "grey") +
  geom_point(data = UT_MORT_vs_Readm, aes(x = Mortality_Score, y = Readmission_Score), col = "#FF000099") +
  ggtitle("Hospital Readmission Percentage vs. Mortality Percentage (Correlation of -0.097)") +
  xlab("Mortality Percentage") + ylab("Readmission Percentage")
  +coord_cartesian(
    xlim = c(0, 25), ylim = c(10, 25))
### Plot 9
U.S._Chronic_Disease_Indicators__CDI_$Topic = tolower(U.S._Chronic_Disease_Indicators__CDI_$Topic)

U.S._Chronic_Disease_Indicators__CDI_$Question = tolower(U.S._Chronic_Disease_Indicators__CDI_$Question)

Cancer = U.S._Chronic_Disease_Indicators__CDI_[grepl("cancer",U.S._Chronic_Disease_Indicators__CDI_$Topic) & 
                                                 grepl("mortal",U.S._Chronic_Disease_Indicators__CDI_$Question)
                                                 ,]

Cancer = Cancer[Cancer$DataValueTypeID == "AvgAnnAgeAdjRate", ]

Cancer = sqldf("SELECT LocationDesc AS region, SUM(DataValue) AS DataValue FROM Cancer GROUP BY GeoLocation")
US_cancer_count = Cancer[1,2]
Cancer = Cancer[-1,]

all_states <- map_data("state")
Cancer$region = tolower(Cancer$region)
Total <- merge(all_states, Cancer, by="region")

p <- ggplot()
p <- p + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=Total$DataValue),colour="white"
) + scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar")
P1 <- p + theme_bw()  + labs(fill = "Cancer-related Deaths \n per 100,000 people" 
                             ,title = "Cancer Motality in the United States (2008-2012)", x="", y="")
P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())

### Plot 10


hist(READM_30_HOSP_WIDE$Readmission_Score, breaks = 100, freq = FALSE, col = "grey", main = "Histogram of U.S. Hospital Readmission Scores (closely follows a Normal Distribution with a left tail)", xlab = "Readmission Score")
lines(density(READM_30_HOSP_WIDE$Readmission_Score))
abline(v = median(READM_30_HOSP_WIDE$Readmission_Score), col ="#FF000099", lty="dotted", lwd = 4)

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

