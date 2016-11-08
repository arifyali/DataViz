setwd("~/Documents/Georgetown/Data-Visualization-PPOL-646-Fall-2016/")
U.S._Chronic_Disease_Indicators__CDI_ <- read.csv("U.S._Chronic_Disease_Indicators__CDI_.csv")
Readmissions_and_Deaths_._Hospital <- read.csv("Readmissions_and_Deaths_-_Hospital.csv")
Medicare_Hospital_Spending_by_Claim <- read.csv("Medicare_Hospital_Spending_by_Claim.csv")

### First Plot
library("sqldf")
Readmissions = Readmissions_and_Deaths_._Hospital[grepl("READM", Readmissions_and_Deaths_._Hospital$Measure.ID), ]
names(Readmissions) = gsub("\\.","_", names(Readmissions))
Readmissions$normalized_scores = as.numeric(as.character(Readmissions$Score))*as.numeric(as.character(Readmissions$Denominator))
Readmissions = Readmissions[!is.na(Readmissions$normalized_scores),]

Readmissions_by_state = sqldf("SELECT AVG(Score) AS average_score, State, Measure_Name AS readmission_type FROM Readmissions
                              GROUP BY State, Measure_Name;")
Readmissions_by_state = Readmissions_by_state[order(Readmissions_by_state$average_score), ]

Readmissions_by_state = Readmissions_by_state[order(Readmissions_by_state$average_score), ]
Readmissions_by_state$readmission_type = gsub("Rate of readmission for ", "",Readmissions_by_state$readmission_type)
Readmissions_by_state$readmission_type = gsub("Rate of readmission after ", "",Readmissions_by_state$readmission_type)
Readmissions_by_state$readmission_type = gsub(" \\(.*", "",Readmissions_by_state$readmission_type)

Readmissions_by_state$readmission_type = gsub(" Readmission Rate", "",Readmissions_by_state$readmission_type)
Readmission_types = unique(Readmissions_by_state$readmission_type)[-6]
par(mfrow =c(3,3),oma=c(0,0,2,0))
for(i in Readmission_types){
  Readmissions_type = merge(Readmissions_by_state[Readmissions_by_state$readmission_type == i,], Readmissions_by_state[Readmissions_by_state$readmission_type == "discharge from hospital",], by.x = "State", by.y = "State")
  Readmissions_type = Readmissions_type[order(Readmissions_type$average_score.y, decreasing = T)[1:10],]
  print(Readmissions_type$average_score.y)
  barplot(t(as.matrix(Readmissions_type[,c("average_score.y")])), names.arg=Readmissions_type$State,col = c("grey80"), border=NA, space=0.25, xlab="State", main = toupper(i),ylab = "Rate of Readmin. (%)", ylim = c(0, 25))
  barplot(t(as.matrix(Readmissions_type[,c("average_score.x")])), names.arg=Readmissions_type$State,col = c("#FF0000FF"), border=NA, space=0.25, xlab="State", ylim = c(0, 25), add = T)
}
plot(1, type="n", axes=F, xlab="", ylab="")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "grey80")
legend("right", legend = c("Rate of Overall readmission"), bty = "n", cex=0.75)
title("The rates of Readmission by Reason for the Top 10 Highest Readmitting States (averaged by hospital)", outer = T)


###
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

symbols(x = c(merged_states$avg_clean_rating,rep(5, 5)), 
        y = c(merged_states$average_score,rep(3.6,5)), 
        circles = sqrt(c(merged_states$total_readmission, c(1e5,2.5e5,5e5,7.5e5, 1e6)/1e6)),
        bg = "#FF000022", ylim = c(0,20))

text(x = c(merged_states$avg_clean_rating), y = merged_states$average_score, labels = merged_states$State)

symbols(x = rep(5, 5), y = rep(3.6,5), circle = sqrt(10^(0:4)), add = T)

### Plot 3
Age.adjusted_death_rates_1900_2013 <- read.csv("NCHS_-_Age-adjusted_death_rates_and_life-expectancy_at_birth___All_Races__Both_Sexes___United_States__1900-2013.csv",  stringsAsFactors=FALSE)
Age.adjusted_death_rates_1900_2013 = Age.adjusted_death_rates_1900_2013[!is.na(Age.adjusted_death_rates_1900_2013$Average.Life.Expectancy), ]
par(mfrow = c(3,1))

for(i in unique(Age.adjusted_death_rates_1900_2013$Race)){
    plot(Average.Life.Expectancy~Year, data = Age.adjusted_death_rates_1900_2013[Age.adjusted_death_rates_1900_2013$Sex== "Both Sexes" & Age.adjusted_death_rates_1900_2013$Race == i, ], type = "l",lwd= 2, col = "black")
    
    points(Average.Life.Expectancy~Year, data = Age.adjusted_death_rates_1900_2013[Age.adjusted_death_rates_1900_2013$Sex== "Female" & Age.adjusted_death_rates_1900_2013$Race == i, ], type = "l",lwd= 2, col = "red")
  
    points(Average.Life.Expectancy~Year, data = Age.adjusted_death_rates_1900_2013[Age.adjusted_death_rates_1900_2013$Sex== "Male" & Age.adjusted_death_rates_1900_2013$Race == i, ], type = "l",lwd= 2, col = "blue")
    
}

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
ggplot(data = kff_medicare_data, aes(x = White, y = Black, size = Total.Medicare.Spending.by.Residence, color = "#FF000022", label = Location)) + geom_point() + geom_text(aes(label=Location),hjust=0, vjust=0)

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

state.abb = state.abb[state.abb != "AK"]
state.abb = state.abb[state.abb != "HI"]

READM_30_HOSP_WIDE = READM_30_HOSP_WIDE[as.character(READM_30_HOSP_WIDE$State) %in% state.abb,]
READM_30_HOSP_WIDE$Score = as.numeric(as.character(READM_30_HOSP_WIDE$Score))
READM_30_HOSP_WIDE = READM_30_HOSP_WIDE[!is.na(READM_30_HOSP_WIDE$Score),]

READM_30_HOSP_WIDE$Score[READM_30_HOSP_WIDE$Score >= 17.5] = 17.5
library(maps)
#load us map data
all_states <- map_data("state")
#plot all states with ggplot
p <- ggplot()
p <- p + geom_polygon( data=all_states, aes(x=long, y=lat, group = group),colour="white", fill="grey" )

p + geom_point(data=READM_30_HOSP_WIDE, aes(x=long, y=lat,colour = Score)) + scale_colour_gradient(low = "#FF000020") + ggtitle("Location of All Hospitals that have Medicare/Medicaid Readmission Scores (in the Continous United States") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())


