setwd("~/Documents/DataViz/")
source("data_sources.R")

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
