U.S._Chronic_Disease_Indicators__CDI_ <- read.csv("~/Downloads/U.S._Chronic_Disease_Indicators__CDI_.csv")
Readmissions_and_Deaths_._Hospital <- read.csv("~/Downloads/Readmissions_and_Deaths_-_Hospital.csv")
Medicare_Hospital_Spending_by_Claim <- read.csv("~/Downloads/Medicare_Hospital_Spending_by_Claim.csv")

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
Patient_survey__HCAHPS__._Hospital <- read.csv("~/Downloads/Patient_survey__HCAHPS__-_Hospital.csv")
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
