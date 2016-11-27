#### Revised Plots
setwd("~/Documents/DataViz/")
source("graphs_11_12_13.R")
### Pareto Chart
US_census = read.csv("data/us_census_2015_population_extimate.csv")
US_census$Geographic.Area = tolower(gsub("\\.", "", US_census$Geographic.Area))
US_census$Census = as.numeric(gsub(",", "", US_census$Census))/1e5
US_census = US_census[, c("Geographic.Area", "Census")]
total_cancer_deaths = Cancer
total_cancer_deaths$Geographic.Area = row.names(total_cancer_deaths)
total_cancer_deaths = merge(total_cancer_deaths, US_census, by ="Geographic.Area")

for(state in total_cancer_deaths$Geographic.Area){
  pop = total_cancer_deaths[total_cancer_deaths$Geographic.Area == state, "Census"]

for(cancer in names(total_cancer_deaths)[-c(1,10)]){
  total_cancer_deaths[total_cancer_deaths$Geographic.Area == state, cancer] = total_cancer_deaths[total_cancer_deaths$Geographic.Area == state, cancer]*pop
  }
}

cancer_counts = colSums(total_cancer_deaths[-c(1,10)])
colfunc <- colorRampPalette(c("#ffe5e5", "#8B0000"))
pareto.chart(cancer_counts, col = rev(colfunc(8)))

#### Correlation 
MORT_vs_Readm = MORT_vs_Readm[, c(1,2,5)]
MORT_vs_Readm = na.omit(MORT_vs_Readm)
MORT_vs_Readm$State = as.character(MORT_vs_Readm$State)
states = (unique(MORT_vs_Readm$State))
states_correlation = c()
for(i in states){
  MORT_vs_Readm_state = MORT_vs_Readm[MORT_vs_Readm$State ==i, "Mortality_Score"]
  states_correlation = c(states_correlation, paste("[", paste0(quantile(MORT_vs_Readm_state,probs = c(0,.25,.5,.75, 1)), collapse = ", "), "],", sep = ""))
  
}
names(states_correlation) <- states

row.names(MORT_vs_Readm) = MORT_vs_Readm$State

fileConn <- file("quantiles.txt")
writeLines(paste(states_correlation, sep=""), fileConn)
close(fileConn)


paste0(states, collapse = "','")
View(MORT_vs_Readm[MORT_vs_Readm$Mortality_Score == min(MORT_vs_Readm$Mortality_Score),])

states_correlation= c()

for(i in states){
  MORT_vs_Readm_state = MORT_vs_Readm[MORT_vs_Readm$State ==i, c("Mortality_Score", "Readmission_Score")]
  states_correlation = states_correlation =c(states_correlation, cor(MORT_vs_Readm_state$Mortality_Score, MORT_vs_Readm_state$Readmission_Score))
  
}

stats = rep(1, times = length(states_correlation))
states_correlation_df = data.frame(stats,states_correlation )
row.names(states_correlation_df) <- states
states_correlation_df = states_correlation_df[order(states_correlation_df$states_correlation),]
states_correlation_matrix = data.matrix(states_correlation_df)
bball_heatmap <- heatmap(states_correlation_matrix, Rowv=NA, Colv=NA, col = (colfunc(256)), scale="column", cexRow = 1.25, las=1)
