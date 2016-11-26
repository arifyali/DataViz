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

