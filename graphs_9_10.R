setwd("~/Documents/DataViz/")
source("data_sources.R")

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
