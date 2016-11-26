setwd("~/Documents/DataViz/")
source("graphs_7_8.R")

### Plot 9
U.S._Chronic_Disease_Indicators__CDI_$Topic = tolower(U.S._Chronic_Disease_Indicators__CDI_$Topic)

U.S._Chronic_Disease_Indicators__CDI_$Question = tolower(U.S._Chronic_Disease_Indicators__CDI_$Question)

Cancer = U.S._Chronic_Disease_Indicators__CDI_[grepl("cancer",U.S._Chronic_Disease_Indicators__CDI_$Topic) & 
                                                 grepl("mortal",U.S._Chronic_Disease_Indicators__CDI_$Question)
                                               ,]

Cancer = Cancer[Cancer$DataValueTypeID == "AvgAnnAgeAdjRate", ]
Cancer_deaths_by_type_by_state = Cancer


Cancer = sqldf("SELECT LocationDesc AS region, SUM(DataValue) AS DataValue FROM Cancer GROUP BY GeoLocation")
US_cancer_count = Cancer[1,2]
Cancer = Cancer[-1,]

states <- readOGR(dsn = "ne_50m_admin_1_states_provinces_lakes",
                  layer = "ne_50m_admin_1_states_provinces_lakes")

gpclibPermit()
# Let's transform them into tidy data that ggplot can use
states.points <- tidy(states, region = "adm1_code")

# And join in the original variables from the shapefile
states.df <- left_join(states.points, states@data, by = c("id" = "adm1_code"))

# Using those variables, we'll filter so we just have US states
states.df <- filter(states.df, iso_a2 == "US")
Cancer$region = tolower(Cancer$region)
states.df$region = tolower(states.df$name)
Total <- merge(states.df, Cancer, by="region")

usa <- ggplot() + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=Total$DataValue),colour="white") + 
     scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar") + 
    theme_bw()  + labs(fill = "Cancer-related Deaths \n per 100,000 people" 
                             ,title = "Cancer Motality in the United States (2008-2012)", x="", y="") + 
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank()) +
  coord_map("albers", lat0 = 29.5, lat1 = 45.5,
            xlim = c(-124.85, -66.88), ylim = c(24.4, 49.38),
            orientation = c(90, 0, -98.35))
usa
alaska = states.df[states.df$region == "alaska", ]
ak <- ggplot() +
  geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=Total$DataValue),colour="white") + 
  scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar") + 
  theme_bw() +
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank()) +
  coord_map("albers", lat0 = 29.5, lat1 = 45.5,
            xlim = c(min(alaska$long), max(alaska$long)), 
            ylim = c(min(alaska$lat), max(alaska$lat)),
            orientation = c(90, 0, -98.35)) 
ak


hawaii = states.df[states.df$region == "hawaii", ]
hi <- ggplot() +
  geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=Total$DataValue),colour="white") + 
  scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar") + 
  theme_bw() +
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank()) +
  coord_map("albers", lat0 = 29.5, lat1 = 45.5,
            xlim = c(-160, -153), 
            ylim = c(16, 24),
            orientation = c(90, 0, -98.35))
hi


### Plot 10
hist(READM_30_HOSP_WIDE$Readmission_Score, breaks = 100, freq = FALSE, col = "grey", main = "Histogram of U.S. Hospital Readmission Scores (closely follows a Normal Distribution with a left tail)", xlab = "Readmission Score")
lines(density(READM_30_HOSP_WIDE$Readmission_Score))
abline(v = median(READM_30_HOSP_WIDE$Readmission_Score), col ="#FF000099", lty="dotted", lwd = 4)
