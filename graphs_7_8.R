setwd("~/Documents/DataViz/")
source("data_sources.R")

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