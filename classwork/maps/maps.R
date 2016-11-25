library(rgdal)
library(maptools)
library(broom)
library(dplyr)
library(ggplot2)
library(ggmap)
# I get many shapefiles, including the below, from here: http://www.naturalearthdata.com
# I also get shapefiles from the US Census Bureau and state agencies, when making local maps

# Read the shapefiles into R
states <- readOGR(dsn = "ne_50m_admin_1_states_provinces_lakes",
                  layer = "ne_50m_admin_1_states_provinces_lakes")

# Let's transform them into tidy data that ggplot can use
states.points <- tidy(states, region = "adm1_code")

# And join in the original variables from the shapefile
states.df <- left_join(states.points, states@data, by = c("id" = "adm1_code"))

# Using those variables, we'll filter so we just have US states
states.df <- filter(states.df, iso_a2 == "US")

# Election data
election <- read.csv("~/Documents/DataViz/classwork/maps/2012.csv", stringsAsFactors = FALSE)

# Add in Obama's margin in each state
election$margin <- (election$obama - election$romney) / election$total

# And join it with the geospatial data
states.df <- left_join(states.df, election, by = c("postal" = "state"))

# Let's make a map
ggplot(data = states.df, aes(x = long, y = lat, group = group, fill = margin)) +
  scale_fill_gradient2(limits = c(-0.5, 0.5)) +
  geom_polygon(color = "white")

# That projection's terrible!
ggplot(data = states.df, aes(x = long, y = lat, group = group, fill = margin)) +
  scale_fill_gradient2(limits = c(-0.5, 0.5)) +
  geom_polygon(color = "white") +
  coord_map("mercator")

# Getting fancy
ggplot(data = states.df, aes(x = long, y = lat, group = group, fill = margin)) +
  scale_fill_gradient2(limits = c(-0.5, 0.5)) +
  geom_polygon(color = "white") +
  coord_map("albers", lat0 = 29.5, lat1 = 45.5,
            xlim = c(-124.85, -66.88), ylim = c(24.4, 49.38),
            orientation = c(90, 0, -98.35))
