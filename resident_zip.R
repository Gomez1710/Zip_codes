library(tidyverse)
library(sp)
library(sf)
library(mclust)
library(cartogram)
library(tmap)
library(tigris)
library(showtext)
library(zipcodeR)

# read in the county data from the tigris library
county_map <- counties(state = "CA", cb = TRUE)
# rename column six to County
colnames(county_map)[6] <- "County"


# read in the zip code data for california
zip <- zctas(state = "California", year = 2010)
colnames(zip)[2] <- "Zip"


# read in the montserrat font to be used for the graphs
font_add_google("Montserrat", "Montserrat")

# read in resident data
dat <- read.csv("resident_data.csv", stringsAsFactors = FALSE)

# fitler for ca

ca <- dat %>% filter(State == "California")

# group by zip codes
ca_counties <- ca %>% 
  group_by(Zip) %>% 
  summarize(n = n())

# merge both resident data with the zip code data by zip codes

merged_data <- left_join(zip, ca_counties, by = "Zip")

merged_data$n[is.na(merged_data$n)] <- 0

# start process to create a dorling cartogram
dat.sp <- as_Spatial(merged_data)


dat.sp <- spTransform( dat.sp, CRS("+init=epsg:3395"))

# create dorling cartogram
zip_dorling <- cartogram_dorling( x=dat.sp, weight="n", k=0.05 )

plot( zip_dorling )


tmap_mode("plot")

box <- st_bbox(county_map %>% filter(County %in% c("Los Angeles", "San Bernardino", "Riverside", "Orange")))

tm_shape(county_map, bbox = box) +
  tm_borders() +
  tm_fill(col = "white") +
tm_shape( zip_dorling, crs = 4326) +
  tm_polygons( size="n", col="n", n=7, style="cont", palette= "RdYlBu", title = "Number of Residents \nPracticing") +
  tm_layout(fontfamily = "Montserrat")

