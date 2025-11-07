library(tidyverse)
library(sf)
library(here)
library(dplyr)
library(readr)
library(tmap)
library(countrycode)
library(janitor)

#read shp
World <- st_read('world_country_shapes.geojson')

#transform CRS
st_crs(World)
names(World)

#extract
world_clean <- worldshp84 %>%
  select(iso = ISO, country = COUNTRY, geometry)
head(world_clean)

#read csv
HDI <- read_csv('HDR_composite_indices.csv',
                locale = locale(encoding = "latin1"),
                na = " ", skip=0)

HDIcols<- HDI %>%
  clean_names()%>%
  select(iso3, country, gii_2019, gii_2010)%>%
  mutate(difference=gii_2019-gii_2010)%>%
  mutate(iso_code=countrycode(country, origin = 'country.name', destination = 'iso2c'))%>%
  mutate(iso_code2=countrycode(iso3, origin ='iso3c', destination = 'iso2c'))

Join_HDI <- World %>% 
  clean_names() %>%
  left_join(., 
            HDIcols,
            # change to "aff_iso" = "iso_code"
            by = c("iso" = "iso_code"))

Join_HDI_GB<-Join_HDI %>%
  filter(aff_iso=="GB")

# Adding a plot
library(tmap)

tmap_mode("view")
tm_shape(Join_HDI) +
  tm_basemap(server = "OpenStreetMap") +
  tm_compass(type = "arrow", position = c("left", "bottom")) +
  tm_scalebar(position = c("left", "bottom")) +
  tm_title("Difference in Gender Inequality 2010-2019") +
  tm_polygons(fill="difference",
              fill_alpha=0.8,
              fill.legend = tm_legend(title = "Difference in Inequality", 
                                      size = 0.8))
