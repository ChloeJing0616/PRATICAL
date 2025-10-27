library(tidyverse)
library(sf)
library(here)
library(dplyr)
library(readr)
library(tmap)
install.packages("countrycode")
library(countrycode)
#read shp
World <- st_read('/Users/GIS/wk4/World_Countries_(Generalized)/World_Countries_Generalized.shp')

#transform CRS
World <- st_transform(worldshp, 4326)
st_crs(World)
names(World)

#extract
world_clean <- worldshp84 %>%
  select(iso = ISO, country = COUNTRY, geometry)
head(world_clean)

#read csv
HDI <- read_csv(here::here("HDR25_Composite_indices_complete_time_series.csv"),
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

Join_HDI_2_GB<-Join_HDI_2 %>%
  filter(aff_iso=="GB")