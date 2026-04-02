library(sf)
library(tigris)
library(dplyr)
library(ggplot2)
library(rio)
texascounties <- counties(state = "TX", cb = TRUE)
ggplot(texascounties)+ geom_sf()

fqhc_url <-"https://data.hrsa.gov/DataDownload/DD_Files/Health_Center_Service_Delivery_and_LookAlike_Sites.xlsx"
fqhc<-import(fqhc_url)
fqhc_sf<-subset(fqhc,`Site State Abbreviation`=="TX")%>%
st_as_sf(coords = c("Geocoding Artifact Address Primary X Coordinate","Geocoding Artifact Address Primary Y Coordinate"), crs = 4326)
ggplot(texascounties)+ geom_sf()+geom_sf(data = fqhc_sf)


fqhc_county<-st_transform(fqhc_sf,crs=st_crs(texascounties))
