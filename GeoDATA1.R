library(sf)
library(tigris)
library(dplyr)
library(ggplot2)
library(rio)
library(tidygeocoder)
library(tmap)
texascounties <- counties(state = "TX", cb = TRUE)
ggplot(texascounties)+ geom_sf()

fqhc_url <-"https://data.hrsa.gov/DataDownload/DD_Files/Health_Center_Service_Delivery_and_LookAlike_Sites.xlsx"
fqhc<-import(fqhc_url)
fqhc_sf<-subset(fqhc,`Site State Abbreviation`=="TX")%>%
st_as_sf(coords = c("Geocoding Artifact Address Primary X Coordinate","Geocoding Artifact Address Primary Y Coordinate"), crs = 4326)
ggplot(texascounties)+ geom_sf()+geom_sf(data = fqhc_sf)

FQHC_counties <- (st_transform (fqhc_sf, crs = st_crs(texascounties)))

FQHC_C2 <- st_join(FQHC_counties, texascounties) %>% 
  group_by(NAME) %>% 
  summarise(clinic_count=n()) %>% 
  st_drop_geometry() %>% 
  left_join(texascounties,.,by="NAME") %>% 
  mutate(clinic_count = coalesce(log(clinic_count), 0))

ggplot(data = FQHC_C2) + 
  geom_sf(aes(fill=clinic_count))+
  geom_sf(data = fqhc_sf, color="pink", size=1.5, alpha=.3)+
  scale_fill_viridis_c()

street_address<-data.frame(full_address=c("7703 Floyd Curl Dr, San Antonio, TX 78258","1 Haven for Hope Way, San Antonio, TX 78207", "300 Alamo Plaza, San Antonio, TX 78205", "1402 Broadway St, Galveston, TX 77550", "411 Elm St, Dallas, TX 75202", "604 Brazos St, Austin, TX 78701", "2515 W 5th St, Irving, TX 75060", "100 Lady Bird Lane, Johnson City, TX 78636", "1412 W Ohio Ave, Midland, TX 79701", "1217 W Sam Rayburn Dr, Bonham, TX 75418", "3700 Hogge Dr, Parker, TX 75002", "2101 Ross Ave, Dallas, TX 75201", "2414 Rosedale St, Houston, TX 77004"))
geocoded_addresses <-geocode(street_address,address=full_address, method="osm") #obtained coordinate for addresses
shapefile_addresses <-st_as_sf(geocoded_addresses, coords = c("long", "lat"), crs=4326)



ggplot(data=FQHC_C2)+
  geom_sf(fill="purple")+
  geom_sf(data=fqhc_sf,color="orange",size=1.5, alpha=.3)+
  geom_sf(data=shapefile_addresses,color="green")
  scale_fill_viridis_c()
  
 
  #interactive map
  tmap_mode ("view")
  tm_shape(FQHC_C2)+
  tm_polygons(fill="clinic_count",fill_alpha = 0.3)+
  tm_shape(fqhc_sf)+
  tm_symbols(fill="darkgreen",fill_alpha=0.5, size=0.2)+
  tm_shape(shapefile_addresses)+
  tm_dots(fill="orange")
  
  
  # this goes at the end of geodata.R
  
  
  save(
    FQHC_C2,
    shapefile_addresses,
    file = 'GeoDATA1.RData'
  )
  
  
  