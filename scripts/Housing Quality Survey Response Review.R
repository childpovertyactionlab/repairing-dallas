library(googlesheets4)
library(tidyverse)
library(sf)

libDB <- "C:/Users/Michael/CPAL Dropbox/"

repairPivot <- rio::import("Data/Repairing Dallas Survey Responses - Geocode.csv")

names(repairPivot)
##### Geocode Cleaned Addresses #####
#repairGeocode = repairPivot %>%
#  tidygeocoder::geocode(address = fulladdress,
#                        method = "arcgis",
#                        full_results = TRUE)

#rio::export(repairGeocode, "Data/Repairing Dallas Survey Responses - Geocode.csv")

##### Summaries of Responses #####
repairPivot %>%
  group_by(incomethreshold) %>%
  summarize(count = n())

repairPivot %>%
  group_by(incomethreshold, tot_u6, home_1979) %>%
  summarize(count = n()) %>%
  view()

repairPivot %>%
  group_by(hh_size) %>%
  summarize(count = n(),
            children_u18 = sum(tot_u18, na.rm = TRUE),
            children_u16 = sum(tot_u6, na.rm = TRUE)) %>%
  mutate(percent = count/sum(count)) %>%
  view()

repairPivot %>%
  mutate(flag_u18 = ifelse(tot_u18 > 0, TRUE, FALSE)) %>%
  group_by(flag_u18) %>%
  summarize(count = n())

repairPivot %>%
  mutate(flag_u18 = ifelse(tot_u18 > 0, TRUE, FALSE)) %>%
  group_by(incomethreshold, flag_u18) %>%
  summarize(count = n())

names(repairPivot)

repairPivot %>%
  group_by(safety_carbonmono) %>%
  summarize(count = n())

repairPivot %>%
  group_by(safety_extinguisher) %>%
  summarize(count = n())

repairPivot %>%
  group_by(safety_smoke) %>%
  summarize(count = n())

repairPivot %>%
  group_by(home_status) %>%
  summarize(count = n())

repairPivot %>%
  group_by(rent_insurance) %>%
  summarize(count = n())

repairPivot %>%
  group_by(home_insurance) %>%
  summarize(count = n())

repairPivot %>%
  group_by(tempwinter) %>%
  summarize(count = n()) %>%
  view()

repairPivot %>%
  group_by(tempsummer) %>%
  summarize(count = n()) %>%
  view()

repairPivot %>%
  group_by(water_llreport, water_llrepair) %>%
  summarize(count = n())

repairPivot %>%
  group_by(hazards_llreport, hazards_llrepair) %>%
  summarize(count = n())

repairPivot %>%
  group_by(pests_service, pests_llreport) %>%
  summarize(count = n())

repairPivot %>%
  group_by(repairs_llreport, repairs_llrepair) %>%
  summarize(count = n())

repairPivot %>%
  filter(home_status != "Rented by you or someone in this household. (Alquilado por usted o alguien en este hogar.)") %>%
  filter(incomethreshold == TRUE) %>%
  summarize(count = n())

repairPivot %>%
  filter(home_status != "Rented by you or someone in this household. (Alquilado por usted o alguien en este hogar.)") %>%
  filter(incomethreshold == TRUE) %>%
  filter(tot_u6 == TRUE | frequent_u6 == TRUE) %>%
  filter(home_1979 == "Yes (Sí)") %>%
  summarize(count = n())

repairPivot %>%
  filter(home_status != "Rented by you or someone in this household. (Alquilado por usted o alguien en este hogar.)") %>%
  filter(incomethreshold == TRUE) %>%
  filter(over65 == TRUE) %>%
  summarize(count = n())

##### Demographics #####
repairPivot %>%
  group_by(ethnicity) %>%
  summarize(count = n())

repairPivot %>%
  filter(incomethreshold == TRUE) %>%
  group_by(ethnicity) %>%
  summarize(count = n())

repairPivot %>%
  group_by(race) %>%
  summarize(count = n())

repairPivot %>%
  filter(incomethreshold == TRUE) %>%
  group_by(race) %>%
  summarize(count = n())

repairPivot %>%
  group_by(over65) %>%
  summarize(count = n()) %>%
  mutate(percent = count/sum(count))

repairPivot %>%
  group_by(frequent_u6) %>%
  summarize(count = n()) %>%
  mutate(percent = count/sum(count))

repairPivot %>%
  mutate(u18flag = ifelse(tot_u18 > 0, TRUE, FALSE)) %>%
  group_by(u18flag) %>%
  summarize(count = n()) %>%
  mutate(percent = count/sum(count))

repairPivot %>%
  group_by(tot_u6) %>%
  summarize(count = n()) %>%
  mutate(percent = count/sum(count))

repairPivot %>%
  group_by(pregnant) %>%
  summarize(count = n()) %>%
  mutate(percent = count/sum(count))

##### Pests #####
repairPivot %>%
  group_by(pests_llreport) %>%
  summarize(count = n()) %>%
  mutate(percent = count/sum(count))

repairPivot %>%
  group_by(pests_service) %>%
  summarize(count = n()) %>%
  mutate(percent = count/sum(count))

repairPivot %>%
  group_by(pests_traps) %>%
  summarize(count = n()) %>%
  mutate(percent = count/sum(count))

##### Homeowners #####
repairPivot %>%
  filter(home_status != "Rented by you or someone in this household. (Alquilado por usted o alguien en este hogar.)") %>%
  group_by(home_insurance) %>%
  summarize(count = n()) %>%
  mutate(percent = count/sum(count))

repairPivot %>%
  filter(home_status != "Rented by you or someone in this household. (Alquilado por usted o alguien en este hogar.)") %>%
  group_by(home_title) %>%
  summarize(count = n()) %>%
  mutate(percent = count/sum(count))

##### Renters #####
repairPivot %>%
  filter(home_status == "Rented by you or someone in this household. (Alquilado por usted o alguien en este hogar.)") %>%
  group_by(rent_lease) %>%
  summarize(count = n()) %>%
  mutate(percent = count/sum(count))

repairPivot %>%
  filter(home_status == "Rented by you or someone in this household. (Alquilado por usted o alguien en este hogar.)") %>%
  group_by(rent_insurance) %>%
  summarize(count = n()) %>%
  mutate(percent = count/sum(count))

##### Smokers #####
repairPivot %>%
  group_by(smoke_live) %>%
  summarize(count = n()) %>%
  mutate(percent = count/sum(count))

repairPivot %>%
  filter(smoke_live == TRUE) %>%
  group_by(smoke_inside) %>%
  summarize(count = n()) %>%
  mutate(percent = count/sum(count))

##### Water and Air #####
repairPivot %>%
  group_by(windows_open) %>%
  summarize(count = n()) %>%
  mutate(percent = count/sum(count))

repairPivot %>%
  group_by(airfilter) %>%
  summarize(count = n()) %>%
  mutate(percent = count/sum(count))

repairPivot %>%
  group_by(hotwater) %>%
  summarize(count = n()) %>%
  mutate(percent = count/sum(count))

repairPivot %>%
  group_by(airfilter_change) %>%
  summarize(count = n()) %>%
  mutate(percent = count/sum(count))
  
##### Repairs #####
repairPivot %>%
  group_by(repairs_needed) %>%
  summarize(count = n()) %>%
  mutate(percent = count/sum(count))

##### Home Repair Programs #####
repairPivot %>%
  group_by(hrp_available) %>%
  summarize(count = n()) %>%
  mutate(percent = count/sum(count))

repairPivot %>%
  group_by(hrp_information) %>%
  summarize(count = n()) %>%
  mutate(percent = count/sum(count))

repairPivot %>%
  group_by(hrp_inspection) %>%
  summarize(count = n()) %>%
  mutate(percent = count/sum(count))

repairPivot %>%
  group_by(hrp_knowledge) %>%
  summarize(count = n()) %>%
  mutate(percent = count/sum(count))

#### Parcels in Jubilee #####
st_layers(paste0(libDB, "Analytics/04_Projects/Super Census/Jubilee Park/JPCC.gpkg"))

boundary <- st_read(paste0(libDB, "Analytics/04_Projects/Super Census/Jubilee Park/JPCC.gpkg"), layer = "JubileePark_bcBoundary") %>%
  st_transform(crs = 2276)
parcels <- st_read(paste0(libDB, "Analytics/04_Projects/Super Census/Jubilee Park/JPCC.gpkg"), layer = "Parcels_2021_rescounts")

sum(parcels$SFR)
sum(parcels$MFR)
sum(parcels$SFVac)
sum(parcels$TotRes)

repairsf <- repairPivot %>%
  filter(!is.na(long)) %>%
  st_as_sf(coords = c(x = "long", y = "lat"), crs = 4326) %>%
  st_transform(crs = 2276)

sum(parcelsResponse$points)

st_write(boundary, "Data/Jubilee Park Data.gpkg", layer = "JPCC Boundary", delete_layer = TRUE)
#st_write(parcelsResponse, "Data/Jubilee Park Data.gpkg", layer = "JPCC Parcels")
#st_write(repairsf, "Data/Jubilee Park Data.gpkg", layer = "JPCC Repairing Dallas Responses")

##### Attach Responses to Parcels #####
repairsf <- st_read("Data/Jubilee Park Data.gpkg", layer = "JPCC Repairing Dallas Responses")

parcelsResponse <- parcels %>%
  mutate(survey = lengths(st_intersects(., repairsf)),
         safety_carbonmono = lengths(st_intersects(., filter(repairsf, safety_carbonmono == "No (No.)"))),
         safety_extinguisher = lengths(st_intersects(., filter(repairsf, safety_extinguisher == "No (No.)"))),
         safety_smoke = lengths(st_intersects(., filter(repairsf, safety_smoke =="No (No)"))))

st_write(parcelsResponse, "Data/Jubilee Park Data.gpkg", layer = "JPCC Parcels with Responses", delete_layer = TRUE)
