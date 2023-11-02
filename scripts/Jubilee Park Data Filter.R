library(tidyverse)
library(sf)
library(rio)

#libDB <- "E:/CPAL Dropbox/"
libDB <- "C:/Users/micha/CPAL Dropbox/"

dcadlu <- st_read(paste0(libDB, "Data Library/Parcel Data/DCAD/Data/DCAD_Parcels.gpkg"), layer = "DCAD Land Use Parcels")

neighborhoods <- st_read(paste0(libDB, "Data Library/bcWORKSHOP/POP_Neighborhoods.shp"))

jubilee <- neighborhoods %>%
  filter(name == "Jubilee Park") %>%
  st_transform(crs = 6584)

neighparcels <- dcadlu %>%
  st_transform(crs = 6584) %>%
  .[jubilee, ]

names(neighparcels)
plot(neighparcels["totacc"])

st_write(jubilee, "Data/Jubilee Park Data.gpkg", layer = "Jubilee Neighborhood Boundary", delete_layer = TRUE)
st_write(neighparcels, "Data/Jubilee Park Data.gpkg", layer = "Parcels and Land Use", delete_layer = TRUE)

#DCAD property information
dcadacc <- import(paste0(libDB, "Data Library/Parcel Data/DCAD/2023/account_info.csv"))

dcadappr <- import(paste0(libDB, "Data Library/Parcel Data/DCAD/2023/account_apprl_year.csv")) %>%
  filter(SPTD_CODE %in% c("A11", "A12", "A13", "B11", "B12")) %>%
  filter(GIS_PARCEL_ID %in% neighparcels$GIS_PARCEL_ID) %>%
  left_join(., dcadacc) %>%
  select(-APPRAISAL_YR, -LAND_VAL)

dcadres <- import(paste0(libDB, "Data Library/Parcel Data/DCAD/2023/res_detail.csv")) %>%
  left_join(dcadappr, .) %>%
  filter(!is.na(NUM_BEDROOMS)) %>%
  inner_join(dcadlu, .)

dcadcom <- import(paste0(libDB, "Data Library/Parcel Data/DCAD/2023/com_detail.csv")) %>%
  left_join(dcadappr, .) %>%
  filter(!is.na(NUM_UNITS)) %>%
  inner_join(dcadlu, .)

st_write(dcadcom, "Data/Jubilee Park Data.gpkg", layer = "Jubilee MFR", delete_layer = TRUE)
st_write(dcadres, "Data/Jubilee Park Data.gpkg", layer = "Jubilee SFR", delete_layer = TRUE)

sum(dcadcom$NUM_UNITS)
