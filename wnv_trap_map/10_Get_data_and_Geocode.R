
##
## GWL 2016-08-18
## Get the West Nile data from the open data portal, and geocode each result
## to census tract and ward, then save the result as wnv.Rds
##


##==============================================================================
## INITIALIZE
##==============================================================================
rm(list=ls())

source("00_Setup.R")
library(geneorama)
loadinstall_libraries(c("data.table", "tigris", "sp", "rgdal"))
sourceDir("functions/")

##==============================================================================
## GET DATA
##==============================================================================

## To download a new file delete the old one: file.remove("data/wnv.csv")
dat <- download_wnv(infile="data/wnv.csv")
census_tracts <- download_census_tracts(infile = "data/censustracts.Rds")
wards <- download_ward_map(infile = "data/BoundariesWards.Rds")

## Keep only rows that are not missing location data
# dat <- dat[!is.na(latitude)]

##------------------------------------------------------------------------------
## Google geocoder
##------------------------------------------------------------------------------

## For missing locations get the block
addresses_with_missing_coords <- dat[is.na(latitude) , unique(block)]
## Replace X with 0
addresses_with_missing_coords_mod <- gsub("X", "0", addresses_with_missing_coords)
## Look up coords in Google
coords_found <- getGoogleGeocode(addresses_with_missing_coords_mod)
coords_found_dt <- data.table(block=addresses_with_missing_coords, coords_found)
coords_found_dt

dat <- merge(dat, coords_found_dt, "block", all.x=TRUE)
dat[is.na(latitude), latitude := lat]
dat[is.na(longitude), longitude := lon]
NAsummary(dat)
dat$lat <- NULL
dat$lon <- NULL

rm(addresses_with_missing_coords, addresses_with_missing_coords_mod,
   coords_found, coords_found_dt)

##==============================================================================
## GEOCODE CENSUS BLOCKS
##==============================================================================
## Convert data to an "sp" object by setting the coordinates
sp::coordinates(dat) <- c("longitude", "latitude")

## Match the projection attributes of data to "census_blocks"
dat@proj4string <- census_tracts@proj4string

## Geocode data to census block
system.time(geo <- sp::over(dat, census_tracts))

## Combine into result
dat <- as.data.table(dat)
dat[ , CENSUS_TRACT := geo$GEOID]
dat[ , .N, keyby=CENSUS_TRACT]

##==============================================================================
## GEOCODE WARDS
##==============================================================================
## Convert data to an "sp" object by setting the coordinates
sp::coordinates(dat) <- c("longitude", "latitude")

# census_blocks@proj4string
# wards@proj4string
wards <- spTransform(wards, CRS("+proj=longlat +datum=WGS84"))

# census_blocks@proj4string
# wards@proj4string
wards <- spTransform(wards, CRS("+proj=longlat +datum=WGS84"))

## Match the projection attributes of data to "wards"
dat@proj4string <- wards@proj4string

## Geocode data to ward
system.time(geo <- sp::over(dat, wards))

## Combine into result
dat <- as.data.table(dat)
dat[ , WARD := as.integer(geo$ward)]
dat[ , .N, keyby=WARD]

##==============================================================================
## SAVE RESULT
##==============================================================================
saveRDS(dat, "data/wnv.Rds")

