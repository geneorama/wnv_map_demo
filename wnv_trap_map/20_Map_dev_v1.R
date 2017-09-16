

##
## GWL 2016-07-21
##

##==============================================================================
## INITIALIZE
##==============================================================================

rm(list=ls())

library("geneorama")
sourceDir("functions")
loadinstall_libraries(c("leaflet", "data.table", "sp", "rgdal", "KernSmooth",
                        "maptools"))

##==============================================================================
## LOAD DATA
##==============================================================================

dat <- readRDS("data/wnv.Rds")
census_tracts <- download_census_tracts()
wards <- download_ward_map()
wards <- readRDS("data/BoundariesWards.Rds")

##==============================================================================
## MAPBOX KEY
## Register at mapbox.com and create a map... or use the one I made
##==============================================================================
MAPBOX_STYLE_TEMPLATE <- paste0("https://api.mapbox.com/styles/v1/coc375492/",
                                "cirqd7mgf001ygcnombg4jtb4/tiles/256/{z}/{x}/{y}",
                                "?access_token=pk.eyJ1IjoiY29jMzc1NDkyIiwiYSI6ImN",
                                "pcnBldzVqMTBmc3J0N25rZTIxZ3ludDIifQ.DgJIcLDjC1h9MtT8CaJ-pQ")

##==============================================================================
## AGGREGATE MOSQUITO DATA
##==============================================================================

## Here's the data without aggregating:
dat

## These steps create fields which will be used for the aggregation step:
dat[ , .N, species]
dat[ , pip := grepl("PIPIENS", species)]
dat[ , res := grepl("RESTUANS", species)]
dat[ , oth := !pip & !res]
dat[ , .N, list(species, pip, res, oth)]

dat[ , PIPIENS := (pip / (pip + res + oth)) * number_of_mosquitoes]
dat[ , RESTUANS := (res / (pip + res + oth)) * number_of_mosquitoes]
dat[ , OTHER := (oth / (pip + res + oth)) * number_of_mosquitoes]

dat[ , pip := NULL]
dat[ , res := NULL]
dat[ , oth := NULL]

## Now the data has some new fields that are
dat[ , list(.N, sum(PIPIENS), sum(RESTUANS), sum(OTHER)), list(species)]

## This is the aggregation step:
counts <- dat[date == max(date),
              list(TOTAL = sum(number_of_mosquitoes),
                   TOTAL_POS = sum(number_of_mosquitoes[result==T]),
                   PIPIENS = sum(PIPIENS),
                   PIPIENS_POS = sum(PIPIENS[result==T]),
                   RESTUANS = sum(RESTUANS),
                   RESTUANS_POS = sum(RESTUANS[result==T]),
                   OTHER = sum(OTHER),
                   OTHER_POS = sum(OTHER[result==T])),
              keyby = list(TRAP = trap,
                           WARD,
                           latitude,
                           longitude)]

## Here's the data after aggregating:
counts

##==============================================================================
## RESULT MAP
##==============================================================================

m <- leaflet() %>%
    addCircles(lng=~longitude, lat=~latitude,  radius=~TOTAL * 5,
               fill="blue", col="blue", weight=1, data=counts) %>%
    addCircles(lng=~longitude, lat=~latitude,  radius=~TOTAL_POS * 5,
               fill="red", col="red", weight=1, data=counts)
mb_attribution <- paste("© <a href='https://www.mapbox.com/about/maps/'>Mapbox</a> ",
                        "© <a href='http://www.openstreetmap.org/about/'>OpenStreetMap</a>")

## Various Examples
m
m %>% addTiles()
m %>% addProviderTiles("CartoDB.Positron")
m %>% addTiles(urlTemplate = MAPBOX_STYLE_TEMPLATE, attribution = mb_attribution)
m %>% addTiles(urlTemplate = MAPBOX_STYLE_TEMPLATE, attribution = mb_attribution) %>%
    addProviderTiles("Stamen.TonerLines", options = providerTileOptions(opacity = 0.35))
m %>% addProviderTiles("MtbMap") %>%
    addProviderTiles("Stamen.TonerLines", options = providerTileOptions(opacity = 0.35)) %>%
    addProviderTiles("Stamen.TonerLabels")

## Create a field called "LABEL" to show the totals for each trap on the map
counts[,  LABEL := paste(paste0("TRAP: ", TRAP),
                         paste0("TOTAL:", TOTAL_POS, "/", TOTAL),
                         paste0("PIPIENS:", PIPIENS_POS, "/", PIPIENS),
                         paste0("RESTUANS:", RESTUANS_POS, "/", RESTUANS),
                         paste0("OTHER:", OTHER_POS, "/", OTHER),
                         sep = " ")]

## Final example
leaflet() %>%
    addTiles(urlTemplate = MAPBOX_STYLE_TEMPLATE, attribution = mb_attribution) %>%
    addPolygons(data=wards, weight=1, fillOpacity=.05, color="black", label=paste0("WARD ", wards$ward), smoothFactor=.02) %>%
    addCircles(lng=~longitude, lat=~latitude,  radius=~TOTAL*5, label=~LABEL, fill="blue", col="blue", weight=1, data=counts) %>%
    addCircles(lng=~longitude, lat=~latitude,  radius=~TOTAL_POS*5, label=~LABEL, fill="red", col="red", weight=1, data=counts)








