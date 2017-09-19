
##
## The full part of the filename refers to the fact that this map uses each
## collection, rather than the aggregated collections at a site. So, if 3
## samples came back and 2 were positive, it's modeled as three binomial trials
## at the exact same location.
##

##==============================================================================
## INITIALIZE
##==============================================================================

rm(list=ls())

library("geneorama")
loadinstall_libraries("leaflet")
loadinstall_libraries("data.table")
loadinstall_libraries("sp")
loadinstall_libraries("rgdal")
loadinstall_libraries("rgeos")
loadinstall_libraries("mgcv")
# loadinstall_libraries("maptools")
# loadinstall_libraries("KernSmooth")

sourceDir("functions/")

##==============================================================================
## LOAD DATA
##==============================================================================
dat <- readRDS("data/wnv.Rds")
census_tracts <- download_census_tracts()
wards <- readRDS("data/BoundariesWards.Rds")

# options(tigris_refresh=TRUE)
# census_blocks <- download_census_blocks()

##==============================================================================
## DATA MANIPULATION
##==============================================================================
setnames(dat,  c("season_year"), c("season"))

##==============================================================================
## ADD LAT LON TO VI DATA
##==============================================================================
dat[ , x := latlon2stateplane(latitude = latitude, longitude = longitude)$x]
dat[ , y := latlon2stateplane(latitude = latitude, longitude = longitude)$y]

##==============================================================================
## GEOCODE USING SHAPE FILES
##==============================================================================
## (not needed here, already geocoded if retreived from data portal)
# sp::coordinates(dat) <- c("longitude", "latitude")
# dat@proj4string <- census_tracts@proj4string
# geo <- sp::over(dat, census_tracts)
# dat <- as.data.table(dat, stringsAsFactors = FALSE)
# dat[ , census_tract2 := as.character(geo$GEOID)]

##==============================================================================
## TRIM CENSUS TRACT DATA TO WARD BOUNDARIES (MANUALLY)
##==============================================================================
## Subset census tracts to chicago
tract_centers <- census_tracts@data[ , c("INTPTLAT", "INTPTLON")]
tract_centers <- as.data.table(sapply(tract_centers, as.numeric))
sp::coordinates(tract_centers) <- c("INTPTLON", "INTPTLAT")
tract_centers@proj4string <- wards@proj4string
census_tracts_chi <- census_tracts[!is.na(sp::over(tract_centers, wards))[,1], ]

## Combine wards into a single city outline
city_outline <- gUnaryUnion(as(census_tracts_chi, "SpatialPolygons"), census_tracts_chi$STATEFP)
plot(city_outline)

##==============================================================================
## SUBSET DATA / MODEL BASIS
##==============================================================================
dat[season == 2017 & week == 31]
d <- dat[i = season==2017 & week == 31,
         j = list(pos = sum(number_of_mosquitoes[result == TRUE]),
                  count = sum(number_of_mosquitoes),
                  N = .N),
         keyby = list(trap, longitude, latitude, x, y,
                      census_tract = CENSUS_TRACT)]


##==============================================================================
## MAPBOX KEY AND ATTRIBUTION
##==============================================================================
MAPBOX_STYLE_TEMPLATE <- paste0("https://api.mapbox.com/styles/v1/coc375492/",
                                "cirqd7mgf001ygcnombg4jtb4/tiles/256/{z}/{x}/{y}",
                                "?access_token=pk.eyJ1IjoiY29jMzc1NDkyIiwiYSI6ImN",
                                "pcnBldzVqMTBmc3J0N25rZTIxZ3ludDIifQ.DgJIcLDjC1h9MtT8CaJ-pQ")

mb_attribution <- paste("© <a href='https://www.mapbox.com/about/maps/'>Mapbox</a> ",
                        "© <a href='http://www.openstreetmap.org/about/'>OpenStreetMap</a>")


##==============================================================================
## binomial gam model
##==============================================================================

model <- gam(d[ , pos!=0] ~ s(x, y, k=50), data=d, family=binomial)
op <- par(mfrow=c(2,2));gam.check(model);par(op)

##==============================================================================
## prediction grid
##==============================================================================

## CREATE PREDICTION GRID
pgrid <- as.data.table(expand.grid(seq(census_tracts_chi@bbox["x","min"],
                                       census_tracts_chi@bbox["x","max"],
                                       length.out = 200),
                                   seq(census_tracts_chi@bbox["y","min"],
                                       census_tracts_chi@bbox["y","max"],
                                       length.out = 200)))
pgrid <- latlon2stateplane(pgrid$Var2, pgrid$Var1)

## GEOCODE pgrid TO THE CENSUS TRACTS
sp::coordinates(pgrid) <- c("longitude", "latitude")
pgrid@proj4string <- census_tracts_chi@proj4string
geo <- sp::over(pgrid, census_tracts_chi)
pgrid <- as.data.table(pgrid, stringsAsFactors = FALSE)
pgrid[ , census_tract_chi := as.character(geo$GEOID)]

## REMOVE pgrid VALUES THAT ARE NOT IN A GEOCODE (OUTSIDE THE CITY)
pgrid <- pgrid[!is.na(census_tract_chi)]

## PREDICT OVER GRID
pgrid[ , yhat := round(predict(model, pgrid, type = "response"), 3)]

## AVERAGE GRID PREDICTIONS PER CENSUS TRACT
census_tract_predictions <- pgrid[i = TRUE,
                                  j = list(meanyhat = mean(yhat),
                                           maxyhat = max(yhat)),
                                  by = list(GEOID = census_tract_chi)]

## ADD TRACT PREDICTIONS TO TRACT MAP DATA
ii <- match(census_tracts_chi@data$GEOID, census_tract_predictions$GEOID)
census_tracts_chi@data$meanyhat <- census_tract_predictions$meanyhat[ii]
census_tracts_chi@data$maxyhat <- census_tract_predictions$maxyhat[ii]

## DEFINE LEAFLET COLOR SCHEME
ctpal <- colorNumeric("Greens", NULL, n = 4)

## DEFINE POPUPS AND COLORS
popup_trap <- d[ , paste0(trap, ": ", pos, "/", N, " (",count,")")]
col_trap <- ifelse(d$pos==0, "orange", "red")

## SIMPLE MAP
leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addCircleMarkers(lng = ~ longitude, lat = ~ latitude, data = d,
                     radius = 3, stroke = FALSE, fillOpacity = 0.95,
                     color = col_trap, popup = popup_trap) %>%
    addPolygons(data = city_outline, fill = FALSE, color = "black", weight = 1)

## TILE MAP
leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = census_tracts_chi, fillColor = ~ ctpal(meanyhat),
                fillOpacity = 0.7, weight = 0.2)  %>%
    addCircleMarkers(lng = ~ longitude, lat = ~ latitude, data = d,
                     radius = 3, stroke = FALSE, fillOpacity = 0.95,
                     color = col_trap, popup = popup_trap) %>%
    addPolygons(data = city_outline, fill = FALSE, color = "black", weight = 1)  %>%
    addLegend(pal = ctpal, values = seq(0,1,.1),
              position = "bottomright", title = "Vector Index Values")

## GRID OVERLAY ON TILE MAP
leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = census_tracts_chi, fillColor = ~ ctpal(meanyhat),
                fillOpacity = 0.7, weight = 0.2)  %>%
    addCircleMarkers(lng = ~ longitude, lat = ~ latitude, data = pgrid,
                     radius = 2, color = ctpal(pgrid$yhat), stroke = FALSE,
                     fillOpacity = 0.95) %>%
    addCircleMarkers(lng = ~ longitude, lat = ~ latitude, data = d,
                     radius = 3, stroke = FALSE, fillOpacity = 0.95,
                     color = col_trap, popup = popup_trap) %>%
    addPolygons(data = city_outline, fill = FALSE, color = "black", weight = 1)  %>%
    addLegend(pal = ctpal, values = seq(0,1,.1),
              position = "bottomright", title = "Vector Index Values")



