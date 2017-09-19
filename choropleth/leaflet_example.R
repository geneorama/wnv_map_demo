
##==============================================================================
## INITIALIZE
##==============================================================================
## Load libraries
geneorama::loadinstall_libraries(c("leaflet", "dplyr", "ggplot2", "tigris",
                                   "acs", "stringr","knitr"))

##==============================================================================
## CENSUS TRACT DOWNLOAD
##==============================================================================

## Load api key for census.gov
## This key is registered to "gene.leynes@cityofchicago.org"
## Please get your own key at census.gov  It's free and easy
## Also, please delete these comments
api.key.install("60179a2964868d80e37ab0d49e88e654dedf0bc5")

## Find the codes for Illinois and Cook County
# 1. Find the code
# 2. Get the census tract boundary data (put into `dfw`)
# 3. Simple plot of census tracts

lookup_code(state = "Illinois", county = "Cook")
dfw <- tracts(state = '17', county = c('031'))
plot(dfw)

# The census data has the following:
# 1. summary data / labels
# 2. polygons (truncated)
# 3. the "bounding box" coordinates
# 4. details about the geometry assumptions (projection method)
str(dfw)
str(dfw, 2)
str(dfw@polygons[[1]])
str(dfw@data)

##==============================================================================
## ACS DOWNLOAD
##==============================================================================

## Download "B19013_001" from http://factfinder.census.gov/

## Use `acs.fetch` to get B19013_001 (income data).

## I'm not sure what causes the NA Warning... I think it might be the missing
## data in the census tract that appears to be Lake Michigan.  As it turns out,
## Lake Michigan is very sparsely populated by humans (which are the only
## animals included in the US census).
income_data <- acs.fetch(endyear = 2014,
                         geography = geo.make(state = "IL",
                                              county = c(31),
                                              tract = "*"),
                         variable = "B19013_001")

# The income data includes a lot of metadata:
str(income_data)

##==============================================================================
## Convert Census Data To data.frame
##==============================================================================
## `GEOID` is a concatination of state, county, census tract.  The county has
## to be 3 characters, padded by leading zeros (hence the `sprintf` statement).
## In the original NY example the county was already 3 characters long and so
## it wasn't necessary to pad with zeros... pretty tricky to figure that out.
income_df <- data.frame(GEOID = paste0(sprintf("%02.f", income_data@geography$state),
                                       sprintf("%03.f", income_data@geography$county),
                                       income_data@geography$tract),
                        hhincome = income_data@estimate[,1],
                        stringsAsFactors=F)
# str(income_df)

## Clean up some NA values:
geneorama::NAsummary(income_df)
income_df[is.na(income_df$hhincome),"hhincome"] <- 0

##==============================================================================
## Merge Map and Census Data
##==============================================================================

## Thanks to the `sprintf` statement (earlier) the GEOIDs are in the same
## format, and can be merged.
head(dfw@data$GEOID)
head(income_df$GEOID)
geneorama::inin(dfw@data$GEOID, income_df$GEOID)
dfw_merged <- geo_join(dfw, income_df, "GEOID", "GEOID")

##==============================================================================
## Plot
##==============================================================================

## Choose a color palette
## Define the pop up text
## Plot using leaflet (this example uses the `%>%` data flow paradigm)

pal <- colorQuantile("Greens", NULL, n = 6)
popup <- paste0("Median household income: ", as.character(dfw_merged$hhincome))
leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = dfw_merged,
                fillColor = ~pal(dfw_merged$hhincome),
                fillOpacity = 0.7,
                weight = 0.2,
                popup = popup) %>%
    addLegend(pal = pal,
              values = dfw_merged$hhincome,
              position = "bottomright",
              title = "Income in DFW")


##==============================================================================
## Merge data in another way
##==============================================================================

dfw2 <- dfw
head(dfw2@data)
head(merge(dfw@data, income_df, "GEOID", sort = FALSE))
dfw2@data <- merge(dfw@data, income_df, "GEOID", sort = FALSE)

leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = dfw2,
                fillColor = ~pal(dfw2@data$hhincome),
                fillOpacity = 0.7,
                weight = 0.2,
                popup = popup) %>%
    addLegend(pal = pal,
              values = dfw2@data$hhincome,
              position = "bottomright",
              title = "Income in DFW")
