##
## GWL 2016-10-10
## Putting 20_Map_dev_v1.R into map_demo.Rmd
##

##==============================================================================
## INITIALIZE
##==============================================================================

rm(list=ls())

library("geneorama")
geneorama::set_project_dir("wnv_map_demo")
sourceDir("functions")
loadinstall_libraries(c("leaflet", "data.table", "sp", "rgdal", "KernSmooth",
                        "maptools", "knitr", "shiny"))
source("app/generate_map.R")
source("app/filter_data.R")
source("app/decorate_map.R")

##==============================================================================
## LOAD DATA
##==============================================================================

dat <- readRDS("data/wnv.Rds")
census_tracts <- download_census_tracts()
# wards <- download_ward_map()
wards <- readRDS("data/BoundariesWards.Rds")

##==============================================================================
## MAPBOX KEY
## Register at mapbox.com and create a map... or use the one I made
##==============================================================================
MAPBOX_STYLE_TEMPLATE <- paste0("https://api.mapbox.com/styles/v1/coc375492/",
                                "cirqd7mgf001ygcnombg4jtb4/tiles/256/{z}/{x}/{y}",
                                "?access_token=pk.eyJ1IjoiY29jMzc1NDkyIiwiYSI6ImN",
                                "pcnBldzVqMTBmc3J0N25rZTIxZ3ludDIifQ.DgJIcLDjC1h9MtT8CaJ-pQ")
mb_attribution <- paste("© <a href='https://www.mapbox.com/about/maps/'>Mapbox</a> ",
                        "© <a href='http://www.openstreetmap.org/about/'>OpenStreetMap</a>")

##==============================================================================
## AGGREGATE MOSQUITO DATA
##==============================================================================

## Here's the data without aggregating:
dat

## These steps create fields which will be used for the aggregation step:
dat[ , .N, species]
dat[ , mix := species == "CULEX PIPIENS/RESTUANS"]
dat[ , pip := species == "CULEX PIPIENS"]
dat[ , res := species == "CULEX RESTUANS" ]
dat[ , oth := !mix & !pip & !res]
dat[ , .N, list(species, mix, pip, res, oth)]

dat[ , species_short := species]
dat[grep("CULEX PIPIENS/RESTUANS", species_short), species_short := "mix"]
dat[grep("CULEX PIPIENS", species_short), species_short := "pip"]
dat[grep("CULEX RESTUANS", species_short), species_short := "res"]
dat[grep("CULEX RESTUANS", species_short), species_short := "res"]
dat[!species_short %in% c("pip", "res", "mix"), species_short := "OTHER"]

#Filter data
POSSIBLE_DATES <- sort(unique(dat$date))
get_counts <- function(filterdate){
    if(length(filterdate) == 0){
        ret <- filter_data(dat, rev(POSSIBLE_DATES)[1], "sum")
    } else {
        ret <- filter_data(dat, filterdate, "sum")
    }
    return(ret)
}

##==============================================================================
## BASE  MAP
##==============================================================================

# https://github.com/SimonGoring/ShinyLeaflet-tutorial/blob/master/Shiny-leaflet-tutorial.Rmd

ui <- fluidPage(
    tags$div(title = "Select a week to view",
             selectInput(inputId = "collection_week",
                         label = "Week of collection:",
                         choices = POSSIBLE_DATES[year(POSSIBLE_DATES) == 2017])),
    leafletOutput("MapPlot1", height = 800)
)

#Set up server
server <- function(input, output){
    output$MapPlot1 <- renderLeaflet({
        leaflet() %>%
            addTiles(urlTemplate = MAPBOX_STYLE_TEMPLATE, attribution = mb_attribution) %>%
            addPolygons(data=wards, weight=1, fillOpacity=.05, color="black",
                        label=paste0("WARD ", wards$ward), smoothFactor=.02)

    })

    observe({
        collection_week <- input$collection_week

        counts <- get_counts(collection_week)

        leafletProxy("MapPlot1") %>% clearShapes() %>%
            addTiles(urlTemplate = MAPBOX_STYLE_TEMPLATE, attribution = mb_attribution) %>%
            addPolygons(data=wards, weight=1, fillOpacity=.05, color="black",
                        label=paste0("WARD ", wards$ward), smoothFactor=.02) %>%
            addCircles(lng=~longitude, lat=~latitude,  radius=~log(counts$TOTAL+1)*200,
                       fill="blue", col="blue", weight=1, data=counts,
                       popup = counts$LABEL) %>%
            addCircles(lng=~longitude, lat=~latitude,  radius=~log(counts$TOTAL_POS)*200,
                       fill="red", col="red", weight=1, data=counts,
                       popup = counts$LABEL)

    })
}

#Run app
shinyApp(ui = ui, server = server, options = list(height = 6000))

