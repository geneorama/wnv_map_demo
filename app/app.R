##
## GWL 2016-10-10
## Putting 20_Map_dev_v1.R into map_demo.Rmd
##

##==============================================================================
## INITIALIZE
##==============================================================================

rm(list=ls())

library("geneorama")
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

##==============================================================================
## BASE  MAP
##==============================================================================
m <- leaflet() %>%
    addTiles(urlTemplate = MAPBOX_STYLE_TEMPLATE, attribution = mb_attribution) %>%
    addPolygons(data=wards, weight=1, fillOpacity=.05, color="black",
                label=paste0("WARD ", wards$ward), smoothFactor=.02)
POSSIBLE_DATES <- sort(unique(dat$date))

POSSIBLE_DATES

# generate_map(m, dat, rev(POSSIBLE_DATES)[1], "sum")
# generate_map(m, dat, rev(POSSIBLE_DATES)[2], "sum")
# generate_map(m, dat, rev(POSSIBLE_DATES)[3], "sum")
# generate_map(m, dat, rev(POSSIBLE_DATES)[4], "sum")
# generate_map(m, dat, rev(POSSIBLE_DATES)[5], "sum")
# generate_map(m, dat, rev(POSSIBLE_DATES)[6], "sum")
# generate_map(m, dat, rev(POSSIBLE_DATES)[7], "sum")
# generate_map(m, dat, rev(POSSIBLE_DATES)[8], "sum")
# generate_map(m, dat, rev(POSSIBLE_DATES)[9], "sum")
# generate_map(m, dat, rev(POSSIBLE_DATES)[10], "sum")
# generate_map(m, dat, rev(POSSIBLE_DATES)[11], "sum")
# generate_map(m, dat, rev(POSSIBLE_DATES)[12], "sum")
# generate_map(m, dat, rev(POSSIBLE_DATES)[13], "sum")
# generate_map(m, dat, rev(POSSIBLE_DATES)[14], "sum")
# generate_map(m, dat, rev(POSSIBLE_DATES)[15], "sum")
# generate_map(m, dat, rev(POSSIBLE_DATES)[16], "sum")
# generate_map(m, dat, rev(POSSIBLE_DATES)[17], "sum")
# generate_map(m, dat, rev(POSSIBLE_DATES)[18], "sum")


ui <- shinyUI(fluidPage(
    sidebarPanel(h5("", width=2),
                 selectInput(inputId="cur_date", label=h4("SLECT DATE"),
                                    choices = POSSIBLE_DATES),
                 # checkboxGroupInput(inputId="AppFlag",label=h4("Application"),
                 #                    choices=setNames(object=c("a","b","c","d"),
                 #                                     nm=c("a","b","c","d")),
                 position="left"),

    #App mainPanel content and styles
    mainPanel(fluidRow(leafletOutput(outputId="lmap")))
))

#Set up server
server <- function(input, output){
    #Build leaflet map
    lmap <- m

    #Filter data
    get_counts <- reactive({
        # filter_data(dat, rev(POSSIBLE_DATES)[1], "sum")
        if(length(input$cur_date) == 0){
            ret <- filter_data(dat, rev(POSSIBLE_DATES)[1], "sum")
        } else {
            ret <- filter_data(dat, input$cur_date, "sum")
        }
        return(ret)
    })

    observe({
        counts <- get_counts()
        if(nrow(counts)==0) {
            print("Nothing selected")
            leafletProxy("lmap") %>% clearMarkerClusters()
        }
        else{
            leafletProxy("lmap", data=counts) %>%
                clearMarkerClusters() %>%
                addCircles(lng=~longitude, lat=~latitude,  radius=~TOTAL*5,
                           label=~LABEL, fill="blue", col="blue", weight=1,
                           data=counts[TOTAL_POS!=TOTAL]) %>%
                addCircles(lng=~longitude, lat=~latitude,  radius=~TOTAL_POS*5,
                           label=~LABEL, fill="red", col="red",
                           weight=1, data=counts[TOTAL_POS!=0])
        }
    })


    output$lmap <- renderLeaflet(lmap)
}

#Run app
shinyApp(ui = ui, server = server)



