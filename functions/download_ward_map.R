
download_ward_map <- function(
    infile = "data/BoundariesWards.Rds",
    inurl = "https://data.cityofchicago.org/api/geospatial/sp34-6z76?method=export&format=Shapefile"){

    ## Check for file and download & save if it doesn't exist
    ## Otherwise, just read it in
    if(!file.exists(infile)){
        ## Libraries needed to read / write geo data
        require(rgdal)
        require(sp)

        ## Download and save, unzip, list layers, and read in layer
        download.file(url = inurl,
                      destfile = "data/Boundaries - Wards (2015-).zip",
                      mode = "wb")
        if(file.exists("data/Boundaries - Wards (2015-)")){
            unlink("data/Boundaries - Wards (2015-)",
                   recursive = TRUE, force = TRUE)
        }
        unzip("data/Boundaries - Wards (2015-).zip",
              exdir = "data/Boundaries - Wards (2015-)")
        layername <- ogrListLayers(dsn = "data/Boundaries - Wards (2015-)")
        shp_ward <- readOGR(dsn = "data/Boundaries - Wards (2015-)",
                            layer = layername,
                            stringsAsFactors = FALSE)
        saveRDS(shp_ward, infile)
    } else {
        shp_ward <- readRDS(infile)
    }

    return(shp_ward)
}
