
getGoogleGeocode <- function(address){

    require(XML)
    require(httr)

    ## CONSTRUCT QUERIES
    q <- paste0("http://maps.googleapis.com/maps/api/geocode/xml?",
                "address=", paste0(gsub(" ", "+", address), ",+chicago,+il"),
                "&sensor=false")

    ## GET RESPONSES (rate limit for google api is 5/second)
    resp <- lapply(q, function(x) {Sys.sleep(.25);httr::GET(x)})

    ## PARSE CONTENT (TAKES A WHILE)
    doc <- lapply(resp, httr::content, resp, as="text")
    doc_xml <- lapply(doc, XML::xmlInternalTreeParse)

    ## GET LAT / LONG VALUES FROM XML
    latlng <- lapply(doc_xml, function(x){

        ## GET STATUS
        status <- XML::xmlValue(XML::getNodeSet(
            doc = x,
            path = "/GeocodeResponse/status")[[1]])

        ## IF STATUS IS OK, EXTRACT LAT LON
        if(status == "OK"){
            lat <- XML::xmlValue(XML::getNodeSet(
                doc = x,
                path = "/GeocodeResponse/result/geometry/location/lat")[[1]])
            lng <- XML::xmlValue(XML::getNodeSet(
                doc = x,
                path = "/GeocodeResponse/result/geometry/location/lng")[[1]])
        } else {
            lat <- NA_character_
            lng <- NA_character_
        }
        ## return lat lon
        return(list(lat, lng))
    })
    latlng_mat <- t(sapply(latlng, do.call, what=c))
    mode(latlng_mat) <- "double"
    dimnames(latlng_mat) <- list(NULL, c("lat", "lon"))

    ## RETURN RESULTS
    return(latlng_mat)
}

