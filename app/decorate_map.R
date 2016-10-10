

decorate_map <- function(m, counts){
    m <- m %>%
        addCircles(lng=~longitude, lat=~latitude,  radius=~TOTAL*5,
                   label=~LABEL, fill="blue", col="blue", weight=1, data=counts[TOTAL_POS!=TOTAL]) %>%
        addCircles(lng=~longitude, lat=~latitude,  radius=~TOTAL_POS*5,
                   label=~LABEL, fill="red", col="red", weight=1, data=counts[TOTAL_POS!=0])
    return(m)
}


