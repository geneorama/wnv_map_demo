

filter_data <- function(dat, CUR_DATE, type = c("sum", "count")){
    if(type == "count"){

        counts <- dat[date == max(date),
                      list(TOTAL = sum(number_of_mosquitoes),
                           TOTAL_POS = sum(number_of_mosquitoes[result==T]),
                           Ns_POS = paste(number_of_mosquitoes[result==TRUE], collapse=","),
                           Ns_NEG = paste(number_of_mosquitoes[result==FALSE], collapse=","),
                           SPECIES_POS = paste(species_short[result==TRUE], collapse=","),
                           SPECIES_NEG = paste(species_short[result==FALSE], collapse=",")),
                      keyby = list(TRAP = trap,
                                   WARD,
                                   latitude,
                                   longitude)]
        counts <- counts[,
                         LABEL := htmltools::HTML(
                             paste(paste0("TRAP: ", TRAP),
                                   paste0("TOTAL: ", TOTAL_POS, " / ", TOTAL),
                                   paste0("COUNTS_POS: ", Ns_POS),
                                   paste0("COUNTS_NEG: ", Ns_NEG),
                                   paste0("SPECIES_POS: ", SPECIES_POS),
                                   paste0("SPECIES_NEG: ", SPECIES_NEG),
                                   sep = "<br>")),
                         keyby = list(TRAP = TRAP,
                                      WARD,
                                      latitude,
                                      longitude)][]

    } else {
        if(type != "sum") warning("type is not equal to count or sum, using count")

        counts <- dat[date == CUR_DATE,
                      list(TOTAL = sum(number_of_mosquitoes),
                           TOTAL_POS = sum(number_of_mosquitoes[result==T]),
                           MIX = sum(number_of_mosquitoes[species_short=="mix"]),
                           MIX_POS = sum(number_of_mosquitoes[species_short=="mix" & result==T]),
                           PIPIENS = sum(number_of_mosquitoes[species_short=="pip"]),
                           PIPIENS_POS = sum(number_of_mosquitoes[species_short=="pip" & result==T]),
                           RESTUANS = sum(number_of_mosquitoes[species_short=="res"]),
                           RESTUANS_POS = sum(number_of_mosquitoes[species_short=="res" & result==T]),
                           OTHER = sum(number_of_mosquitoes[species_short=="OTHER"]),
                           OTHER_POS = sum(number_of_mosquitoes[species_short=="OTHER" & result==T])),
                      keyby = list(TRAP = trap,
                                   WARD,
                                   latitude,
                                   longitude)]
        counts <- counts[,
                         LABEL := htmltools::HTML(
                             paste(paste0("TRAP: ", TRAP),
                                   paste0("TOTAL: ", TOTAL_POS, " / ", TOTAL),
                                   paste0("MIX: ", MIX_POS, " / ", MIX),
                                   paste0("PIPIENS: ", PIPIENS_POS, " / ", PIPIENS),
                                   paste0("RESTUANS: ", RESTUANS_POS, " / ", RESTUANS),
                                   paste0("OTHER: ", OTHER_POS, " / ", OTHER),
                                   sep = "<br>")),
                         keyby = list(TRAP = TRAP,
                                      WARD,
                                      latitude,
                                      longitude)][]


    }
    return(counts)
}


