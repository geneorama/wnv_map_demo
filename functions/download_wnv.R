
download_wnv <- function(infile = "data/wnv.csv",
                         inurl = "https://data.cityofchicago.org/api/views/jqe8-8r6s/rows.csv?accessType=DOWNLOAD"){

    if(!file.exists(infile)){
        download.file(url = inurl, destfile = infile)
    }
    dat <- data.table::fread(infile)
    setnames(dat, tolower(colnames(dat)))
    setnames(dat, gsub(" ", "_", colnames(dat)))
    setnames(dat, "test_date", "date")
    dat <- dat[ , date := as.IDate(date, "%m/%d/%Y")][]
    dat <- dat[ , result := result == "positive"][]
    dat <- dat[ , location := NULL][]
    setkey(dat, date, trap, species, result)

    return(dat)
}
