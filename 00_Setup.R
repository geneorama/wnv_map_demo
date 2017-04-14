
##------------------------------------------------------------------------------
## CLEAR THE GLOBAL ENVIRONMENT
##------------------------------------------------------------------------------
rm(list=ls(), pos=.GlobalEnv)

##------------------------------------------------------------------------------
## INSTALL DEPENDENCIES IF MISSING
##------------------------------------------------------------------------------
if(!"devtools" %in% rownames(installed.packages())){
    install.packages("devtools",
                     dependencies = TRUE,
                     repos = "http://cran.rstudio.com/")
}

if(!"Rcpp" %in% rownames(installed.packages())){
    install.packages("Rcpp",
                     dependencies = TRUE,
                     repos = "http://cran.rstudio.com/")
}

if(!"geneorama" %in% rownames(installed.packages())){
    devtools::install_github('geneorama/geneorama')
}

if(installed.packages()["geneorama","Version"] < "1.5.0"){
    devtools::install_github('geneorama/geneorama')
}

##------------------------------------------------------------------------------
## DETACH ANY "NON STANDARD" PACKAGES
##------------------------------------------------------------------------------
# geneorama::detach_nonstandard_packages()

##------------------------------------------------------------------------------
## UPDATE PACKAGES?
##------------------------------------------------------------------------------
## If you're having problems, try updating packages:
## ** NOTE: ask=FALSE in the update packages command below **
# getCRANmirrors()
# update.packages(repos = "https://cloud.r-project.org/", ask = FALSE)
# update.packages(repos = "https://cloud.r-project.org/")

