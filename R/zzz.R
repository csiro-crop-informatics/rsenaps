.onLoad <- function(libname, pkgname) {

    senaps.local <- local_senaps()
    if(senaps.local && !is.null(Sys.getenv("SENAPS_APIKEY"))) {
        senaps_options(apikey = Sys.getenv("SENAPS_APIKEY"))
        message("RSenaps is running locally and the API key has been set to the environment variable SENAPS_APIKEY\n")
    } else {
        if(senaps.local) {
            message(
"RSenaps is running locally, but the API key is not set.\n
See ?senaps_options for help on setting the API key.\n")
        } else {
            message(
"If you are running RSenaps locally, use local_senaps(set_local = TRUE), or set an
environment variable LOCAL_SENAPS=TRUE in a .Renviron file.\n
See ?senaps_options for help on setting the API key.\n")
        }
    }
}