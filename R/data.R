
# Function related with tmd data
#' Retrieving data from the NetCDF Subset Service (NCSS) in Senaps
#'
#' See details from https://www.unidata.ucar.edu/software/thredds/current/tds/reference/NetcdfSubsetServiceReference.html
#'
#' @param path The URL path at which to place the data.
#' @param var Name of variables.
#' @param latitude Point location. units of degrees_east, degrees_north
#' @param longitude Point location. units of degrees_east, degrees_north
#' @param temporal all
#' @param time_var The variable name of time.
#' @param format The format to return
#' @param tz Timezone
#'
#' @return The data in the NCSS
#' @export
get_data <- function(path, var,
                     latitude = NULL, longitude = NULL, temporal = 'all',
                     format = NULL, tz = 'UTC', time_var = NULL) {

    query <- list(var = paste(var, collapse = ','))

    if ((is.null(latitude) & !is.null(longitude)) |
        (!is.null(latitude) & is.null(longitude))) {
        stop('latitude and longitude have to be specified at the same time.')
    }
    if (!is.null(latitude) && !is.null(longitude) &&
        length(latitude) == 1 && length(longitude) == 1) {
        query$latitude <- latitude
        query$longitude <- longitude
    }
    if (temporal == 'all') {
        query$temporal <- 'all'
    }
    if (!is.null(format)) {
        query$accept <- format
    }
    if (grepl('^https?://', path)) {
        response <- GET(path, query = query)
    } else {
        response <- request(GET, paste0('data/', path), query = query)
    }
    httr::stop_for_status(response)

    con <- httr::content(response)

    if (format == 'csv') {
        if (length(con) == 0) {
            stop('No data were read which might be caused by wrong path, or out range of location.')
        }
        res <- read.csv(textConnection(con), header = TRUE)
        if (!is.null(time_var)) {
            res[[time_var]] <- as.POSIXct(as.character(res[[time_var]]),
                               format = "%Y-%m-%dT%H:%M:%SZ", tz = tz)
        }
    } else {
        res <- con
    }
    return(res)
}

