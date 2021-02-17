

#' A R package to communicate with Senaps platform.
#'
#' @export
rsenaps <- function() {

}

#' Format data and time for Senaps
#'
#' @param dt Date and time format
#' @param tz timezone
#' @param url_encode Whether to use RUL encode
#'
#' @return A string for Senaps
#' @export
format_datetime <- function(dt, tz = 'GMT', url_encode = FALSE) {
    if ('Date' %in% class(dt)) {
        dt <- as.POSIXct(format(dt), tz = tz)
    }
    r <- format(dt, format="%Y-%m-%dT%H:%M:%OS3%z")
    if (url_encode) {
        r <- URLencode(r, reserved = TRUE)
    }
    r
}


#' Perform a request to Senaps
#'
#' @param method The method in the httr package, e.g. GET, POST
#' @param path The path of request
#' @param query The query of request
#' @param ... Other arguments of request
#'
#' @return The contents of response
#' @export
request <- function(method,
                           path = '/',
                           query = list(),
                           ...) {
    # Remove the leading "/" if it has one.
    path <- gsub('^/*(.*)$', "\\1", path)
    # Get the base commands in the senaps
    commands <- strsplit(path, '/')[[1]][1]

    host <- NULL
    if (commands %in% c("streams", "groups", "observations",
                        "locations", "platforms", "roles", "aggregation")) {
        host <- SENAPS_OPTIONS("sensor_url")
    } else if (commands %in% c("base-images", "models", "workflows", 'schedules')) {
        host <- SENAPS_OPTIONS("analysis_url")
    } else if (commands %in% c('data')) {
        host <- SENAPS_OPTIONS("tmd_url")
        path <- gsub('^(data/)(.*)', '\\2', path)
    } else {
        stop(paste0("Not implemented api for \"", commands, "\""))
    }

    url <- httr::modify_url(host,
                      path = gsub("/+", "/",
                                  paste(httr::parse_url(host)$path, path, sep = "/")))
    if (!is.null(SENAPS_OPTIONS('apikey')) & nchar(SENAPS_OPTIONS('apikey')) > 0) {
        query$apikey <- SENAPS_OPTIONS('apikey')
        response <- method(url, query = query, ...)
    } else if (!is.null(SENAPS_OPTIONS('username')) & nchar(SENAPS_OPTIONS('username')) > 0 &
               !is.null(SENAPS_OPTIONS('password')) & nchar(SENAPS_OPTIONS('password')) > 0) {
        response <- method(url, httr::authenticate(SENAPS_OPTIONS('username'),
                                                   SENAPS_OPTIONS('password')),
                           query = query, ...)
    } else {
        stop('apikey or username/password should be specified.')
    }
    response
}


#' Parse the parameter in a string with key and value
#'
#' @param p The parameter
#'
#' @return A list of parameter values
#' @export
parse_parameter <- function(p) {
    paras <- strsplit(p, ' *, *')[[1]]
    res <- list()
    for (i in seq(along = paras)) {
        para <- strsplit(paras[i], ' *: *')[[1]]
        res[[para[1]]] <- para[2]
    }
    res
}
