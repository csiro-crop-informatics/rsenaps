

# Variable, global to package's namespace.
# This function is not exported to user space and does not need to be documented.
SENAPS_OPTIONS <- settings::options_manager(sensor_url = 'https://senaps.io/api/sensor/v2/',
                                  analysis_url = 'https://senaps.io/api/analysis/',
                                  tmd_url = "https://senaps.io/thredds/ncss/",
                                  username = '',
                                  password = '',
                                  apikey = '')


#' Set or get options for my package
#'
#' @param ... Option names to retrieve option values or \code{[key]=[value]} pairs to set options.
#'
#' @section Supported options:
#' The following options are supported
#' \itemize{
#'  \item{\code{sensor_url}}{ The url for sensor API}
#'  \item{\code{analysis_url}}{ The url for analysis API}
#'  \item{\code{username}}{ The username for Senaps}
#'  \item{\code{password}}{ The password for Senaps}
#'  \item{\code{apikey}}{ The apikey for Senaps}
#' }
#'
#' @export
senaps_options <- function(...){
    # protect against the use of reserved words.
    settings::stop_if_reserved(...)
    SENAPS_OPTIONS(...)
}

#' Reset global options for pkg
#'
#' @export
senaps_reset <- function() {
    settings::reset(SENAPS_OPTIONS)
}
