

# Variable, global to package's namespace.
# This function is not exported to user space and does not need to be documented.
SENAPS_OPTIONS <- settings::options_manager(sensor_url = 'https://sensor-cloud.io/api/sensor/v2/',
                                  analysis_url = 'https://sensor-cloud.io/api/analysis/',
                                  tmd_url = "https://sensor-cloud.io/thredds/ncss/",
                                  username = '',
                                  password = '',
                                  apikey = '')


#' Set or get options for interacting with the Senaps api
#'
#' @param ... Option names to retrieve option values or \code{[key]=[value]} pairs to set options.
#'
#' @section Supported options:
#' The following options are supported
#' \itemize{
#'  \item{\code{sensor_url}}{ Senaps Sensor Data API url}
#'  \item{\code{analysis_url}}{ Senaps Analysis Service API url}
#'  \item{\code{username}}{ Senaps username}
#'  \item{\code{password}}{ Senaps password}
#'  \item{\code{apikey}}{ Senaps API key}
#' }
#'
#' @section Usage:
#' You are strongly encouraged to use an API key to authenticate with the Senaps API.
#'
#' Keys are available through \url{https://senaps.io/dashboard/#/app/account}.
#'
#' Never include your credentials in a script. Put them in a \code{.Renviron} file in the form:
#'
#'   \code{
#'   SENAPS_APIKEY=putyourkeyhere
#'   }
#'
#' Ensure that the .Renviron file is added to your .gitignore so that the credentials are not added to version control.
#'
#' The API key can then be set with: \code{senaps_options(Sys.getenv('SENAPS_APIKEY'))}
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
