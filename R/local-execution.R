#' Test if execution is local
#'
#' This function looks for the environment variable SENAPS_LOCAL, and if it set to TRUE, the function will return TRUE.
#'
#' The easiest way to set the variable interactively is with the \code{set_local} argument to this
#' function \code{local_senaps(set_local = TRUE)}, which will set the variable for the session.
#'
#' Alternatively, the variable can be set in a .Renviron file with \code{SENAPS_LOCAL=TRUE)}.
#'
#' @param set_local A logical value. Should the local environment be set to TRUE?
#'
#' @return logical
#' @export
#'
#' @examples
#' \donttest{
#'   if(local_senaps()) {
#'     # some logic to do locally, but not when running on Senaps
#'   }
#' }
local_senaps <- function(set_local = FALSE) {
    # Look to see if the environment variable is already set, and if so, return true
    if(isTRUE(as.logical(Sys.getenv("SENAPS_LOCAL")))) {
        return(TRUE)
    } else {

        # if the environment variable is not set, and set_local = TRUE, set the environment variable and return TRUE
        if(set_local) {

            Sys.setenv(SENAPS_LOCAL = TRUE)
            message("\nSENAPS_LOCAL environment variable has been set to TRUE, will assume not running on Senaps.\n")
            return(TRUE)

        } else {

            if(isFALSE(as.logical(Sys.getenv("SENAPS_LOCAL")))) {
                message("\nSENAPS_LOCAL environment variable is set to FALSE, will assume running on Senaps.\n")
                return(FALSE)
            } else {
                message("\nThere is no SENAPS_LOCAL environment variable set, will assume running on Senaps.\n")
                return(FALSE)
            }
        }

    }
}
