

#' Get the authenticated user using root api call
#' :reference: https://data.sense-t.org.au/api/sensor/v2/api-docs/#!/default/
#' @export
get_me <- function() {
    response <- request(GET)
    httr::stop_for_status(response)
    response <- httr::content(response)
    userid <- response$`_embedded`$user[[1]]$id
    return (get_users(userid))
}

#' Get a user
#'
#' @param userid user identifier eg email address
#' @export
get_users <- function(userid) {
  query <- list(userid = userid)
  response <- request(GET, paste0('users/', userid), query = query)
  response <- httr::content(response)
  return (response)
}

