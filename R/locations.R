# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   03:40 PM Saturday, 09 June 2018
# * Copyright: AS IS



#' Get location ids from Senaps
#'
#' @param groups group ids
#' @param near A WKT string eg "POINT (lat lon)"
#' @param radius The distance (in m) around the near point to search
#'
#' @return A vector of location ids
#' @export
get_locations <- function(groups = NULL,
                          near = NULL,
                          radius = NULL) {
    query <- list()
    if (!is.null(groups)) {
        query$groupids <- groups
    }

    if (!is.null(near)) {
        query$near <- near
    }

    if (!is.null(radius)) {
        query$radius <- radius
    }
    response <- request(GET, 'locations', query = query)
    httr::stop_for_status(response)
    response <- httr::content(response)
    ids <- map_chr(response$`_embedded`$locations, 'id')
    return(ids)
}



#' The detail inforamtion of locations in the Senaps
#'
#' @param id Whether return detail information of locations
#' @return A list of location meta information
#' @export
get_location <- function(id) {
    response <- request(GET, paste0('locations/', id))
    httr::stop_for_status(response)
    response <- httr::content(response)
    res <- list()
    res$id <- response$id
    res$description <- response$description
    if (!is.null(response$geojson)) {
        coordinates <- response$geojson$coordinates
        if (length(coordinates) > 0)  res$longitude <- coordinates[[1]]
        if (length(coordinates) > 1) res$latitude <- coordinates[[2]]
        if (length(coordinates) > 2) res$elevation <- coordinates[[3]]
    }
    res$organisation <- map_chr(response$`_embedded`$organisation, 'id')
    res$groups <- map_chr(response$`_embedded`$groups, 'id')
    res$metadata <- response$usermetadata
    res
}


#' Create a new location into Senaps
#'
#' @param id The id of new location. Have to be unique
#' @param description The description of new location
#' @param organisation The organisation of new location. Have to be an existing organisation
#' @param longitude The longitude (degree)
#' @param latitude The latitude (degree)
#' @param elevation The elevation above sea level (m, optional)
#' @param groups The list of groups. Have to be an existing group (optional)
#'
#' @return A list of new location if the new location is successfully created
#' @export
put_location <- function(id, description, organisation,
                         longitude,
                         latitude, elevation = NULL,
                         groups = character(),
                         usermetadata = NULL) {
    if (is.null(usermetadata)) {
        usermetadata <- jsonlite::unbox('')
    }
    body <- list(id = jsonlite::unbox(id),
                 description = jsonlite::unbox(description),
                 organisationid = jsonlite::unbox(organisation),
                 groupids = groups,
                 usermetadata = usermetadata,
                 geoJson = list(type = jsonlite::unbox("Point"),
                                coordinates = c(longitude, latitude, elevation)))

    response <- request(POST, path = paste0('locations/', id),
            body = jsonlite::toJSON(body, auto_unbox = FALSE,
                                    null = 'null'),
            encode = 'json')

    status <- httr::status_code(response)

    if (!(status %in% c(200, 201))) {
        stop(httr::http_status(response)$message)
    }
    return(TRUE)
}


#' Delete an existing Location.
#'
#' @param id The location id
#' @param cascade Remove Location even when is referenced by other objects (eg. Platform deployments or Streams).
#' @export
delete_location <- function(id, cascade = FALSE) {
    invisible(request(DELETE, path = paste0('locations/', id),
                        query = list(cascade = cascade)))
}

