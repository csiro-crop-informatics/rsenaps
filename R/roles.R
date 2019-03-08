# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   09:29 PM Wednesday, 08 August 2018
# * Copyright: AS IS

# functions related with roles

#' Add a new role
#'
#' @param id Role id
#' @param type Role type
#' @param organisation Organisation
#' @param group Group
#' @param permissions A vector of of permissions
#'
#' @export
put_role <- function(id, type, organisation, group, permissions) {
    if (length(group) != 1) {
        stop('Require one group')
    }
    permissions <- lapply(as.character(permissions), function(x) {
        list(type = jsonlite::unbox(as.character(x)))})
    body <- list(id = jsonlite::unbox(id),
                 type = jsonlite::unbox(type),
                 organisationid = jsonlite::unbox(organisation),
                 groupid = jsonlite::unbox(group),
                 permissions = permissions)

    response <- request(PUT, path = paste0('roles/', id),
                        body = jsonlite::toJSON(body, auto_unbox = FALSE,
                                                null = 'null'),
                        encode = 'json')
    status <- httr::status_code(response)

    if (!(status %in% c(200, 201))) {
        stop(httr::http_status(response)$message)
    }
    return(TRUE)
}

#' Get roles in Senaps
#'
#' @param groups The group ids
#'
#' @return A character vector of role ids
#' @export
get_roles <- function(groups = NULL) {
    query <- list()
    if (!is.null(groups)) {
        query$groupids <- groups
    }
    response <- request(GET, 'roles', query = query)
    httr::stop_for_status(response)
    response <- httr::content(response)
    ids <- map_chr(response$`_embedded`$roles, 'id')
    return(ids)
}


#' The detail inforamtion of role in the Senaps
#'
#' @param id Role id
#' @return A list of role meta information
#' @export
get_role <- function(id) {
    response <- request(GET, paste0('roles/', id))
    httr::stop_for_status(response)
    response <- httr::content(response)
    # res <- list()
    # res$id <- response$id
    # res$description <- response$description
    # if (!is.null(response$geojson)) {
    #     coordinates <- response$geojson$coordinates
    #     if (length(coordinates) > 0)  res$longitude <- coordinates[[1]]
    #     if (length(coordinates) > 1) res$latitude <- coordinates[[2]]
    #     if (length(coordinates) > 2) res$elevation <- coordinates[[3]]
    # }
    # res$organisation <- map_chr(response$`_embedded`$organisation, 'id')
    # res$groups <- map_chr(response$`_embedded`$groups, 'id')
    # res$metadata <- response$usermetadata
    # res
}
