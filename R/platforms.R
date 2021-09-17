
#' Create a new platform or update an existing platform
#'
#' @param id Platform ID. Have to be unique
#' @param name Platform name
#' @param organisation Platform organisation. Have to be an existing organisation
#' @param groups Platform groups. Have to be an existing groups (optional)
#' @param streams Platform streams Have to be an existing streams (optional)
#' @param deployments Platform deployments (optional)
#' @param usermetadata  Arbitrary usermetadata. Any valid list.
#'
#' @return TRUE if sucessful.
#' @export
#'
#' @examples
#' \dontrun{
#'  id <- 'rsenaps.platform.test1'
#'  name <- "RSenaps Platform Test1"
#'  organisation <- "sandbox"
#'  put_platform(id =  id, name = name, organisation = organisation)
#' }
#'
put_platform <- function(id, name = id, organisation, groups = NULL,
                         streams = NULL, deployments = NULL,
                         usermetadata = NULL
                         ) {
    if (is.null(usermetadata)) {
        usermetadata <- jsonlite::unbox('')
    }
    if (is.null(groups)) {
        groups <- character()
    }
    if (is.null(streams)) {
        streams <- character()
    }
    if (is.null(deployments)) {
        deployments <- character()
    }
    body <- list(id = jsonlite::unbox(id),
                 name = jsonlite::unbox(name),
                 organisationid = jsonlite::unbox(organisation),
                 groupids = groups,
                 streamids = streams,
                 deployments = deployments,
                 usermetadata = usermetadata)


    response <- request(POST, path = paste0('platforms/', id),
                        body = jsonlite::toJSON(body, auto_unbox = FALSE,
                                                null = 'null'),
                        encode = 'json')
    status <- httr::status_code(response)

    if (!(status %in% c(200, 201))) {
        stop(httr::http_status(response)$message)
    }
    return(TRUE)
}

#' Get details about a platform.
#'
#' @param id The platform id
#'
#' @return A list of platform details. NULL if platform does not exist.
#' @export
#'
#' @examples
#' \dontrun{
#'   get_platform('rsenaps.platform.test1')
#' }
#'
get_platform <- function(id) {
    response <- request(GET, paste0('platforms/', id))
    if (httr::status_code(response) == 404) {
        return(NULL)
    }
    httr::stop_for_status(response)
    response <- httr::content(response)
    res <- list()
    res$id <- response$id
    res$name <- response$name
    res$organisation <- response$`_embedded`$organisation %>% map_chr('id')
    res$groups <- response$`_embedded`$groups %>% map_chr('id')
    res$streams <- response$`_embedded`$streams %>% map_chr('id')
    deployments <- response$`_embedded`$platformdeployment
    d <- list()
    for (i in seq(along = deployments)) {
        start <- NULL
        if (!is.null(deployments[[i]]$validTime$start)) {
            start <- as.POSIXct(deployments[[i]]$validTime$start, tz = 'GMT', format='%Y-%m-%dT%H:%M:%OSZ')
        }
        finish <- NULL
        if (!is.null(deployments[[i]]$validTime$finish)) {
            finish <- as.POSIXct(deployments[[i]]$validTime$finish, tz = 'GMT', format='%Y-%m-%dT%H:%M:%OSZ')
        }
        d[[i]] <- list(name = deployments[[i]]$name,
                    validTime = list(start = start, finish = finish),
                    location = deployments[[i]]$`_embedded`$location[[1]]$id)
    }

    res$deployments <- d
    res$usermetadata <- response$usermetadata
    res
}


#' The all platforms in the Senaps
#'
#' @param id Only return platforms with this id or partial match using wildcards (*, ?).
#' @param groups filter response by a comma separated list of group ids
#' @param groupids filter response by a comma separated list of group ids
#'
#' @return A data frame with all streamids
#' @export
#' @examples
#' \dontrun{
#'   get_platforms()
#' }
#'
get_platforms <- function(id = NULL, groupids = NULL, groups = NULL) {
    query <- list()
    if (!is.null(id)) {
        query$id <- id
    }
    if (!is.null(groups)) {
        query$groupids <- groups
        # so the interface is consistent with python and REST API
        warning("groups argument will be deprecated and will be overridden by groupids if defined. Use groupids parameter.")
    }
    if (!is.null(groupids)) {
        query$groupids <- groupids
    }
    response <- request(GET, 'platforms', query = query)
    httr::stop_for_status(response)
    response <- httr::content(response)
    ids <- map_chr(response$`_embedded`$platforms, 'id')
    return(ids)
}

#' Delete an existing platform
#'
#' @param id The platfrom id
#' @param cascade Logical. Should the deletion take place even if the platform is referenced by other objects.
#'
#' @return The status code of request.
#' \itemize{
#'   \item 200 OK
#'   \item 400 The Platform is referenced by other objects. Either use the 'cascade' parameter or manually remove references.
#'   \item 401 Unauthorised to delete this Platform
#'   \item 404 Not found
#' }
#' @export
#' @examples
#' \dontrun{
#'   delete_platform('rsenaps.platform.test1')
#' }
#'
delete_platform <- function(id, cascade = FALSE) {
    query <- list()
    if (cascade) {
        query$cascade <- TRUE
    }
    response <- request(DELETE, path = paste0('platforms/', id), query = query)
    status <- status_code(response)
    status
}

#' Get the current deployment for a platform
#'
#' @param platform_detail A list of platform details. Output from `get_platform`
#'
#' @return A list with deployment details
#' @export
#'
#' @examples
#' \dontrun{
#'   current_deployment('rsenaps.platform.test1')
#' }
#'

current_deployment <- function(platform_detail) {
    #  browser()
    d <- platform_detail$deployments

    if(length(d) == 0) return(NULL)

    finish <- lapply(d, function(d) {
        return(d$validTime$finish)
    })

    lfinish <- as.logical(lapply(finish, is.null))

    if(any(lfinish[length(lfinish) - 1])) stop("More than the last deployment has a NULL finish time")

    start <- lapply(d, function(d) {
        return(d$validTime$start)
    })

    lstart <- as.logical(lapply(start, is.null))

    if(length(lfinish) != length(start)) stop("check deployments, there's something wrong")

    if(as.POSIXct(start[[1]], format='%Y-%m-%dT%H:%M:%OSZ') < Sys.time()) return(d[[length(d)]])

}
