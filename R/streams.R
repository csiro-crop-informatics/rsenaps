

#' Count streams in the Senaps
#'
#' @param groups A vector of strings of group ids
#'
#' @return Number of streams
#' @export
count_streams <- function(groups = NULL) {
    query <- list()
    if (!is.null(groups)) {
        query <- list(groupids = groups)
    }
    response <- request(GET, 'streams/count', query = query)
    httr::stop_for_status(response)
    contents <- httr::content(response)
    contents$count
}


#' The all streams in the Senaps
#'
#' @param groups A group id (optional)
#' @param observed_property An observed property uri
#' @param limit Maximum records to retrieve
#'
#' @return A data frame with all streamids
#' @export

get_streams <- function(groups = NULL,
                        observed_property = NULL,
                        limit = 1000) {
    query <- list(limit = limit)

    if (!is.null(groups)) {
        query$groupids <- groups
    }

    if (!is.null(observed_property)) {
        query$streamMetadata.observedProperty = observed_property
    }

    response <- request(GET, 'streams', query = query)
    httr::stop_for_status(response)
    contents <- httr::content(response)
    streams <- contents$`_embedded`$streams
    map_chr(streams, 'id')
}


#' The stream detail information in the Senaps
#'
#' @param id The stream id
#'
#' @return A list of steam meta information
#' @export
get_stream <- function(id) {
    response <- request(httr::GET, paste0('streams/', id))

    .stop_for_status(response)
    contents <- httr::content(response)
    res <- list()

    res$id <- contents$id
    res$resulttype <- contents$resulttype
    res$organisationid <- map_chr(contents$`_embedded`$organisation, 'id')
    res$groupids <- map_chr(contents$`_embedded`$groups, 'id')
    if (!is.null(contents$`_embedded`$location)) {
        res$locationid <- map_chr(contents$`_embedded`$location, 'id')
    }
    res$reportingPeriod <- contents$reportingPeriod
    res$samplePeriod <- contents$samplePeriod
    res$usermetadata <- contents$usermetadata
    if (!is.null(contents$`_embedded`$metadata)) {
        metadata <- contents$`_embedded`$metadata[[1]]
        streamMetadata <- list()
        streamMetadata$type <- metadata$type
        streamMetadata$observedProperty <-  metadata$`_embedded`$observedProperty[[1]]$`_links`$self$href
        streamMetadata$unitOfMeasure <-  metadata$`_embedded`$unitOfMeasure[[1]]$`_links`$self$href
        streamMetadata$interpolationType <-  metadata$`_embedded`$interpolationType[[1]]$`_links`$self$href
        res$streamMetadata <- streamMetadata
    }
    res
}


#' Create a new stream in Senaps
#'
#' @param id The id of new stream. Have to be unique
#' @param reportingPeriod Reporting frequency. TODO: Add documentation about valid format
#' @param samplePeriod Sampling frequency. TODO: Add documentation about valid format
#' @param observedProperty Observe property. TODO: Add documentation about valid values
#' @param unitOfMeasure Observe unit. TODO: Add documentation about valid values
#' @param location The location of new location. Have to be an existing location
#' @param groups The list of groups. Have to be an existing group (optional)
#' @param resulttype Types of reuls. scalarvalue in default
#' @param interpolationType Interpolation type. Continuous in default
#' @param organisation An organisation id
#' @param usermetadata User metadata values
#'
#' @return A list of new stream if the new stream is successfully created
#' @export
put_stream <- function(id, organisation,
                       reportingPeriod,
                       samplePeriod,
                       observedProperty,
                       unitOfMeasure,
                       location = NULL,
                       groups = character(),
                       resulttype = 'scalarvalue',
                       interpolationType = 'Continuous',
                       usermetadata = NULL) {
    # TODO: Check the observedProperty and unitOfMeasure
    if (!grepl('^http', observedProperty)) {
        observedProperty <- paste0('http://registry.it.csiro.au/def/environment/property/', observedProperty)
    }

    if (!grepl('^http', unitOfMeasure)) {
        unitOfMeasure <- paste0('http://registry.it.csiro.au/def/environment/property/', unitOfMeasure)
    }

    if (!grepl('^http', interpolationType)) {
        interpolationType <- paste0("http://www.opengis.net/def/waterml/2.0/interpolationType/", interpolationType)
    }

    if (is.null(usermetadata)) {
        usermetadata <- NULL
    }
    if (is.null(location) | length(location) == 0) {
        location <- NULL
    }
    body <- list(id = jsonlite::unbox(id),
                 resulttype = jsonlite::unbox(resulttype),
                 organisationid = jsonlite::unbox(organisation),
                 groupids = groups,
                 locationid = location,
                 samplePeriod = jsonlite::unbox(samplePeriod),
                 reportingPeriod = jsonlite::unbox(reportingPeriod),
                 streamMetadata = list(
                     type = jsonlite::unbox('.ScalarStreamMetaData'),
                     observedProperty = jsonlite::unbox(observedProperty),
                     unitOfMeasure = jsonlite::unbox(unitOfMeasure),
                     interpolationType = jsonlite::unbox(interpolationType)),
                 usermetadata = usermetadata)


    response <- request(httr::POST, path = paste0('streams/', id),
                        body = jsonlite::toJSON(body, auto_unbox = FALSE,
                                                null = 'null'),
                        encode = 'json')
    .stop_for_status(response)
    return(TRUE)
}



#' Delete an existing stream
#'
#' @param id The stream id
#' @return The status code of request
#' @export
delete_stream <- function(id) {
    response <- request(httr::DELETE, path = paste0('streams/', id))
    .stop_for_status(response)
    return(TRUE)
}

