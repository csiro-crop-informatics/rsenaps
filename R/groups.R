# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   03:40 PM Saturday, 09 June 2018
# * Copyright: AS IS



#' The all groups in the Senaps
#'
#' @return A data frame with all groups
#' @export
get_groups <- function(id = NULL,
                       organisation = NULL,
                       groups = NULL,
                       expand = FALSE,
                       usermetadatafield = NULL,
                       usermetadatavalues = NULL) {
    query <- list(expand = expand)
    if (!is.null(id)) {
        query$id = paste0("*", id, "*")
    }
    if (!is.null(organisation)) {
        if (length(organisation) > 1) {
            stop("Only support for one organisation.")
        }
        query$organisationid = organisation
    }
    if (!is.null(groups)) {
        query$groupids <- paste(groups, collapse = ',')
    }
    if (!is.null(usermetadatavalues) & !is.null(usermetadatafield)) {
        query$usermetadatafield <- usermetadatafield
        query$usermetadatavalues <- paste(usermetadatavalues, collapse = ',')
    }

    response <- request(GET, 'groups', query = query)
    httr::stop_for_status(response)
    response <- httr::content(response)
    groups <- response$`_embedded`$groups
    res <- NULL
    if (expand) {
        res <- list()
        for (i in seq_len(response$count)) {
            group_i <- groups[[i]]
            group_i$`_links` <- NULL
            group_i$organisation <- map_chr(group_i$`_embedded`$organisation, "id")
            group_i$groups <- map_chr(group_i$`_embedded`$groups, 'id')
            group_i$`_embedded` <- NULL
            res[[i]] <- group_i
        }
    } else {
        res <- map_chr(groups, 'id')
    }
    res

}


#' The group detail information in the Senaps
#'
#' @param id The group id
#' @param recursive Return full details of embedded resources
#' @return list of group meta data. NULL if group doesn't exist
#' @export
get_group <- function(id, recursive = FALSE) {
    response <- request(GET, paste0('groups/', id), recursive = TRUE)
    status <- httr::status_code(response)
    if (!(status %in% c(200))) {
        return(NULL)
    }
    response <- httr::content(response)
    res <- list()
    res$id <- response$id
    res$name <- response$name
    res$description <- response$description
    res$organisation <- map_chr(response$`_embedded`$organisation, 'id')
    res$groups <- map_chr(response$`_embedded`$groups, 'id')
    res$usermetadata <- response$usermetadata
    res
}

#' Create a new group into Senaps
#'
#' @param id The new group unique id
#' @param name The name of group
#' @param description The description of group
#' @param organisation The organisation ids
#' @param groups The parent groups
#' @param usermetadata A list of usermeta information
#'
#' @return TRUE if new group is added
#' @export
put_group <- function(id, name, description, organisation, groups = NULL,
                      usermetadata = NULL) {
    if (class(id) != 'character' | length(id) > 1) {
        stop('Stream id should be character element')
    }

    if (is.null(usermetadata)) {
        usermetadata <- jsonlite::unbox('')

    }
    body <- list(id = jsonlite::unbox(id),
                 name = jsonlite::unbox(name),
                 organisationid = jsonlite::unbox(organisation),
                 description = jsonlite::unbox(description),
                 groupids = groups,
                 usermetadata = usermetadata)
    response <- request(POST, path = paste0('groups/', id),
                        body = jsonlite::toJSON(body, auto_unbox = FALSE,
                                                null = 'null'),
                        encode = 'json')
    status <- httr::status_code(response)

    if (!(status %in% c(200, 201))) {
        stop(httr::http_status(response)$message)
    }
    TRUE
}

#' Delete a group
#'
#' @param id Group ID
#' @param cascade Remove group even when not empty (default FALSE)
#'
#' @return The staus code of delete
#' @export
delete_group <- function(id, cascade = FALSE) {
    response <- request(DELETE, path = paste0('groups/', id),
                        query = list(cascade = cascade))
    status <- status_code(response)
    status
}




#' Delete a group and all children with all streams, locations and platforms associated. Use it with your own risk.
#'
#' @param id Group id
#'
#' @export
clean_group <- function(id, ask = TRUE) {
    if (ask) {
        n <- readline(prompt="Do you really want to delete this group and all childrens \n with all associated streams, locations and platform. \nType 'yes' to confirm.\n")
        if (n != 'yes') {
            warning('Return without deleting anything.')
            return()
        }
    }

    ginfo <- get_group(id)
    if (is.null(ginfo)) {
        stop('Group is not found.')
    }

    recursive_clean_group <- function(id) {
        # Delete platforms
        platforms <- get_platforms(groups = id)
        for (i in seq(along = platforms)) {
            p <- get_platform(platforms[i])
            put_platform(platforms[i], organisation = p$organisation,
                         groups = p$groups)
            delete_platform(platforms[i])
        }

        # Delete streams
        stream_count <- count_streams(groups = id)
        streams <- get_streams(groups = id, limit = stream_count)
        for (i in seq(along = streams)) {
            delete_observations(streams[i])
            delete_stream(streams[i])
        }



        # Delete locations
        locations <- get_locations(groups = id)
        for (i in seq(along = locations)) {
            delete_location(locations[i])
        }

        # Delete child groups
        child_groups <- rev(get_groups(groups = id))
        for (i in seq(along = child_groups)) {
            recursive_clean_group(child_groups[i])
        }

        # Delete group
        delete_group(id)
    }
    recursive_clean_group(id)
}
