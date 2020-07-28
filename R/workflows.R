


#' The all workflows in the Senaps
#'
#' @param name A workflow name. Can accept * and ? wildcards.
#' @param groups A list of group ids to filter the search
#'
#' @return A list of workflows
#' @export
get_workflows <- function(name = NULL, groups = NULL) {

    query <- list()
    if (!is.null(name)) {
        if (length(name) > 1) {
            stop('Only a single workflow can be searched for.')
        }
        query$name <- paste0('*', name, "*")
    }
    response <- request(GET, 'workflows', query = query)
    httr::stop_for_status(response)
    response <- httr::content(response)

    workflows <- response$`_embedded`$workflows
    ids <- map_chr(workflows, 'id')
    res <- ids
    if (!is.null(groups)) {
        res <- NULL
        all_groups <- map(workflows, 'groupids')
        for (i in seq(along = ids)) {
            if (groups %in% as.character(all_groups[[i]])) {
                res <- c(res, ids[i])
            }
        }
    }
    res
}



#' The workflow detail information in the Senaps
#'
#' @param id A workflow id
#'
#' @return A list of workflow meta data
#' @export
get_workflow <- function(id) {
    response <- request(GET, paste0('workflows/', id))
    httr::stop_for_status(response)
    response <- httr::content(response)
    response
}

#' Add a new workflow into Senaps
#'
#' @param name Name
#' @param description Description
#' @param organisation Organisation
#' @param groups Groups
#' @param graph Graph
#'
#' @export
add_workflow <- function(name, description, organisation, groups, graph) {

    body <- list(name = jsonlite::unbox(name),
                 description = jsonlite::unbox(description),
                 organisationid = jsonlite::unbox(organisation),
                 groupids = I(groups),
                 graph = graph)


    response <- request(POST, path = 'workflows',
                        body = jsonlite::toJSON(body, auto_unbox = TRUE,
                                                null = 'null'),
                        config = httr::content_type('application/json'))
    status <- httr::status_code(response)

    if (!(status %in% c(200, 201))) {
        stop(httr::http_status(response)$message)
    }
    return(httr::content(response)$id)
}

#' Delete an existing workflow
#'
#' @param id The platfrom id
#' @return The status code of request.
#' \itemize{
#'   \item 200 OK
#'   \item 401 Unauthorised to delete this Platform
#'   \item 404 Not found
#' }
#' @export
delete_workflow <- function(id) {
    schedule <- get_schedules(workflow = id)
    for (j in seq(along = schedule)) {
        delete_schedule(schedule[j])
    }
    response <- request(DELETE, path = paste0('workflows/', id))
    status <- status_code(response)
    status
}
