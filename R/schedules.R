
# functions related with schedule

#' Add a new schedule
#'
#' @param name The schedule name
#' @param workflow A workflow id
#' @param cron A cron expression
#' @param timezone Timezone
#' @param description Description of the schedule
#' @param active Whether the schedule is active or not
#'
#' @return The schedule id
#' @export
put_schedule <- function(name, workflow, cron = '0 */1 * * *',
                         timezone = "UTC", description = name, active = TRUE) {
    body <- list(workflowid = jsonlite::unbox(workflow),
                 active = jsonlite::unbox(active),
                 name = jsonlite::unbox(name),
                 timezone = jsonlite::unbox(timezone),
                 description = jsonlite::unbox(description),
                 cron = jsonlite::unbox(cron))

    response <- request(POST, path = 'schedules',
                        body = jsonlite::toJSON(body, auto_unbox = TRUE,
                                                null = 'null'),
                        config = content_type('application/json'))
    status <- httr::status_code(response)

    if (!(status %in% c(200, 201))) {
        stop(httr::http_status(response)$message)
    }
    r <- content(response)
    return(r$id)
}

#' Get schedules in Senaps
#'
#' @param workflow The workflow id
#'
#' @return A character vector of role ids
#' @export
get_schedules <- function(workflow = NULL) {
    query <- list()
    if (!is.null(workflow)) {
        query$workflowid <- workflow
    }
    response <- request(GET, 'schedules', query = query)
    httr::stop_for_status(response)
    response <- httr::content(response)
    ids <- map_chr(response$`_embedded`$schedules, 'id')
    return(ids)
}


#' The detail inforamtion of schedule in the Senaps
#'
#' @param id schedule id
#' @return A list of schedule meta information
#' @export
get_schedule <- function(id) {
    response <- request(GET, paste0('schedules/', id))
    httr::stop_for_status(response)
    response <- httr::content(response)
    response
}



#' Delete an existing schedule
#'
#' @param id The schedule id
#' @return The status code of request.
#' \itemize{
#'   \item 200 OK
#'   \item 401 Unauthorised to delete this Platform
#'   \item 404 Not found
#' }
#' @export
delete_schedule <- function(id) {
    response <- request(DELETE, path = paste0('schedules/', id))
    status <- status_code(response)
    status
}
