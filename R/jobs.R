


#' The all jobs for a workflow in the Senaps
#'
#' @param workflowid Workflow id
#' @param limit Number of jobs retrieved
#' @param skip Skip the number of jobs for pagination
#'
#' @return A list of jobs
#' @export
get_jobs <- function(workflowid = NULL, limit = NULL, skip = NULL) {

    query <- list()
    if (!is.null(workflowid)) {
        if (length(workflowid) > 1) {
            stop('Only a single workflow can be searched for.')
        }
        query$workflowid <- workflowid
    }
    if (!is.null(limit)) {
        if (length(limit) > 1) {
            stop('Only a single value for limit.')
        }
        if (!is.numeric(limit)) {
            stop("Numeric value is required.")
        }
        query$limit <- limit
    }


    if (!is.null(skip)) {
        if (length(skip) > 1) {
            stop('Only a single value for skip')
        }
        if (!is.numeric(skip)) {
            stop("Numeric value is required.")
        }
        query$skip <- skip
    }

    response <- request(GET, 'jobs', query = query)
    httr::stop_for_status(response)
    response <- httr::content(response)
    response
}


#' Get logs for a jog
#'
#' @param jobid The job id
#' @param limit Number of logs retrieved
#'
#' @return  A list for job
#' @export
get_job_log <- function(jobid, limit = NULL) {
    if (!is.null(jobid)) {
        if (length(jobid) > 1) {
            stop('Only a single value for jobid')
        }
        if (!is.character(jobid)) {
            stop("character value is required.")
        }
    }

    url <- paste0('jobs', "/", jobid, "/log")
    query <- list()
    if (!is.null(limit)) {
        if (length(limit) > 1) {
            stop('Only a single value for limit.')
        }
        if (!is.numeric(limit)) {
            stop("Numeric value is required.")
        }
        query$limit <- limit
    }

    response <- request(httr::GET, url, query = query)
    .stop_for_status(response)
    response <- httr::content(response)
    response
}
