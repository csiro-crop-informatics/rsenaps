


#' The all jobs for a workflow in the Senaps
#'
#' @param workflowid Workflow id
#' @param limit Number of jobs retrieved
#'
#' @return A list of jobs
#' @export
get_jobs <- function(workflowid = NULL, limit = NULL) {

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

    response <- request(GET, 'jobs', query = query)
    httr::stop_for_status(response)
    response <- httr::content(response)

    jobs <- response$`_embedded`$jobs
    jobs
}

